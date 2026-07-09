suppressMessages({ library(dplyr); library(readr); library(arrow); library(stringr);
                   library(jsonlite); library(purrr) })

KEY <- Sys.getenv("CENSUS_API_KEY")
stopifnot("CENSUS_API_KEY missing from ~/.Renviron" = nzchar(KEY))

dir.create("data/raw/census/api", showWarnings = FALSE, recursive = TRUE)
dl <- function(url, dest) {
  if (!file.exists(dest)) download.file(url, dest, mode = "wb", quiet = TRUE)
  dest
}

# Drop any cached non-JSON responses (e.g. HTML "Invalid Key" pages) so a
# re-run after key activation refetches them.
for (f in list.files("data/raw/census/api", full.names = TRUE)) {
  if (!startsWith(trimws(readChar(f, 64L, useBytes = TRUE)), "[")) file.remove(f)
}

# Once one keyed call comes back as a non-JSON page (key not yet activated),
# skip all further keyed calls instead of hammering the API.
.census_key_dead <- FALSE

# Cached Census API GET → data.frame. NEVER cat() the URL (contains the key).
census_get <- function(base, vars, geo, in_clause, cache_name) {
  dest <- file.path("data/raw/census/api", paste0(cache_name, ".json"))
  if (!file.exists(dest)) {
    if (.census_key_dead) return(NULL)
    enc <- function(x) gsub(" ", "%20", x)   # Census API wants ':' and '*' literal
    url <- sprintf("%s?get=%s&for=%s%s&key=%s", base, vars, enc(geo),
                   ifelse(nzchar(in_clause), paste0("&in=", enc(in_clause)), ""),
                   KEY)
    res <- tryCatch(curl::curl_fetch_memory(url), error = function(e) NULL)
    if (is.null(res) || res$status_code != 200) {
      warning(sprintf("census_get failed for %s (status %s)", cache_name,
                      ifelse(is.null(res), "conn", res$status_code)))
      return(NULL)
    }
    if (!startsWith(trimws(rawToChar(res$content)), "[")) {
      # HTML error page (key not activated) — never cache it
      message("Census API key not yet activated - keyed pulls skipped")
      .census_key_dead <<- TRUE
      return(NULL)
    }
    writeBin(res$content, dest)
    Sys.sleep(0.5)
  }
  m <- fromJSON(dest)
  df <- as.data.frame(m[-1, , drop = FALSE], stringsAsFactors = FALSE)
  names(df) <- m[1, ]
  df
}

# --- Place Gazetteer (names, LSAD, land area, centroid) ---
gz_zip <- dl("https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2023_Gazetteer/2023_Gaz_place_national.zip",
             "data/raw/census/gaz_place_2023.zip")
unzip(gz_zip, exdir = "data/raw/census")
gaz <- read_tsv(list.files("data/raw/census", pattern = "Gaz_place.*[.]txt$",
                           full.names = TRUE),
                col_types = cols(.default = col_character()))
names(gaz) <- trimws(names(gaz))         # last col name is whitespace-padded
gaz <- gaz |>
  transmute(state = USPS, geoid = GEOID, name_raw = NAME, lsad = LSAD,
            aland_sqmi = as.numeric(ALAND_SQMI),
            lat = as.numeric(INTPTLAT), lon = as.numeric(INTPTLONG))
stopifnot(nrow(gaz) > 30000)
state_fips <- sort(unique(substr(gaz$geoid, 1, 2)))

# --- Place populations + income via API (covers CDPs) ---
pull_places <- function(base, vars, tag) {
  map_dfr(state_fips, function(st) {
    df <- census_get(base, vars, "place:*", paste0("state:", st),
                     paste0(tag, "_", st))
    if (is.null(df)) return(NULL)
    df$geoid <- paste0(df$state, df$place)
    df
  })
}

dec2000 <- pull_places("https://api.census.gov/data/2000/dec/sf1", "P001001", "dec2000")
dec2010 <- pull_places("https://api.census.gov/data/2010/dec/sf1", "P001001", "dec2010")
acs2023 <- pull_places("https://api.census.gov/data/2023/acs/acs5",
                       "B01003_001E,B19013_001E", "acs2023")
sf3_2000 <- pull_places("https://api.census.gov/data/2000/dec/sf3", "P053001", "sf3_2000")

keyed_mode <- !is.null(acs2023) && nrow(acs2023) > 0
cat(if (keyed_mode) "MODE: keyed (full CDP+income)\n"
    else "MODE: keyless fallback (incorporated places only, no income)\n")

as_int <- function(x) suppressWarnings(as.integer(x))
# geoid + one keyed value column; empty (never NULL) when the pull failed,
# so joins below work in both modes.
keyed_col <- function(df, var, newname) {
  if (is.null(df) || nrow(df) == 0 || !var %in% names(df)) {
    out <- tibble(geoid = character(0), value = integer(0))
  } else {
    out <- tibble(geoid = df$geoid, value = as_int(df[[var]]))
  }
  names(out)[2] <- newname
  out
}

# --- Keyless popest fallback (incorporated places only; no CDPs, no income).
# Keyed decennial/ACS values win when present; these only fill gaps, so a
# re-run after key activation upgrades the parquet in place.
sub00 <- read_csv(dl("https://www2.census.gov/programs-surveys/popest/datasets/2000-2010/intercensal/cities/sub-est00int.csv",
                     "data/raw/census/sub-est00int.csv"),
                  col_types = cols(.default = col_character()),
                  locale = locale(encoding = "latin1")) |>
  filter(SUMLEV == "162") |>
  transmute(geoid = paste0(str_pad(STATE, 2, pad = "0"), str_pad(PLACE, 5, pad = "0")),
            pop2000_f = as_int(ESTIMATESBASE2000),
            pop2010_f = as_int(CENSUS2010POP)) |>
  distinct(geoid, .keep_all = TRUE)
sub24 <- read_csv(dl("https://www2.census.gov/programs-surveys/popest/datasets/2020-2024/cities/totals/sub-est2024.csv",
                     "data/raw/census/sub-est2024.csv"),
                  col_types = cols(.default = col_character()),
                  locale = locale(encoding = "latin1")) |>
  filter(SUMLEV == "162") |>
  transmute(geoid = paste0(str_pad(STATE, 2, pad = "0"), str_pad(PLACE, 5, pad = "0")),
            pop_now_f = as_int(POPESTIMATE2024)) |>
  distinct(geoid, .keep_all = TRUE)

places <- gaz |>
  left_join(keyed_col(dec2000, "P001001", "pop2000_k"), by = "geoid") |>
  left_join(keyed_col(dec2010, "P001001", "pop2010_k"), by = "geoid") |>
  left_join(keyed_col(acs2023, "B01003_001E", "pop_now_k"), by = "geoid") |>
  left_join(keyed_col(acs2023, "B19013_001E", "income_now"), by = "geoid") |>
  left_join(keyed_col(sf3_2000, "P053001", "income1999"), by = "geoid") |>
  left_join(sub00, by = "geoid") |>
  left_join(sub24, by = "geoid") |>
  mutate(pop2000 = coalesce(pop2000_k, pop2000_f),
         pop2010 = coalesce(pop2010_k, pop2010_f),
         pop_now = coalesce(pop_now_k, pop_now_f),
         # ACS uses negative sentinels (-666666666) for suppressed medians → NA
         income_now = if_else(!is.na(income_now) & income_now < 0, NA_integer_, income_now),
         income1999 = if_else(!is.na(income1999) & income1999 < 0, NA_integer_, income1999)) |>
  select(state, geoid, name_raw, lsad, aland_sqmi, lat, lon,
         pop2000, pop2010, pop_now, income1999, income_now)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
write_parquet(places, "data/processed/census_places.parquet")
cat(sprintf("places: %d | pop2000: %d | pop2010: %d | pop_now: %d | income_now: %d | CDPs w/ pop_now: %d\n",
            nrow(places), sum(!is.na(places$pop2000)), sum(!is.na(places$pop2010)),
            sum(!is.na(places$pop_now)), sum(!is.na(places$income_now)),
            sum(places$lsad == "57" & !is.na(places$pop_now))))

# --- ZCTA income/population (banked for the high-school phase; keyed only) ---
if (keyed_mode) {
  z <- census_get("https://api.census.gov/data/2023/acs/acs5",
                  "B01003_001E,B19013_001E", "zip code tabulation area:*", "", "zcta2023")
  if (!is.null(z)) {
    zcta <- z |> transmute(zcta = `zip code tabulation area`,
                           pop_now = as_int(B01003_001E),
                           income_now = as_int(B19013_001E)) |>
      mutate(income_now = if_else(!is.na(income_now) & income_now < 0, NA_integer_, income_now))
    write_parquet(zcta, "data/processed/census_zcta.parquet")
    cat("zctas:", nrow(zcta), "\n")
  }
}

# --- County populations (keyless popest) ---
co <- read_csv(dl("https://www2.census.gov/programs-surveys/popest/datasets/2020-2024/counties/totals/co-est2024-alldata.csv",
                  "data/raw/census/co-est2024-alldata.csv"),
               col_types = cols(.default = col_character()),
               locale = locale(encoding = "latin1")) |>
  filter(SUMLEV == "050") |>
  transmute(county_fips = paste0(STATE, COUNTY), county_name = CTYNAME,
            state = STNAME, pop2024 = as.integer(POPESTIMATE2024))
stopifnot(nrow(co) > 3000)
write_parquet(co, "data/processed/census_counties.parquet")
cat("counties:", nrow(co), "\n")

# --- County cartographic boundaries (for point-in-polygon + maps) ---
shp_zip <- dl("https://www2.census.gov/geo/tiger/GENZ2023/shp/cb_2023_us_county_500k.zip",
              "data/raw/census/cb_2023_us_county_500k.zip")
unzip(shp_zip, exdir = "data/raw/census/cb_2023_us_county_500k")
cat("county shapefile files:",
    length(list.files("data/raw/census/cb_2023_us_county_500k")), "\n")
