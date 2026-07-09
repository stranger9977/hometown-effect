# High-school parsing + NCES EDGE geocode matching.
#
# NCES EDGE geocode source files (verified live 2026-07-08, HTTP 200):
#   https://nces.ed.gov/programs/edge/data/EDGE_GEOCODE_PUBLICSCH_2324.zip
#   https://nces.ed.gov/programs/edge/data/EDGE_GEOCODE_PRIVATESCH_2324.zip
# Downloaded to data/raw/nces/. Contents actually found:
#   PUBLIC zip  -> EDGE_GEOCODE_PUBLICSCH_2324/EDGE_GEOCODE_PUBLICSCH_2324.TXT
#                  (pipe-delimited, NO header row, 102,274 rows; also ships
#                  .xlsx and .sas7bdat with identical columns) — 23 columns:
#                  NCESSCH, LEAID, NAME, OPSTFIPS, STREET, CITY, STATE, ZIP,
#                  STFIP, CNTY, NMCNTY, LOCALE, LAT, LON, CBSA, NMCBSA,
#                  CBSATYPE, CSA, NMCSA, CD, SLDL, SLDU, SCHOOLYEAR
#   PRIVATE zip -> EDGE_GEOCODE_PRIVATESCH_2324.xlsx (flat in zip root,
#                  22,510 rows; no TXT variant) — 21 columns:
#                  PPIN, NAME, STREET, CITY, STATE, ZIP, STFIP, CNTY, NMCNTY,
#                  LOCALE, LAT, LON, CBSA, NMCBSA, CBSATYPE, CSA, NMCSA, CD,
#                  SLDL, SLDU, SCHOOLYEAR
# (2223/2021 PRIVATESCH zips 404; 2324 is the newest year with both files.)
# build_nces_schools() combines both into data/processed/nces_schools.parquet
# with exactly: name (chr), city (chr), state (chr USPS), zip (chr),
# lat (dbl), lon (dbl).

suppressMessages({ library(dplyr); library(stringr) })

parse_sleeper_hs <- function(x) {
  paren <- str_match(x, "^(.*?)\\s*\\(([^)]+)\\)\\s*$")
  name <- coalesce(paren[, 2], x)
  st <- paren[, 3]
  st <- if_else(!is.na(st) & st %in% c(state.abb, "DC"), st, NA_character_)
  tibble::tibble(hs_name = str_squish(name), hs_state = st)
}

normalize_school <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("\\bst[.]?\\s", "saint ") |>
    str_replace_all("[^a-z ]", "") |>
    str_remove_all("\\b(senior|junior|jr|sr)\\b") |>
    str_remove_all("\\b(high school|highschool|high|school|hs|h s|academy|prep|preparatory)\\b") |>
    str_squish()
}

match_schools <- function(hs_df, nces_df) {
  lookup <- nces_df |>
    mutate(school_norm = normalize_school(name)) |>
    filter(school_norm != "")
  hs_df |>
    mutate(.row = dplyr::row_number(), school_norm = normalize_school(hs_name)) |>
    left_join(lookup, by = c("school_norm", "hs_state" = "state"),
              relationship = "many-to-many") |>
    group_by(.row) |>
    mutate(n_cand = sum(!is.na(name))) |>
    slice(1) |>
    ungroup() |>
    mutate(school_match = n_cand == 1,
           nces_city  = ifelse(school_match, city, NA_character_),
           nces_state = ifelse(school_match, hs_state, NA_character_),
           nces_zip   = ifelse(school_match, as.character(zip), NA_character_),
           nces_lat   = ifelse(school_match, lat, NA_real_),
           nces_lon   = ifelse(school_match, lon, NA_real_)) |>
    select(hs_name, hs_state, nces_city, nces_state, nces_zip,
           nces_lat, nces_lon, school_match)
}

# Build data/processed/nces_schools.parquet from the raw EDGE files above.
# Idempotent; called by R/06b_hometown.R when the parquet is missing.
build_nces_schools <- function(raw_dir = "data/raw/nces",
                               out = "data/processed/nces_schools.parquet") {
  pub_txt  <- file.path(raw_dir, "EDGE_GEOCODE_PUBLICSCH_2324.TXT")
  priv_xlsx <- file.path(raw_dir, "EDGE_GEOCODE_PRIVATESCH_2324.xlsx")
  stopifnot(file.exists(pub_txt), file.exists(priv_xlsx))

  pub_cols <- c("NCESSCH", "LEAID", "NAME", "OPSTFIPS", "STREET", "CITY",
                "STATE", "ZIP", "STFIP", "CNTY", "NMCNTY", "LOCALE", "LAT",
                "LON", "CBSA", "NMCBSA", "CBSATYPE", "CSA", "NMCSA", "CD",
                "SLDL", "SLDU", "SCHOOLYEAR")
  pub <- readr::read_delim(pub_txt, delim = "|", col_names = pub_cols,
                           col_types = readr::cols(.default = "c"),
                           show_col_types = FALSE) |>
    transmute(name = NAME, city = CITY, state = STATE, zip = as.character(ZIP),
              lat = as.numeric(LAT), lon = as.numeric(LON))

  priv <- openxlsx::read.xlsx(priv_xlsx, sheet = 1) |>
    transmute(name = NAME, city = CITY, state = STATE, zip = as.character(ZIP),
              lat = as.numeric(LAT), lon = as.numeric(LON))

  schools <- bind_rows(pub, priv)
  arrow::write_parquet(schools, out)
  schools
}
