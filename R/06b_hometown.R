# Hometown table: high-school geography preferred, birthplace fallback.
# Produces:
#   data/processed/hometown.parquet  - one row per spine player with
#     hometown_source ("high_school"|"birthplace"|NA), hometown_city/state/
#     geoid/lat/lon/zcta, unified matched_pop*/matched_income*/aland_sqmi
#     from the place match of the chosen source, era.
#   data/processed/divergence.csv    - n_both, n_same_city, n_diff_city,
#     divergence_rate for players with BOTH geocoded HS and matched birthplace.
#
# Join note: spine$sleeper_id is entirely NA (checked 2026-07-08), so instead
# of the sleeper_id join we use (a) the gsis_id embedded in the Sleeper
# records (present for ~36% of HS rows; needs trimws - some have leading
# spaces), then (b) a name+birth_date join restricted to combinations unique
# on BOTH sides (there are real name+DOB collisions, e.g. two Kevin Whites
# born 1992-06-25).

suppressMessages({ library(dplyr); library(arrow); library(jsonlite); library(purrr) })
source("R/lib/schools.R"); source("R/lib/places.R")
`%||%` <- function(a, b) if (is.null(a)) b else a

spine <- read_parquet("data/processed/spine.parquet")
bp <- read_parquet("data/processed/birthplace_matched.parquet")
stopifnot(!any(duplicated(spine$gsis_id)), !any(duplicated(bp$gsis_id)))

# --- Sleeper HS for NFL players ---------------------------------------------
sl <- fromJSON("data/raw/sleeper_players.json", simplifyVector = FALSE) |>
  keep(is.list) |>
  map_dfr(~ tibble(sleeper_id = as.character(.x$player_id %||% NA),
                   gsis_id = trimws(.x$gsis_id %||% NA_character_),
                   full_name = .x$full_name %||% NA_character_,
                   birth_date = .x$birth_date %||% NA_character_,
                   position = .x$position %||% NA_character_,
                   high_school = .x$high_school %||% NA_character_)) |>
  filter(position != "DEF" | is.na(position), !is.na(high_school))

hs <- bind_cols(sl, parse_sleeper_hs(sl$high_school))

# --- NCES lookup (public + private; files pinned in R/lib/schools.R) --------
if (!file.exists("data/processed/nces_schools.parquet")) build_nces_schools()
nces <- read_parquet("data/processed/nces_schools.parquet")

hs_geo <- bind_cols(hs, match_schools(hs |> select(hs_name, hs_state), nces) |>
                          select(-hs_name, -hs_state))
school_match_rate <- mean(hs_geo$school_match)
cat(sprintf("school match: %d / %d parsed HS names (%.1f%%); with parsed state: %d\n",
            sum(hs_geo$school_match), nrow(hs_geo), 100 * school_match_rate,
            sum(!is.na(hs_geo$hs_state))))

# --- Attach spine gsis_id: sleeper gsis_id first, unique name+DOB fallback --
by_gsis <- hs_geo |>
  filter(!is.na(gsis_id), gsis_id %in% spine$gsis_id)

spine_uniq <- spine |>
  mutate(bd = as.character(birth_date)) |>
  filter(!is.na(bd)) |>
  add_count(display_name, bd, name = ".n_sp") |>
  filter(.n_sp == 1) |>
  select(display_name, bd, spine_gsis = gsis_id)

by_namedob <- hs_geo |>
  filter(is.na(gsis_id) | !(gsis_id %in% spine$gsis_id), !is.na(birth_date)) |>
  add_count(full_name, birth_date, name = ".n_sl") |>
  filter(.n_sl == 1) |>
  select(-.n_sl) |>
  inner_join(spine_uniq, by = c("full_name" = "display_name",
                                "birth_date" = "bd")) |>
  mutate(gsis_id = spine_gsis) |>
  select(-spine_gsis)

hs_join <- bind_rows(by_gsis, by_namedob) |>
  filter(school_match) |>
  arrange(gsis_id) |>
  distinct(gsis_id, .keep_all = TRUE)
cat(sprintf("HS rows geocoded + joined to spine: %d\n", nrow(hs_join)))

# --- HS city -> census place (pop/density/income for the HS hometown) -------
hs_matched <- match_places(
  hs_join |> transmute(gsis_id, birth_city = nces_city, birth_state = nces_state),
  read_parquet("data/processed/census_places.parquet")) |>
  select(-birth_city, -birth_state) |>
  rename_with(~ paste0("hs_", .x), -gsis_id)

# --- Assemble: HS preferred, birthplace fallback -----------------------------
hometown <- spine |>
  left_join(hs_join |> select(gsis_id, nces_city, nces_state, nces_zip,
                              nces_lat, nces_lon), by = "gsis_id") |>
  left_join(hs_matched, by = "gsis_id") |>
  left_join(bp |> select(gsis_id, birth_city, birth_state, geoid, lat, lon,
                         matched_pop2000, matched_pop2010, matched_pop_now,
                         matched_income1999, matched_income_now, aland_sqmi,
                         match_tier), by = "gsis_id") |>
  mutate(
    hometown_source = case_when(
      !is.na(nces_city)                              ~ "high_school",
      !is.na(geoid) & match_tier != "unmatched"      ~ "birthplace",
      TRUE                                           ~ NA_character_),
    hometown_city  = if_else(hometown_source == "high_school", nces_city, birth_city),
    hometown_state = if_else(hometown_source == "high_school", nces_state, birth_state),
    hometown_geoid = if_else(hometown_source == "high_school", hs_geoid, geoid),
    hometown_lat   = if_else(hometown_source == "high_school", nces_lat, lat),
    hometown_lon   = if_else(hometown_source == "high_school", nces_lon, lon),
    hometown_zcta  = if_else(hometown_source == "high_school",
                             substr(nces_zip, 1, 5), NA_character_),
    # unified place-match stats for the chosen source (density = pop/aland)
    matched_pop2000    = if_else(hometown_source == "high_school", hs_matched_pop2000,    matched_pop2000),
    matched_pop2010    = if_else(hometown_source == "high_school", hs_matched_pop2010,    matched_pop2010),
    matched_pop_now    = if_else(hometown_source == "high_school", hs_matched_pop_now,    matched_pop_now),
    matched_income1999 = if_else(hometown_source == "high_school", hs_matched_income1999, matched_income1999),
    matched_income_now = if_else(hometown_source == "high_school", hs_matched_income_now, matched_income_now),
    aland_sqmi         = if_else(hometown_source == "high_school", hs_aland_sqmi,         aland_sqmi),
    hometown_match_tier = if_else(hometown_source == "high_school", hs_match_tier, match_tier)) |>
  select(gsis_id, display_name, birth_date, rookie_season, draft_year,
         position, college_name, era,
         hometown_source, hometown_city, hometown_state, hometown_geoid,
         hometown_lat, hometown_lon, hometown_zcta, hometown_match_tier,
         matched_pop2000, matched_pop2010, matched_pop_now,
         matched_income1999, matched_income_now, aland_sqmi,
         nces_city, nces_state, nces_zip, birth_city, birth_state, match_tier)

stopifnot(nrow(hometown) == nrow(spine))
write_parquet(hometown, "data/processed/hometown.parquet")

# --- Divergence: HS town vs birth city for players with both ----------------
both <- hometown |>
  filter(!is.na(nces_city), !is.na(birth_city), match_tier != "unmatched")
divergence <- tibble(
  n_both = nrow(both),
  n_same_city = sum(normalize_city(both$nces_city) == normalize_city(both$birth_city) &
                    both$nces_state == both$birth_state),
  n_diff_city = n_both - n_same_city,
  divergence_rate = n_diff_city / n_both)
write.csv(divergence, "data/processed/divergence.csv", row.names = FALSE)

print(count(hometown, hometown_source))
print(count(hometown, era, hometown_source) |> tidyr::pivot_wider(
  names_from = hometown_source, values_from = n, values_fill = 0L))
print(divergence)
