suppressMessages({ library(dplyr); library(stringr) })

normalize_city <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("\\bst[.]?\\s", "saint ") |>
    str_replace_all("[-]", " ") |>
    str_replace_all("[^a-z ]", "") |>
    str_squish() |>
    # collapse spaces entirely so "La Grange"/"LaGrange", "De Kalb"/"DeKalb" collide
    str_remove_all(" ")
}

strip_lsad <- function(name_raw) {
  name_raw |>
    str_remove("\\s*\\(balance\\)\\s*$") |>
    str_remove(paste0(
      "\\s(city and borough|metropolitan government|metro government|",
      "unified government|consolidated government|urban county|",
      "city|town|village|borough|municipality|comunidad|zona urbana|CDP)$"))
}

# Player-side aliases: birthplace strings that are not Census place names.
# Boroughs/neighborhoods belong to a larger incorporated city; a few are
# renames or source data quirks (see comments). Keys are normalize_city output.
city_aliases <- tibble::tribble(
  ~city_norm,      ~birth_state, ~alias_norm,
  "brooklyn",      "NY", "newyork",
  "queens",        "NY", "newyork",
  "bronx",         "NY", "newyork",
  "statenisland",  "NY", "newyork",
  "manhattan",     "NY", "newyork",
  "harlem",        "NY", "newyork",
  "honolulu",      "HI", "urbanhonolulu",   # Census place is "Urban Honolulu CDP"
  "northridge",    "CA", "losangeles",      # LA city neighborhoods
  "sanpedro",      "CA", "losangeles",
  "vannuys",       "CA", "losangeles",
  "woodlandhills", "CA", "losangeles",
  "pacoima",       "CA", "losangeles",
  "sylmar",        "CA", "losangeles",
  "canogapark",    "CA", "losangeles",
  "encino",        "CA", "losangeles",
  "granadahills",  "CA", "losangeles",
  "harborcity",    "CA", "losangeles",
  "ventura",       "CA", "sanbuenaventuraventura",  # "San Buenaventura (Ventura) city"
  "boise",         "ID", "boisecity",               # official name "Boise City"
  "valencia",      "CA", "santaclarita",
  "amite",         "LA", "amitecity",       # place name is "Amite City"
  "lakeworth",     "FL", "lakeworthbeach",  # renamed 2019
  "greenview",     "SC", "greenville",      # ESPN data quirk (no Greenview place)
  "greenview",     "NC", "greenville"
)

match_places <- function(players_df, places_df) {
  lookup <- places_df |>
    mutate(city_norm = normalize_city(strip_lsad(name_raw)),
           incorporated = lsad != "57",
           best_pop = coalesce(pop_now, pop2010, pop2000, 0L))

  # Consolidated city-county governments carry compound official names
  # ("Nashville-Davidson metropolitan government (balance)") but players are
  # born in "Nashville". Index those rows under their leading component too.
  consolidated <- places_df |>
    filter(str_detect(name_raw, "\\(balance\\)|government|urban county|County$")) |>
    mutate(prefix = str_extract(strip_lsad(name_raw), "^[^-/]+"),
           city_norm = normalize_city(prefix),
           incorporated = TRUE,
           best_pop = coalesce(pop_now, pop2010, pop2000, 0L)) |>
    select(-prefix) |>
    filter(!is.na(city_norm), city_norm != "") |>
    anti_join(lookup, by = c("city_norm", "state"))

  lookup <- bind_rows(lookup, consolidated)

  candidates <- players_df |>
    mutate(.row = row_number(),
           city_norm = normalize_city(birth_city)) |>
    left_join(city_aliases, by = c("city_norm", "birth_state")) |>
    mutate(city_norm = coalesce(alias_norm, city_norm)) |>
    select(-alias_norm) |>
    left_join(lookup, by = c("city_norm", "birth_state" = "state"),
              relationship = "many-to-many")

  picked <- candidates |>
    group_by(.row) |>
    mutate(n_cand = sum(!is.na(geoid))) |>
    arrange(desc(incorporated), desc(best_pop), .by_group = TRUE) |>
    slice(1) |>
    ungroup() |>
    mutate(match_tier = case_when(
      is.na(geoid)                 ~ "unmatched",
      n_cand == 1                  ~ "unique",
      incorporated & n_cand > 1    ~ "lsad_pref",
      TRUE                         ~ "biggest_pop"))

  players_df |>
    mutate(.row = row_number()) |>
    left_join(picked |>
                select(.row, geoid, matched_pop2000 = pop2000,
                       matched_pop2010 = pop2010, matched_pop_now = pop_now,
                       matched_income1999 = income1999,
                       matched_income_now = income_now,
                       aland_sqmi, lat, lon, match_tier),
              by = ".row") |>
    select(-.row)
}
