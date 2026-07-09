suppressMessages({ library(dplyr); library(stringr) })

normalize_city <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("\\bst[.]?\\s", "saint ") |>
    str_replace_all("[-]", " ") |>
    str_replace_all("[^a-z ]", "") |>
    str_squish()
}

strip_lsad <- function(name_raw) {
  name_raw |>
    str_remove("\\s*\\(balance\\)\\s*$") |>
    str_remove(paste0(
      "\\s(city and borough|metropolitan government|metro government|",
      "unified government|consolidated government|urban county|",
      "city|town|village|borough|municipality|comunidad|zona urbana|CDP)$"))
}

match_places <- function(players_df, places_df) {
  lookup <- places_df |>
    mutate(city_norm = normalize_city(strip_lsad(name_raw)),
           incorporated = lsad != "57",
           best_pop = coalesce(pop_now, pop2010, pop2000, 0L))

  candidates <- players_df |>
    mutate(.row = row_number(),
           city_norm = normalize_city(birth_city)) |>
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
