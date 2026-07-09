era_cohort <- function(rookie_season) {
  dplyr::case_when(
    is.na(rookie_season)      ~ NA_character_,
    rookie_season < 1990      ~ NA_character_,
    rookie_season < 2000      ~ "1990s",
    rookie_season < 2010      ~ "2000s",
    rookie_season < 2020      ~ "2010s",
    TRUE                      ~ "2020s"
  )
}

cote_bin <- function(pop) {
  cut(as.numeric(pop),
      breaks = c(0, 2500, 10000, 30000, 50000, 100000, 250000, 500000, Inf),
      labels = c("<2.5k", "2.5k–10k", "10k–30k", "30k–50k",
                 "50k–100k", "100k–250k", "250k–500k", "500k+"),
      right = FALSE, ordered_result = TRUE)
}

# Era -> Census population vintage. Rookie/debut era determines which
# decennial/ACS population column approximates "population when this player
# grew up": 1990s/2000s rookies -> pop2000, 2010s -> pop2010, 2020s -> pop_now.
# Expects matched_pop2000/matched_pop2010/matched_pop_now + era columns
# (the shape match_places() produces); returns df with a `pop` column added.
vintage_pop <- function(df) {
  df |> dplyr::mutate(pop = dplyr::case_when(
    era %in% c("1990s", "2000s") ~ matched_pop2000,
    era == "2010s"               ~ matched_pop2010,
    era == "2020s"               ~ matched_pop_now))
}
