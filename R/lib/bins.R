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
