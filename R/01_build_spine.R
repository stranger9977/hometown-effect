suppressMessages({ library(dplyr); library(arrow) })
source("R/lib/bins.R")

raw <- "data/raw/players.parquet"
if (!file.exists(raw)) {
  download.file(
    "https://github.com/nflverse/nflverse-data/releases/download/players/players.parquet",
    raw, mode = "wb")
}

players <- read_parquet(raw)
stopifnot(nrow(players) > 24000)

spine <- players |>
  transmute(
    gsis_id, display_name,
    birth_date = as.Date(birth_date),
    rookie_season = as.integer(rookie_season),
    draft_year, position, college_name,
    espn_id = as.character(espn_id),
    pfr_id,
    sleeper_id = if ("sleeper_id" %in% names(players)) as.character(sleeper_id) else NA_character_,
    era = era_cohort(as.integer(rookie_season))
  )

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
write_parquet(spine, "data/processed/spine.parquet")

cat("rows:", nrow(spine), "\n")
cat("with espn_id:", sum(!is.na(spine$espn_id)), "\n")
print(count(spine, era))
