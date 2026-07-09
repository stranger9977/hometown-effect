suppressMessages({ library(dplyr); library(arrow) })
source("R/lib/bins.R")

raw <- "data/raw/People.RData"
if (!file.exists(raw)) {
  cat("data/raw/People.RData missing -- downloading Lahman People table...\n")
  dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)
  ok <- tryCatch({
    download.file(
      "https://raw.githubusercontent.com/cdalzell/Lahman/master/data/People.RData",
      raw, mode = "wb")
    TRUE
  }, error = function(e) FALSE)
  if (!ok || !file.exists(raw)) {
    stop("Could not download People.RData. Place the Lahman v14 file at ",
         raw, " and re-run.")
  }
}

env <- new.env()
load(raw, envir = env)
if (!"People" %in% ls(env)) {
  stop("People.RData did not contain a `People` object -- unexpected Lahman format.")
}
people <- get("People", envir = env)

# Players only: non-null MLB debut date (excludes managers/coaches/execs-only
# rows that Lahman's People table also carries).
players <- people |> filter(!is.na(debut), debut != "")

# birth_date: v14 ships a ready-made ISO birthDate column; fall back to
# constructing YYYY-MM-DD from birthYear/Month/Day if a future Lahman drop
# ever omits it.
if ("birthDate" %in% names(players)) {
  # Lahman v14 stores birthDate as an R Date object, not a plain string --
  # format() to the required chr YYYY-MM-DD (as.character() on a Date gives
  # the same result, but format() makes the intent explicit).
  birth_date_vals <- format(players$birthDate, "%Y-%m-%d")
} else {
  complete <- !is.na(players$birthYear) & !is.na(players$birthMonth) & !is.na(players$birthDay)
  birth_date_vals <- ifelse(complete,
                             sprintf("%04d-%02d-%02d", players$birthYear, players$birthMonth, players$birthDay),
                             NA_character_)
}

mlb <- players |>
  mutate(
    sport        = "MLB",
    player_name  = trimws(paste(dplyr::coalesce(nameFirst, ""), nameLast)),
    birth_city   = birthCity,
    # birthState is already the 2-letter code for USA-born players in Lahman
    # v14 (verified: all 18,231 non-NA US birthState values are nchar == 2);
    # non-US rows carry a full province/region name -- passed through as-is.
    birth_state  = birthState,
    birth_country = birthCountry,
    birth_date   = birth_date_vals,
    era          = era_cohort(as.integer(substr(debut, 1, 4)))
  ) |>
  filter(!is.na(birth_city) | !is.na(birth_state)) |>
  select(sport, player_name, birth_city, birth_state, birth_country, birth_date, era)

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
write_parquet(mlb, "data/processed/players_mlb.parquet")

cat("=== MLB ===\n")
cat("total rows:", nrow(mlb), "\n")
cat("US-born rows:", sum(mlb$birth_country == "USA", na.rm = TRUE), "\n")
cat("era distribution:\n")
print(count(mlb, era))
cat("birth_date coverage:", sprintf("%.1f%%", 100 * mean(!is.na(mlb$birth_date))), "\n")
