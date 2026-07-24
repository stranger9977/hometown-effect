# =============================================================================
# 23_pro_entry_age.R -- age at pro entry over time, capped vs uncapped sports.
#
# Thesis: sports with no draft-eligibility age floor (MLB debut whenever a team
# calls you up, NBA once one-and-done/NIL loosened the incentive to stay in
# school) show entry age drifting upward in the modern era, while the NFL's age
# floor (three college seasons, functionally an age cap) keeps rookie age flat.
#
# Sources, all cached, no fetch:
#   NFL: data/processed/spine.parquet (birth_date + rookie_season)
#   MLB: data/raw/People.RData (Lahman; debut + birthDate)
#   NBA: data/raw/bbref/*.html (51 state birthplace index pages), re-parsed
#        here with the same field mapping R/13_pull_nba.R uses when it builds
#        players_nba.parquet: table#stats rows, data-stat player/season_min/
#        birth_date (csk attribute is the sortable YYYY-MM-DD).
# =============================================================================

suppressMessages({
  library(dplyr); library(arrow); library(ggplot2); library(tidyr)
  library(zoo); library(rvest); library(tibble)
})
source("R/lib/theme_hometown.R")

dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)

YR_LO <- 1990
YR_HI <- 2025

# --------------------------------------------------------------------------
# 1. NFL: rookie age from spine.parquet
# --------------------------------------------------------------------------
spine <- read_parquet("data/processed/spine.parquet")

nfl_players <- spine |>
  filter(!is.na(birth_date), !is.na(rookie_season),
         rookie_season >= YR_LO, rookie_season <= YR_HI) |>
  mutate(entry_age = rookie_season - as.integer(format(birth_date, "%Y")))

nfl_yearly <- nfl_players |>
  group_by(year = rookie_season) |>
  summarise(mean_age = mean(entry_age), n = n(), .groups = "drop") |>
  mutate(sport = "NFL")

cat("=== NFL rookie age, n per year (min/max) ===\n")
cat(sprintf("years covered: %d-%d, n range %d-%d\n",
            min(nfl_yearly$year), max(nfl_yearly$year),
            min(nfl_yearly$n), max(nfl_yearly$n)))

# --------------------------------------------------------------------------
# 2. MLB: debut age from Lahman People.RData
# --------------------------------------------------------------------------
load("data/raw/People.RData")  # -> People

mlb_players <- People |>
  as_tibble() |>
  filter(!is.na(debut), !is.na(birthDate)) |>
  mutate(debut_date = as.Date(debut),
         debut_year = as.integer(format(debut_date, "%Y")),
         debut_age = debut_year - as.integer(format(birthDate, "%Y"))) |>
  filter(debut_year >= YR_LO, debut_year <= YR_HI)

mlb_yearly <- mlb_players |>
  group_by(year = debut_year) |>
  summarise(mean_age = mean(debut_age), n = n(), .groups = "drop") |>
  mutate(sport = "MLB")

cat("=== MLB debut age, n per year (min/max) ===\n")
cat(sprintf("years covered: %d-%d, n range %d-%d\n",
            min(mlb_yearly$year), max(mlb_yearly$year),
            min(mlb_yearly$n), max(mlb_yearly$n)))

# --------------------------------------------------------------------------
# 3. NBA: re-parse data/raw/bbref/*.html (per-state birthplace pages).
#    Field mapping matches R/13_pull_nba.R's parse_state(): table#stats rows,
#    data-stat player / season_min ("From", season-end year) / birth_date
#    (csk attribute is the machine-readable YYYY-MM-DD sort key).
# --------------------------------------------------------------------------
bbref_dir <- "data/raw/bbref"
state_files <- list.files(bbref_dir, pattern = "\\.html$", full.names = TRUE)
cat(sprintf("=== NBA: found %d cached state pages ===\n", length(state_files)))

parse_nba_state <- function(path) {
  page <- read_html(path)
  rows <- html_elements(page, "table#stats tbody tr:not(.thead)")
  if (length(rows) == 0) {
    return(tibble(from_year = integer(), birth_date = character()))
  }
  from  <- html_element(rows, "td[data-stat='season_min']") |> html_text2()
  bdate <- html_element(rows, "td[data-stat='birth_date']") |> html_attr("csk")
  bdate <- ifelse(!is.na(bdate) & grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", bdate),
                  bdate, NA_character_)
  tibble(from_year = suppressWarnings(as.integer(from)), birth_date = bdate)
}

nba_raw <- bind_rows(lapply(state_files, parse_nba_state))
cat(sprintf("NBA rows parsed: %d (with usable birth_date: %d)\n",
            nrow(nba_raw), sum(!is.na(nba_raw$birth_date))))

nba_players <- nba_raw |>
  filter(!is.na(from_year), !is.na(birth_date)) |>
  mutate(birth_date = as.Date(birth_date),
         entry_age = from_year - as.integer(format(birth_date, "%Y"))) |>
  filter(from_year >= YR_LO, from_year <= YR_HI)

nba_yearly <- nba_players |>
  group_by(year = from_year) |>
  summarise(mean_age = mean(entry_age), n = n(), .groups = "drop") |>
  mutate(sport = "NBA")

cat("=== NBA entry age, n per year (min/max) ===\n")
cat(sprintf("years covered: %d-%d, n range %d-%d\n",
            min(nba_yearly$year), max(nba_yearly$year),
            min(nba_yearly$n), max(nba_yearly$n)))

nba_parse_ok <- nrow(nba_yearly) > 0 &&
  length(setdiff(YR_LO:YR_HI, nba_yearly$year)) == 0

if (!nba_parse_ok) {
  cat("*** NBA PARSE INCOMPLETE: missing years or zero rows. ",
      "Delivering NFL + MLB only for ba_entry_age.png. ***\n", sep = "")
}

# --------------------------------------------------------------------------
# 4. Combine, fill any gap years (NA), 3-year centered rolling mean (partial
#    windows at the edges so no data is dropped at 1990/2025). This is a light
#    smoother chosen because per-year Ns are small for MLB debuts and NBA
#    entrants (dozens, not thousands) and the raw means are visibly saw-toothed.
# --------------------------------------------------------------------------
all_years <- tibble(year = YR_LO:YR_HI)

build_series <- function(yearly, sport_name) {
  yearly |>
    right_join(all_years, by = "year") |>
    mutate(sport = sport_name) |>
    arrange(year) |>
    mutate(smoothed_age = rollapply(mean_age, width = 3, FUN = function(x) mean(x, na.rm = TRUE),
                                     partial = TRUE, align = "center", fill = NA))
}

series_list <- list(build_series(nfl_yearly, "NFL"), build_series(mlb_yearly, "MLB"))
if (nba_parse_ok) series_list <- c(series_list, list(build_series(nba_yearly, "NBA")))

combined <- bind_rows(series_list) |>
  mutate(sport = factor(sport, levels = c("NFL", "MLB", "NHL", "NBA")))

cat("\n=== smoothed mean entry age, selected years ===\n")
print(combined |> filter(year %in% c(1990, 2000, 2006, 2010, 2015, 2019, 2021, 2025)) |>
        select(sport, year, mean_age, smoothed_age) |> arrange(sport, year), n = 100)

# --------------------------------------------------------------------------
# 5. Plot: direct-labeled lines, no legend.
# --------------------------------------------------------------------------
end_labels <- combined |>
  filter(!is.na(smoothed_age)) |>
  group_by(sport) |>
  filter(year == max(year)) |>
  ungroup() |>
  mutate(label = sprintf("%s %.1f", sport, smoothed_age))

nba_note <- if (nba_parse_ok) "" else " NBA omitted: bbref state-page re-parse did not yield complete 1990-2025 coverage."

p1 <- ggplot(combined, aes(year, smoothed_age, colour = sport, group = sport)) +
  geom_baseline(23) +
  geom_line(linewidth = 1, na.rm = TRUE) +
  scale_color_manual(values = pal_sport) +
  scale_x_continuous(breaks = seq(1990, 2025, 5), expand = expansion(mult = c(0.01, 0.09))) +
  scale_y_continuous(limits = c(19, NA)) +
  direct_label(end_labels, aes(x = year, y = smoothed_age, label = label, colour = sport),
               nudge_x = 0.4, size = 3.4, inherit.aes = FALSE) +
  coord_cartesian(clip = "off") +
  labs(
    title = "MLB debut age keeps climbing; the NFL's college-eligibility rule holds rookies flat near 23",
    subtitle = "Mean age at first pro season/game, 3-year centered rolling average, 1990-2025",
    x = NULL, y = "Mean age at entry (years)",
    caption = paste0(
      "Data: nflverse/ESPN spine (NFL rookie seasons), Lahman People table (MLB debuts), Basketball-Reference state pages (NBA \"From\" season).\n",
      "3-year centered rolling mean of yearly averages; raw yearly counts are small enough (dozens to low hundreds) to be noisy year to year.\n",
      "NBA age dipped through the prep-to-pro and early one-and-done years (mid-1990s to late 2000s) and has ticked back up since about 2021, the NIL/portal era.\n",
      "Dashed line marks age 23 for reference.", nba_note
    )
  ) +
  theme_hometown() +
  theme(plot.margin = margin(10, 60, 8, 10))

save_fig("docs/figures/ba_entry_age.png", p1)

# --------------------------------------------------------------------------
# 6. NFL rookie age distribution, ages 20-28, the flat/capped control chart.
# --------------------------------------------------------------------------
nfl_dist <- nfl_players |>
  filter(entry_age >= 20, entry_age <= 28) |>
  count(entry_age)

cat("\n=== NFL rookie age distribution (20-28) ===\n")
print(nfl_dist)

age20_players <- nfl_players |> filter(entry_age == 20) |>
  select(display_name, birth_date, rookie_season)
cat("\nAge-20 rookies (1990-2025):\n")
print(age20_players)

nfl_col <- unname(pal_sport["NFL"])

p2 <- ggplot(nfl_dist, aes(entry_age, n)) +
  geom_col(fill = nfl_col, width = 0.7) +
  geom_text(aes(label = n), vjust = -0.4, size = 3.2, colour = ink_body) +
  scale_x_continuous(breaks = 20:28) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "NFL rookies debut in a tight window; only four players ever debuted at 20",
    subtitle = "Rookie-season age distribution, players with career starts 1990-2025",
    x = "Age at rookie season", y = "Number of players",
    caption = paste0(
      "Data: nflverse/ESPN spine, birth_date and rookie_season, career starts 1990-2025.\n",
      "The 20-year-old rookies on record: Braelon Allen, Tremaine Edmunds, Amobi Okoye, and Kevin Jefferson.\n",
      "College's three-season eligibility rule acts as a de facto age floor; there is no equivalent tail-thinning rule in MLB or the modern NBA."
    )
  ) +
  theme_hometown()

save_fig("docs/figures/ba_nfl_agedist.png", p2, w = 10, h = 6)

cat("\nDone.\n")
