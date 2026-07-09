# =============================================================================
# 17_rae_calendar.R -- RAE calendar heatmaps (design spec section 2).
# Replaces the faceted-bar RAE figures with calendar-style ratio matrices:
#   docs/figures/rae_cal_all.png   cross-sport, pooled 1990+ eras
#   docs/figures/rae_cal_{nfl,mlb,nhl,nba}.png   per-sport, rows = eras
# Baseline identical to R/07 and R/14: days-in-month adjusted uniform
# (Feb = 28.25). ratio = month share / expected share.
# Universe: NFL all players with birth dates (source has no country field);
# MLB and NBA US-born only (matches the page); NHL all birth countries.
# Reads data/processed/*.parquet only; writes figures only.
# =============================================================================

suppressMessages({
  library(dplyr); library(arrow); library(ggplot2)
  library(lubridate); library(tibble)
})
source("R/lib/theme_hometown.R")

days_in_month <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
expected <- tibble(month = 1:12, expected_share = days_in_month / sum(days_in_month))

era_levels <- c("pre-1990", "1990s", "2000s", "2010s", "2020s")

# --- Load the four player universes -----------------------------------------
nfl <- read_parquet("data/processed/spine.parquet") |>
  filter(!is.na(birth_date)) |>
  transmute(sport = "NFL", birth_date = as.Date(birth_date), era)

mlb_src <- read_parquet("data/processed/players_mlb.parquet") |>
  filter(!is.na(birth_date))
mlb <- mlb_src |>
  filter(birth_country == "USA") |>
  transmute(sport = "MLB", birth_date = as.Date(birth_date), era)

nhl <- read_parquet("data/processed/players_nhl.parquet") |>
  filter(!is.na(birth_date)) |>
  transmute(sport = "NHL", birth_date = as.Date(birth_date), era,
            birth_country)

nba <- read_parquet("data/processed/players_nba.parquet") |>
  filter(!is.na(birth_date), birth_country == "USA") |>
  transmute(sport = "NBA", birth_date = as.Date(birth_date), era)

players <- bind_rows(nfl, mlb, nhl |> select(-birth_country), nba)

# Caption facts, computed from the data actually plotted.
mlb_intl_share  <- mean(mlb_src$birth_country != "USA")
nhl_pooled      <- nhl |> filter(!is.na(era))
nhl_pooled_n    <- nrow(nhl_pooled)
nhl_canada_share<- mean(nhl_pooled$birth_country == "CAN")
nhl_era_n <- nhl |>
  mutate(era2 = coalesce(era, "pre-1990")) |>
  count(era2) |>
  arrange(match(era2, era_levels))

# --- RAE machinery -----------------------------------------------------------
rae_by <- function(df, ...) {
  df |>
    mutate(month = month(birth_date)) |>
    count(..., month) |>
    group_by(...) |>
    mutate(share = n / sum(n)) |>
    ungroup() |>
    left_join(expected, by = "month") |>
    mutate(ratio = share / expected_share)
}

# --- Calendar heatmap builder ------------------------------------------------
# rae_df needs columns: row_lab (factor, first level drawn at BOTTOM), month,
# ratio. Annotates every cell with abs(log2(ratio)) > log2(1.15); white text
# on the most saturated fills, grey20 elsewhere.
max_ratio  <- 1.6
lab_thresh <- log2(1.15)
white_thresh <- 0.75 * log2(max_ratio)

rae_calendar <- function(rae_df, title, subtitle, caption) {
  d <- rae_df |>
    mutate(
      month_lab = factor(month.abb[month], levels = month.abb),
      log2r     = log2(ratio),
      lab       = ifelse(abs(log2r) > lab_thresh, sprintf("%.2f", ratio), NA),
      lab_col   = ifelse(pmin(abs(log2r), log2(max_ratio)) > white_thresh,
                         "white", "grey20")
    )
  ggplot(d, aes(month_lab, row_lab, fill = log2r)) +
    geom_tile(colour = "white", linewidth = 1.2) +
    geom_text(data = ~ filter(.x, !is.na(lab)),
              aes(label = lab, colour = lab_col), size = 3) +
    scale_colour_identity() +
    scale_fill_ratio(max_ratio = max_ratio) +
    scale_x_discrete(position = "top", expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    coord_fixed() +
    guides(fill = guide_colourbar(barwidth = unit(24, "lines"),
                                  barheight = unit(0.25, "lines"),
                                  ticks = FALSE)) +
    labs(title = title, subtitle = subtitle, caption = caption,
         x = NULL, y = NULL) +
    theme_hometown_legend(grid = "none", position = "bottom") +
    theme(legend.margin      = margin(t = 2),
          legend.box.spacing = unit(0.3, "lines"))
}

# =============================================================================
# 2a. Cross-sport calendar, pooled 1990+ eras
# =============================================================================
sport_rows <- c("NFL", "MLB", "NHL (all countries)", "NBA")

rae_all <- players |>
  filter(!is.na(era)) |>
  rae_by(sport) |>
  mutate(row_lab = factor(ifelse(sport == "NHL", "NHL (all countries)", sport),
                          levels = rev(sport_rows)))

cap_all <- fig_caption(
  source   = "nflverse (NFL), Lahman database (MLB), NHL API (NHL), Basketball-Reference (NBA)",
  universe = "Players with career starts 1990-2025.",
  note     = sprintf(paste0(
    "\nMLB and NBA rows are US-born players only; NFL is all players with birth dates. ",
    "NHL row includes all birth countries (%.0f%% Canadian-born).",
    "\nPooled NHL n = %s: month cells are hundreds of players, not thousands. ",
    "Baseline: days-in-month adjusted uniform (Feb = 28.25)."),
    100 * nhl_canada_share, format(nhl_pooled_n, big.mark = ","))
)

p_all <- rae_calendar(
  rae_all,
  title    = "Hockey is the only league where birth month matters",
  subtitle = "Birth-month share vs a days-adjusted uniform baseline, players with career starts 1990-2025",
  caption  = cap_all
)
save_fig("docs/figures/rae_cal_all.png", p_all, w = 10, h = 3.6)

# =============================================================================
# 2b. Per-sport era calendars (rows pre-1990 top through 2020s bottom)
# =============================================================================
sport_figs <- tribble(
  ~sport, ~file, ~title, ~source, ~universe, ~note,
  "NFL", "rae_cal_nfl",
  "Birth month barely matters in football, in any era",
  "nflverse",
  "All NFL players with birth dates, career starts pre-1990 through 2025.",
  "Baseline: days-in-month adjusted uniform (Feb = 28.25).",

  "MLB", "rae_cal_mlb",
  "Baseball tilts toward late-summer births, but the tilt fades in the 2020s",
  "Lahman database",
  "US-born MLB players with birth dates, career starts pre-1990 through 2025.",
  sprintf(paste0("%.0f%% of players with birth dates in the source were born outside the US ",
                 "(mostly Latin America) and are excluded.\n",
                 "Baseline: days-in-month adjusted uniform (Feb = 28.25)."),
          100 * mlb_intl_share),

  "NHL", "rae_cal_nhl",
  "The January effect is real, large, and stable across eras",
  "NHL API",
  "NHL players with birth dates, career starts pre-1990 through 2025, all birth countries.",
  sprintf(paste0("%.0f%% Canadian-born in the 1990+ eras. Era n: %s.\n",
                 "Small rows: month cells hold roughly 35 to 180 players. ",
                 "Baseline: days-in-month adjusted uniform (Feb = 28.25)."),
          100 * nhl_canada_share,
          paste(sprintf("%s %s", nhl_era_n$era2,
                        format(nhl_era_n$n, big.mark = ",", trim = TRUE)),
                collapse = ", ")),

  "NBA", "rae_cal_nba",
  "No birth-month pattern sticks in basketball, in any era",
  "Basketball-Reference",
  "US-born NBA players with birth dates, career starts pre-1990 through 2025.",
  "Baseline: days-in-month adjusted uniform (Feb = 28.25)."
)

for (i in seq_len(nrow(sport_figs))) {
  s <- sport_figs$sport[i]

  rae_s <- players |>
    filter(sport == s) |>
    mutate(era2 = coalesce(era, "pre-1990")) |>
    rae_by(era2) |>
    mutate(row_lab = factor(era2, levels = rev(era_levels)))

  min_cell <- min(rae_s$n)
  note <- sport_figs$note[i]
  if (min_cell < 30) {
    note <- paste0(note, sprintf(" Smallest era x month cell: %d players.", min_cell))
  }

  p <- rae_calendar(
    rae_s,
    title    = sport_figs$title[i],
    subtitle = sprintf("%s birth-month share vs a days-adjusted uniform baseline, by career-start era (rows), through 2025", s),
    caption  = fig_caption(
      source   = sport_figs$source[i],
      universe = sport_figs$universe[i],
      note     = paste0("\n", note)
    )
  )
  save_fig(sprintf("docs/figures/%s.png", sport_figs$file[i]), p, w = 10, h = 4.4)
}

cat("done\n")
