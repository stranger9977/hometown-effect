# =============================================================================
# 34_skew_over_time.R -- has the relative age effect grown or shrunk over time?
#
# RAE-strength metric: for each sport x era, (share of players born in Q1,
# Jan-Mar) minus (share born in Q4, Oct-Dec), in percentage points. A positive
# number means the sport still skews toward early-year birthdays; zero means
# no skew; negative means it actually leans late-year.
#
# Baseline: real US birth seasonality (data/processed/us_birth_seasonality.csv,
# built in R/27_rae_realbirths.R from CDC/NCHS daily counts) gives the true
# expected Q1-Q4 gap, which is NOT zero -- Q4 has slightly more real births
# than Q1 because of the late-summer birth peak. That expected gap is drawn
# as the reference line instead of a flat zero.
#
# Player universes: same four-sport read as R/23 and R/27 (all birth dates,
# all birth countries pooled), but split by era instead of pooled across it.
# Reads processed parquet + the CDC-derived CSV only; writes one figure.
# =============================================================================

suppressMessages({
  library(dplyr); library(arrow); library(ggplot2)
  library(lubridate); library(tibble); library(readr)
})
source("R/lib/theme_hometown.R")

# -----------------------------------------------------------------------
# Baseline: expected Q1 vs Q4 share under REAL US birth seasonality.
# -----------------------------------------------------------------------
birth_month <- read_csv("data/processed/us_birth_seasonality.csv", show_col_types = FALSE)

quarter_of_month <- function(m) ceiling(m / 3)

birth_quarter <- birth_month |>
  mutate(quarter = quarter_of_month(month)) |>
  group_by(quarter) |>
  summarise(expected_share = sum(expected_share), .groups = "drop")

baseline_gap <- 100 * (birth_quarter$expected_share[birth_quarter$quarter == 1] -
                         birth_quarter$expected_share[birth_quarter$quarter == 4])
cat(sprintf("Real-birth expected Q1-Q4 gap (the 'zero' line): %.2f points (Q4 has slightly more real births than Q1)\n\n",
            baseline_gap))

# -----------------------------------------------------------------------
# Load the four player universes, split by era. Drop rows with no era or
# no birth date.
# -----------------------------------------------------------------------
nfl <- read_parquet("data/processed/spine.parquet") |>
  filter(!is.na(birth_date), !is.na(era)) |>
  transmute(sport = "NFL", era, birth_date = as.Date(birth_date))
mlb <- read_parquet("data/processed/players_mlb.parquet") |>
  filter(!is.na(birth_date), !is.na(era)) |>
  transmute(sport = "MLB", era, birth_date = as.Date(birth_date))
nhl <- read_parquet("data/processed/players_nhl.parquet") |>
  filter(!is.na(birth_date), !is.na(era)) |>
  transmute(sport = "NHL", era, birth_date = as.Date(birth_date))
nba <- read_parquet("data/processed/players_nba.parquet") |>
  filter(!is.na(birth_date), !is.na(era)) |>
  transmute(sport = "NBA", era, birth_date = as.Date(birth_date))

sport_levels <- c("NFL", "MLB", "NHL", "NBA")
era_levels <- c("1990s", "2000s", "2010s", "2020s")

players <- bind_rows(nfl, mlb, nhl, nba) |>
  mutate(sport = factor(sport, levels = sport_levels),
         era = factor(era, levels = era_levels),
         month = month(birth_date),
         quarter = quarter_of_month(month))

# -----------------------------------------------------------------------
# RAE-strength table: sport x era, with a minimum-n screen.
# -----------------------------------------------------------------------
MIN_N <- 50

rae_table <- players |>
  group_by(sport, era) |>
  summarise(n = n(),
            n_q1 = sum(quarter == 1),
            n_q4 = sum(quarter == 4),
            share_q1 = n_q1 / n,
            share_q4 = n_q4 / n,
            rae_strength = 100 * (share_q1 - share_q4),
            .groups = "drop") |>
  arrange(sport, era)

thin <- rae_table |> filter(n < MIN_N)
if (nrow(thin) > 0) {
  cat("Dropping thin sport x era cells (n <", MIN_N, "players):\n")
  print(thin |> select(sport, era, n) |> as.data.frame())
  rae_table <- rae_table |> filter(n >= MIN_N)
} else {
  cat(sprintf("No sport x era cell has fewer than %d players; none dropped.\n", MIN_N))
}

cat("\n=== RAE strength (Q1 share minus Q4 share, percentage points) by sport x era ===\n")
print(rae_table |>
        mutate(share_q1 = round(100 * share_q1, 1),
               share_q4 = round(100 * share_q4, 1),
               rae_strength = round(rae_strength, 1)) |>
        as.data.frame())

# -----------------------------------------------------------------------
# Chart: one line per sport, direct-labeled at the 2020s end, no legend.
# -----------------------------------------------------------------------
end_labels <- rae_table |>
  filter(era == era_levels[length(era_levels)]) |>
  mutate(label = sprintf("%s %+.1f", sport, rae_strength))

p <- ggplot(rae_table, aes(era, rae_strength, colour = sport, group = sport)) +
  geom_baseline(baseline_gap) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.6) +
  scale_colour_manual(values = pal_sport) +
  scale_y_continuous(limits = c(-6, 17), breaks = seq(-5, 15, 5)) +
  direct_label(end_labels, aes(era, rae_strength, label = label, colour = sport),
               nudge_x = 0.13, size = 3.6, inherit.aes = FALSE) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Hockey's early-birth skew hasn't shrunk in three decades; the other three leagues never had one",
    subtitle = "RAE strength = share of players born Jan-Mar minus share born Oct-Dec, by draft/debut-era cohort",
    x = NULL, y = "Q1 share minus Q4 share (points)",
    caption = fig_caption(
      "nflverse (NFL), Lahman (MLB), NHL API (NHL), Basketball-Reference (NBA); baseline CDC/NCHS births 1994-2003",
      paste0("\nAll players with birth dates, all birth countries pooled, grouped by draft/debut-era cohort. ",
             "Every sport x era cell has at least ", MIN_N, " players (NBA is thinnest, 531-654 per era)."),
      paste0("\nDashed line is the expected gap under real US birth seasonality (", round(baseline_gap, 1),
             " points), not zero, since Q4 gets slightly more real births than Q1.\n",
             "NHL sits 12-15 points above that line in every era with no consistent rise or fall. NFL, MLB, and NBA all stay within about 4 points of it throughout.")
    )) +
  theme_hometown(grid = "y") +
  theme(plot.margin = margin(10, 62, 8, 10))

save_fig("docs/figures/ba_skew_over_time.png", p, w = 12, h = 6.75)

cat("\ndone\n")
