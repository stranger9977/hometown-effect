# =============================================================================
# 23_rae_anchor_charts.R -- two "anchor" RAE figures for the birthday-advantage
# page: a cross-league quarter comparison and an NHL month-by-month bar chart.
# Baseline identical to R/07, R/14, R/17: days-in-month adjusted uniform
# (Feb = 28.25). ratio = observed birth-period share / expected share.
# Universe per league matches the existing rae_{sport}.png / rae_cal figures
# built in R/14 and R/17: NFL is every player with a birth date (spine.parquet
# has no country field); MLB, NHL, NBA include ALL birth countries, pooled
# across every era (not just US-born, and not per-era) -- this is the same
# universe already saved in data/processed/multisport_tables.rds$rae, just
# regrouped by quarter/month instead of by era.
# Reads data/processed/*.parquet only; writes figures only.
# =============================================================================

suppressMessages({
  library(dplyr); library(arrow); library(ggplot2)
  library(lubridate); library(tibble)
})
source("R/lib/theme_hometown.R")

days_in_month <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
expected_month <- tibble(month = 1:12, expected_share = days_in_month / sum(days_in_month))

quarter_of_month <- function(m) ceiling(m / 3)
q_days <- tapply(days_in_month, quarter_of_month(1:12), sum)
expected_q <- tibble(quarter = as.integer(names(q_days)),
                     expected_share = as.numeric(q_days) / sum(days_in_month))

# --- Load the four player universes (all birth countries, all eras pooled) --
nfl <- read_parquet("data/processed/spine.parquet") |>
  filter(!is.na(birth_date)) |>
  transmute(sport = "NFL", birth_date = as.Date(birth_date))

mlb_src <- read_parquet("data/processed/players_mlb.parquet") |> filter(!is.na(birth_date))
mlb <- mlb_src |> transmute(sport = "MLB", birth_date = as.Date(birth_date))

nhl_src <- read_parquet("data/processed/players_nhl.parquet") |> filter(!is.na(birth_date))
nhl <- nhl_src |> transmute(sport = "NHL", birth_date = as.Date(birth_date))

nba <- read_parquet("data/processed/players_nba.parquet") |>
  filter(!is.na(birth_date)) |>
  transmute(sport = "NBA", birth_date = as.Date(birth_date))

players <- bind_rows(nfl, mlb, nhl, nba) |>
  mutate(month = month(birth_date), quarter = quarter_of_month(month))

# Caption facts, computed from the data actually plotted.
mlb_intl_share   <- mean(mlb_src$birth_country != "USA")
nhl_canada_share <- mean(nhl_src$birth_country == "CAN")

# =============================================================================
# 1. ba_rae_quarter.png -- birth-quarter representation ratio, all 4 leagues,
#    pooled across eras. The cleanest "born early = overrepresented" figure.
# =============================================================================
quarter_tbl <- players |>
  count(sport, quarter) |>
  group_by(sport) |>
  mutate(share = n / sum(n)) |>
  ungroup() |>
  left_join(expected_q, by = "quarter") |>
  mutate(ratio = share / expected_share,
         sport = factor(sport, levels = c("NFL", "MLB", "NHL", "NBA")),
         quarter_lab = factor(paste0("Q", quarter), levels = paste0("Q", 1:4)))

cat("=== birth-quarter ratio by league (pooled eras) ===\n")
print(as.data.frame(quarter_tbl |> select(sport, quarter_lab, n, ratio)))

p_quarter <- ggplot(quarter_tbl, aes(quarter_lab, ratio, fill = sport)) +
  geom_baseline(1) +
  geom_col(width = 0.65) +
  geom_text(aes(label = sprintf("%.2f", ratio)), vjust = -0.5, size = 3.1,
           colour = ink_body) +
  facet_wrap(~sport, nrow = 1) +
  scale_fill_manual(values = pal_sport, guide = "none") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.15))) +
  labs(title = "Hockey players are born early in the year; the other three leagues are not",
       subtitle = "Birth-quarter share vs a days-adjusted uniform baseline, all eras pooled (Q1 = Jan-Mar ... Q4 = Oct-Dec)",
       x = NULL, y = "Representation ratio (1 = expected)",
       caption = fig_caption(
         "nflverse (NFL), Lahman database (MLB), NHL API (NHL), Basketball-Reference (NBA)",
         "All players with birth dates, all eras pooled.",
         sprintf(paste0(
           "\nMLB and NHL rows include all birth countries (%.0f%% of MLB players born outside the US, mostly Latin America;\n",
           "%.0f%% of NHL players Canadian-born). NBA is entirely US-born in this source; NFL has no birth-country field.\n",
           "Baseline: days-in-month adjusted uniform (Feb = 28.25)."),
           100 * mlb_intl_share, 100 * nhl_canada_share))) +
  theme_hometown() +
  theme(strip.text = element_text(hjust = 0.5))
save_fig("docs/figures/ba_rae_quarter.png", p_quarter, w = 12, h = 5.4)

# =============================================================================
# 2. ba_hockey_months.png -- NHL birth-month bars, all players, all countries,
#    all eras pooled: the textbook relative age effect, January/February peak
#    to a November/December trough.
# =============================================================================
nhl_month <- nhl |>
  mutate(month = month(birth_date)) |>
  count(month) |>
  mutate(share = n / sum(n)) |>
  left_join(expected_month, by = "month") |>
  mutate(ratio = share / expected_share,
         month_lab = factor(month.abb[month], levels = month.abb))

cat("\n=== NHL birth-month ratio, all countries, all eras pooled ===\n")
print(as.data.frame(nhl_month |> select(month_lab, n, ratio)))

jan_feb_ratio <- mean(nhl_month$ratio[nhl_month$month %in% c(1, 2)])
nov_dec_ratio <- mean(nhl_month$ratio[nhl_month$month %in% c(11, 12)])

p_hockey_months <- ggplot(nhl_month, aes(month_lab, ratio)) +
  geom_baseline(1) +
  geom_col(fill = pal_sport[["NHL"]], width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", ratio)), vjust = -0.5, size = 3.1,
           colour = ink_body) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.12))) +
  labs(title = "Born in January, made the NHL: the textbook relative age effect",
       subtitle = "NHL birth-month share vs a days-adjusted uniform baseline, all eras pooled, all birth countries",
       x = NULL, y = "Representation ratio (1 = expected)",
       caption = fig_caption(
         "NHL API",
         sprintf("All NHL players with birth dates (n = %s), all eras pooled, all birth countries (%.0f%% Canadian-born).",
                 format(sum(nhl_month$n), big.mark = ",", trim = TRUE), 100 * nhl_canada_share),
         sprintf(paste0(
           "\nJanuary/February players are overrepresented (avg ratio %.2f); November/December players are underrepresented (avg ratio %.2f).\n",
           "Youth hockey age cutoffs group kids by birth year, so the oldest players in a cohort (born early) look more physically advanced\n",
           "and get more coaching and ice time. That edge compounds every season until it shows up here, in the pros.\n",
           "Baseline: days-in-month adjusted uniform (Feb = 28.25)."),
           jan_feb_ratio, nov_dec_ratio))) +
  theme_hometown() +
  theme(axis.text.x = element_text(angle = 0))
save_fig("docs/figures/ba_hockey_months.png", p_hockey_months, w = 12, h = 6.2)

cat("\ndone\n")
