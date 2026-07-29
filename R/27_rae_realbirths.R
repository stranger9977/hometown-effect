# =============================================================================
# 27_rae_realbirths.R -- Michael's Birthday-thread asks, three at once:
#   (A) Re-baseline the relative age effect on ACTUAL US birth seasonality
#       instead of a days-in-month uniform line. Real US births peak in late
#       summer and dip in winter, so a flat baseline is not quite fair. Source:
#       CDC/NCHS natality daily counts 1994-2003 (data/raw/births), aggregated
#       to a monthly and quarterly share of all births.
#   (B) A clean birth-MONTH bar chart for each league (NFL, MLB, NHL, NBA),
#       observed share of players per month with the real-birth baseline drawn
#       on top as a reference line. This is the "bar charts, one per sport,
#       easy to read" figure Michael asked for, and it answers "did you weight
#       by actual births?" by drawing the actual-birth curve right on the bars.
#   (C) The classic QUARTER view: percent of players in each quarter Q1-Q4, one
#       marker per league, against the real-birth expected quarter share. This
#       matches the shape of the original 1982/83 NHL relative-age figure.
#
# Player universes match R/23_rae_anchor_charts.R exactly (all birth dates,
# all countries, all eras pooled). Reads processed parquet + the CDC CSV only;
# writes figures + one small processed table (the birth-seasonality baseline).
# =============================================================================

suppressMessages({
  library(dplyr); library(arrow); library(ggplot2)
  library(lubridate); library(tibble); library(readr); library(tidyr)
})
source("R/lib/theme_hometown.R")

# -----------------------------------------------------------------------
# (A) Real US birth seasonality baseline, from CDC/NCHS daily counts.
# expected_share = a month's (or quarter's) share of ALL births 1994-2003.
# This already folds in BOTH month length and seasonality, which is exactly
# the "weight by actual births" baseline Michael asked for.
# -----------------------------------------------------------------------
births_raw <- read_csv("data/raw/births/US_births_1994-2003_CDC_NCHS.csv",
                       show_col_types = FALSE)

birth_month <- births_raw |>
  group_by(month) |>
  summarise(births = sum(births), .groups = "drop") |>
  mutate(expected_share = births / sum(births))

quarter_of_month <- function(m) ceiling(m / 3)

birth_quarter <- birth_month |>
  mutate(quarter = quarter_of_month(month)) |>
  group_by(quarter) |>
  summarise(expected_share = sum(expected_share), .groups = "drop")

# Save the baseline so other scripts can reuse it.
write_csv(birth_month |> mutate(month_abb = month.abb[month]) |>
            select(month, month_abb, births, expected_share),
          "data/processed/us_birth_seasonality.csv")

# The old uniform (days-in-month) baseline, for the before/after comparison.
days_in_month <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
uniform_month <- tibble(month = 1:12, uniform_share = days_in_month / sum(days_in_month))

cat("=== (A) real US birth seasonality vs the old uniform baseline ===\n")
print(birth_month |>
        left_join(uniform_month, by = "month") |>
        mutate(month = month.abb[month],
               real_pct = round(100 * expected_share, 2),
               uniform_pct = round(100 * uniform_share, 2)) |>
        select(month, real_pct, uniform_pct) |> as.data.frame())
peak <- birth_month$month[which.max(birth_month$expected_share)]
trough <- birth_month$month[which.min(birth_month$expected_share)]
cat(sprintf("real births peak in %s, trough in %s\n\n", month.name[peak], month.name[trough]))

# -----------------------------------------------------------------------
# Load the four player universes (identical to R/23).
# -----------------------------------------------------------------------
nfl <- read_parquet("data/processed/spine.parquet") |>
  filter(!is.na(birth_date)) |> transmute(sport = "NFL", birth_date = as.Date(birth_date))
mlb_src <- read_parquet("data/processed/players_mlb.parquet") |> filter(!is.na(birth_date))
mlb <- mlb_src |> transmute(sport = "MLB", birth_date = as.Date(birth_date))
nhl_src <- read_parquet("data/processed/players_nhl.parquet") |> filter(!is.na(birth_date))
nhl <- nhl_src |> transmute(sport = "NHL", birth_date = as.Date(birth_date))
nba <- read_parquet("data/processed/players_nba.parquet") |>
  filter(!is.na(birth_date)) |> transmute(sport = "NBA", birth_date = as.Date(birth_date))

players <- bind_rows(nfl, mlb, nhl, nba) |>
  mutate(month = month(birth_date), quarter = quarter_of_month(month))

sport_levels <- c("NFL", "MLB", "NHL", "NBA")

# -----------------------------------------------------------------------
# (B) Per-sport birth-MONTH bars, observed share vs the real-birth line.
# -----------------------------------------------------------------------
month_obs <- players |>
  count(sport, month, name = "n") |>
  group_by(sport) |>
  mutate(obs_share = n / sum(n)) |>
  ungroup() |>
  left_join(birth_month |> select(month, expected_share), by = "month") |>
  mutate(ratio = obs_share / expected_share,
         sport = factor(sport, levels = sport_levels),
         month_lab = factor(month.abb[month], levels = month.abb))

# expected line, one copy per facet
exp_line <- tidyr::crossing(sport = factor(sport_levels, levels = sport_levels),
                            birth_month |> select(month, expected_share)) |>
  mutate(month_lab = factor(month.abb[month], levels = month.abb))

p_month <- ggplot(month_obs, aes(month_lab, 100 * obs_share)) +
  geom_col(aes(fill = sport), width = 0.72) +
  geom_line(data = exp_line, aes(month_lab, 100 * expected_share, group = 1),
            colour = ink_baseline, linewidth = 0.5, linetype = "dashed") +
  facet_wrap(~sport, nrow = 2) +
  scale_fill_manual(values = pal_sport, guide = "none") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "Only hockey players cluster in the early months; the dashed line is when Americans are actually born",
    subtitle = "Share of players born in each month (bars) vs the real US birth distribution (dashed line), all eras pooled",
    x = NULL, y = "Share of players (%)",
    caption = fig_caption(
      "nflverse (NFL), Lahman (MLB), NHL API (NHL), Basketball-Reference (NBA); baseline CDC/NCHS births 1994-2003",
      "All players with birth dates, all eras and birth countries pooled.",
      paste0("\nDashed line is the actual US birth distribution, not a flat line, so it already accounts for the late-summer birth peak.\n",
             "Bars above the line mean that month is overrepresented among players. Hockey towers over the line Jan-Mar and falls under it Oct-Dec.\n",
             "The other three leagues track the birth line closely, i.e. almost no relative age effect."))) +
  theme_hometown(grid = "y") +
  theme(axis.text.x = element_text(size = rel(0.62)),
        panel.spacing = unit(1.3, "lines"))
save_fig("docs/figures/ba_month_bars_bysport.png", p_month, w = 12, h = 7.4)

# -----------------------------------------------------------------------
# (C) Classic quarter view: percent of players per quarter, real-birth expected.
# -----------------------------------------------------------------------
q_obs <- players |>
  count(sport, quarter, name = "n") |>
  group_by(sport) |>
  mutate(obs_share = n / sum(n)) |>
  ungroup() |>
  left_join(birth_quarter, by = "quarter") |>
  mutate(sport = factor(sport, levels = sport_levels),
         quarter_lab = factor(paste0("Q", quarter), levels = paste0("Q", 1:4)))

q_exp_line <- tidyr::crossing(sport = factor(sport_levels, levels = sport_levels),
                              birth_quarter) |>
  mutate(quarter_lab = factor(paste0("Q", quarter), levels = paste0("Q", 1:4)))

p_quarter <- ggplot(q_obs, aes(quarter_lab, 100 * obs_share, group = sport)) +
  geom_line(data = q_exp_line, aes(quarter_lab, 100 * expected_share, group = 1),
            colour = ink_baseline, linewidth = 0.5, linetype = "dashed") +
  geom_line(aes(colour = sport), linewidth = 1) +
  geom_point(aes(colour = sport), size = 2.6) +
  geom_text(aes(label = sprintf("%.0f%%", 100 * obs_share), colour = sport),
            vjust = -1.0, size = 3.0, fontface = "bold", show.legend = FALSE) +
  facet_wrap(~sport, nrow = 1) +
  scale_colour_manual(values = pal_sport, guide = "none") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0.02, 0.16))) +
  labs(
    title = "The share of players born in each quarter: hockey slides from Q1 to Q4, the rest stay level",
    subtitle = "Percent of players born in each birth quarter (Q1 = Jan-Mar ... Q4 = Oct-Dec), against the real US birth share (dashed)",
    x = NULL, y = "Share of players (%)",
    caption = fig_caption(
      "nflverse (NFL), Lahman (MLB), NHL API (NHL), Basketball-Reference (NBA); baseline CDC/NCHS births 1994-2003",
      "All players with birth dates, all eras and birth countries pooled.",
      "\nThis is the same view as the original 1982 NHL relative-age figure: raw share of players per quarter. Dashed line is the real birth share.")) +
  theme_hometown(grid = "y") +
  theme(strip.text = element_text(hjust = 0.5),
        axis.text.x = element_text(size = rel(0.8)))
save_fig("docs/figures/ba_quarter_share.png", p_quarter, w = 12, h = 5.2)

# -----------------------------------------------------------------------
# Reporting: how much does the real-birth baseline move the headline numbers?
# -----------------------------------------------------------------------
uni_q <- tibble(quarter = 1:4,
                uniform_share = as.numeric(tapply(days_in_month, quarter_of_month(1:12), sum)) / sum(days_in_month))

hockey_cmp <- q_obs |> filter(sport == "NHL") |>
  left_join(uni_q, by = "quarter") |>
  mutate(ratio_real = obs_share / expected_share,
         ratio_uniform = obs_share / uniform_share)
cat("=== (A/C) NHL quarter ratios: real-birth baseline vs old uniform baseline ===\n")
print(hockey_cmp |> mutate(across(starts_with("ratio"), ~round(.x, 3))) |>
        select(quarter_lab, obs_pct = obs_share, ratio_real, ratio_uniform) |>
        mutate(obs_pct = round(100 * obs_pct, 1)) |> as.data.frame())

# CEO July, re-baselined (the summer dip should deepen under real births).
if (file.exists("data/processed/ceo_birthdates.csv")) {
  ceo <- read_csv("data/processed/ceo_birthdates.csv", show_col_types = FALSE) |>
    mutate(dob = as.Date(dob), month = as.integer(format(dob, "%m")))
  ceo_m <- ceo |> count(month, name = "n") |> mutate(obs_share = n / sum(n)) |>
    left_join(birth_month |> select(month, expected_share), by = "month") |>
    left_join(uniform_month, by = "month") |>
    mutate(ratio_real = obs_share / expected_share,
           ratio_uniform = obs_share / uniform_share)
  cat(sprintf("\n=== CEO summer months: ratio under real vs uniform baseline (n = %d) ===\n", nrow(ceo)))
  print(ceo_m |> filter(month %in% c(6, 7, 8)) |>
          transmute(month = month.abb[month],
                    ratio_real = round(ratio_real, 3),
                    ratio_uniform = round(ratio_uniform, 3)) |> as.data.frame())
}

cat("\ndone\n")
