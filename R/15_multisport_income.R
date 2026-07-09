suppressMessages({
  library(dplyr); library(arrow); library(ggplot2); library(tidyr); library(tibble)
})
source("R/lib/places.R")
source("R/lib/theme_hometown.R")

dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

places <- read_parquet("data/processed/census_places.parquet")

stopifnot(all(places$income1999 >= 0, na.rm = TRUE))
stopifnot(all(places$income_now  >= 0, na.rm = TRUE))

# Same population-weighted income-quartile cutpoints as R/10_income.R, computed
# once from Census places -- sport-independent (every sport's players get
# graded against the same national income distribution, same as R/14's
# sport-independent pop_bins).
wq <- function(income, pop) {
  ok <- !is.na(income) & !is.na(pop) & pop > 0
  o <- order(income[ok])
  cw <- cumsum(as.numeric(pop[ok][o])) / sum(as.numeric(pop[ok][o]))
  list(income = income[ok][o], cw = cw)
}
cuts <- function(w) sapply(c(.25, .5, .75), function(q) w$income[which.max(w$cw >= q)])

cut99  <- cuts(wq(places$income1999, places$pop2000))
cutnow <- cuts(wq(places$income_now, places$pop_now))

era_levels <- c("1990s", "2000s", "2010s", "2020s")

us_filter <- function(df) {
  df |> filter(birth_country == "USA", birth_state %in% c(state.abb, "DC"))
}

sport_meta <- tribble(
  ~sport,  ~label, ~source_label,
  ~income_title,
  "mlb",   "MLB",  "Lahman database",
  "MLB's share of players from the richest hometowns has doubled since the 1990s",
  "nhl",   "NHL",  "NHL API",
  "Nearly half of recent US-born NHL players grew up in the richest quartile",
  "nba",   "NBA",  "Basketball-Reference",
  "Most NBA players still come from below-median-income hometowns"
)

# ================= Per sport: match, income quartile table, figure =================
income_tbl_list <- list()

for (i in seq_len(nrow(sport_meta))) {
  s <- sport_meta$sport[i]; lab <- sport_meta$label[i]; src <- sport_meta$source_label[i]

  players <- read_parquet(sprintf("data/processed/players_%s.parquet", s)) |> us_filter()
  matched <- match_places(players, places) |>
    filter(match_tier != "unmatched", !is.na(era))

  income_all <- matched |>
    mutate(vintage = if_else(era %in% c("1990s", "2000s"), "1999", "now"),
           income = if_else(vintage == "1999", matched_income1999, matched_income_now))

  no_income_share <- mean(is.na(income_all$income))

  income_tbl <- income_all |>
    mutate(q = case_when(
             is.na(income) ~ NA_integer_,
             vintage == "1999" ~ findInterval(income, cut99) + 1L,
             TRUE              ~ findInterval(income, cutnow) + 1L)) |>
    filter(!is.na(q)) |>
    count(era, income_quartile = q) |>
    group_by(era) |> mutate(share = n / sum(n)) |> ungroup()

  # Same sanity guard as R/10_income.R: shares must sum to 1 per era.
  era_sums <- income_tbl |> group_by(era) |> summarise(total = sum(share), .groups = "drop")
  stopifnot(all(abs(era_sums$total - 1) < 1e-9))

  income_tbl_list[[s]] <- income_tbl |> mutate(sport = lab)

  min_cell <- min(income_tbl$n)
  thin_note <- ""
  if (min_cell < 30) {
    thin_note <- sprintf("\n%s's smallest era x quartile cell has only n=%d players; read thin cells cautiously.",
                          lab, min_cell)
  }
  # Spec rule 11: NHL always carries a small-n disclosure (one compact line
  # replacing the generic thin-cell note, so the caption stays at 3 lines).
  if (s == "nhl") {
    era_n <- income_tbl |> group_by(era) |> summarise(n = sum(n), .groups = "drop")
    thin_note <- sprintf("\nNHL is small: per-era n runs %d to %d players and quartile cells are dozens (smallest n=%d); read thin cells cautiously.",
                         min(era_n$n), max(era_n$n), min_cell)
  }

  # 4-era dodge keeps the one-row bottom legend (spec rule 8 exception:
  # in-panel labels over bars 0.2 x-units apart physically collide).
  p <- ggplot(income_tbl, aes(factor(income_quartile), share, fill = era)) +
    geom_baseline(0.25) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    scale_fill_manual(values = pal_era) +
    scale_x_discrete(labels = c("Q1 (poorest)", "Q2", "Q3", "Q4 (richest)")) +
    scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                       expand = expansion(mult = c(0, 0.05))) +
    labs(title = sport_meta$income_title[i],
         subtitle = "Share of players by hometown median household income quartile (population-weighted, era-matched vintages). Dashed line: proportional (25%).",
         x = NULL, y = "Share of players",
         caption = fig_caption(
           paste0(src, " + US Census"),
           "US-born players, career starts 1990-2025.",
           sprintf("\nIncome vintages: 2000 SF3 for 1990s/2000s cohorts, ACS 2023 for 2010s/2020s. %.0f%% of matched players lacked a vintage income value and are excluded.%s",
                   100 * no_income_share, thin_note))) +
    theme_hometown() +
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          legend.key.size = unit(0.7, "lines"))
  save_fig(sprintf("docs/figures/income_gradient_%s.png", s), p)

  cat(sprintf("wrote income_gradient_%s.png (min era x quartile cell n = %d)\n", s, min_cell))
}

# ================= Combine with NFL into one table =================
nfl_tbl <- read.csv("data/processed/income_table.csv", stringsAsFactors = FALSE) |>
  mutate(sport = "NFL")

income_by_sport <- bind_rows(
  nfl_tbl,
  income_tbl_list$mlb, income_tbl_list$nhl, income_tbl_list$nba
) |>
  select(sport, era, income_quartile, n, share) |>
  mutate(sport = factor(sport, levels = c("NFL", "MLB", "NHL", "NBA")),
         era = factor(era, levels = era_levels)) |>
  arrange(sport, era, income_quartile) |>
  mutate(sport = as.character(sport), era = as.character(era))

write.csv(income_by_sport, "data/processed/income_by_sport.csv", row.names = FALSE)
cat("\nwrote data/processed/income_by_sport.csv\n")

# ================= Interrogation: full Q1 / Q4 tables + per-era n, flag thin cells =================
cat("\n=== Q1 (poorest) share by sport and era ===\n")
income_by_sport |>
  filter(income_quartile == 1) |>
  mutate(sport = factor(sport, levels = c("NFL", "MLB", "NHL", "NBA")),
         era = factor(era, levels = era_levels)) |>
  select(sport, era, share) |>
  pivot_wider(names_from = era, values_from = share) |>
  arrange(sport) |>
  print()

cat("\n=== Q4 (richest) share by sport and era ===\n")
income_by_sport |>
  filter(income_quartile == 4) |>
  mutate(sport = factor(sport, levels = c("NFL", "MLB", "NHL", "NBA")),
         era = factor(era, levels = era_levels)) |>
  select(sport, era, share) |>
  pivot_wider(names_from = era, values_from = share) |>
  arrange(sport) |>
  print()

cat("\n=== n (players with a valid income-quartile) by sport and era ===\n")
income_by_sport |>
  group_by(sport, era) |>
  summarise(n = sum(n), .groups = "drop") |>
  mutate(sport = factor(sport, levels = c("NFL", "MLB", "NHL", "NBA")),
         era = factor(era, levels = era_levels)) |>
  pivot_wider(names_from = era, values_from = n) |>
  arrange(sport) |>
  print()

cat("\n=== every era x quartile x sport cell with n < 30 ===\n")
thin_cells <- income_by_sport |> filter(n < 30) |> arrange(sport, era, income_quartile)
if (nrow(thin_cells) > 0) print(thin_cells) else cat("(none)\n")
cat(sprintf("Smallest cell overall: n = %d\n", min(income_by_sport$n)))

# ================= The payoff figure: Q4 trajectory across sports =================
q4_cross <- income_by_sport |>
  filter(income_quartile == 4) |>
  mutate(sport = factor(sport, levels = c("NFL", "MLB", "NHL", "NBA")),
         era = factor(era, levels = era_levels))

# End-of-line labels ("NHL 48%" etc.) replace the legend. Final values are
# at least 4.6 points apart on a 0-50% axis, so labels do not collide with
# each other; NFL (24.9%) is nudged below the dashed 25% baseline so the
# baseline does not strike through its label.
# This PNG is also the static fallback for the interactive version.
q4_lab <- q4_cross |>
  filter(era == "2020s") |>
  mutate(lab = sprintf("%s %.0f%%", sport, round(100 * share)),
         label_y = share + if_else(sport == "NFL", -0.016, 0))

p_cross <- ggplot(q4_cross, aes(era, share, colour = sport, group = sport)) +
  geom_baseline(0.25) +
  geom_line(aes(alpha = sport), linewidth = 1.2) +
  geom_point(aes(alpha = sport), size = 2.5) +
  scale_color_manual(values = pal_sport) +
  scale_alpha_manual(values = c(NFL = 1, MLB = 1, NHL = 0.65, NBA = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, NA), expand = expansion(mult = c(0, 0.06))) +
  direct_label(q4_lab,
               aes(x = era, y = label_y, label = lab, colour = sport),
               nudge_x = 0.12, size = 3.4, inherit.aes = FALSE) +
  coord_cartesian(clip = "off") +
  labs(title = "Every sport now draws far more of its players from the richest hometowns",
       subtitle = "Share of players whose hometown is in the top income quartile (population-weighted, era-matched vintages), by career-start era. Dashed line: proportional (25%).",
       x = NULL, y = "Share from a Q4 (richest-quartile) hometown",
       caption = fig_caption(
         "nflverse+ESPN, Lahman, NHL API, Basketball-Reference + US Census",
         "US-born players, career starts 1990-2025.",
         paste0("\nIncome vintages: 2000 SF3 for 1990s/2000s cohorts, ACS 2023 for 2010s/2020s. ",
                "Cohort edges differ by up to one season across sports.\n",
                "NHL's US-born sample is small (about 180 to 370 players per era), so its line is drawn lighter: read it with caution."))) +
  theme_hometown() +
  theme(plot.margin = margin(10, 70, 8, 10))
save_fig("docs/figures/crosssport_income.png", p_cross)
