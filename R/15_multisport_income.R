suppressMessages({
  library(dplyr); library(arrow); library(ggplot2); library(tidyr); library(tibble)
})
source("R/lib/places.R")

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

era_palette   <- c("1990s" = "#A6BDDB", "2000s" = "#74A9CF",
                    "2010s" = "#2B8CBE", "2020s" = "#045A8D")
sport_palette <- c(NFL = "#0072B2", MLB = "#D55E00", NHL = "#009E73", NBA = "#CC79A7")
era_levels    <- c("1990s", "2000s", "2010s", "2020s")
caption_theme <- theme(plot.caption = element_text(hjust = 0, size = rel(0.7)))

us_filter <- function(df) {
  df |> filter(birth_country == "USA", birth_state %in% c(state.abb, "DC"))
}

sport_meta <- tribble(
  ~sport,  ~label, ~source_label,
  "mlb",   "MLB",  "Lahman database",
  "nhl",   "NHL",  "NHL API",
  "nba",   "NBA",  "Basketball-Reference"
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
    thin_note <- sprintf(" %s's smallest era x quartile cell has only n=%d players -- read thin cells cautiously.",
                          lab, min_cell)
  }

  p <- ggplot(income_tbl, aes(factor(income_quartile), share, fill = era)) +
    geom_hline(yintercept = 0.25, linetype = "dashed", color = "grey40") +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    scale_fill_manual(values = era_palette) +
    scale_x_discrete(labels = c("Q1 (poorest)", "Q2", "Q3", "Q4 (richest)")) +
    labs(title = sprintf("Do %s players increasingly come from richer hometowns?", lab),
         subtitle = "Share of players by hometown income quartile (population-weighted, era-matched vintages)\nDashed line = proportional (25%)",
         x = "Hometown median household income quartile", y = "Share of players",
         fill = "Career-start era",
         caption = sprintf(
           "Data: %s + US Census (2000 SF3 income for 1990s/2000s cohorts, ACS 2023 for 2010s/2020s).\n%.0f%% of matched players lacked a vintage income value and are excluded.%s",
           src, 100 * no_income_share, thin_note)) +
    theme_minimal(base_size = 16) +
    theme(panel.grid.minor = element_blank(),
          plot.caption = element_text(hjust = 0, size = rel(0.7)))
  ggsave(sprintf("docs/figures/income_gradient_%s.png", s), p, width = 12, height = 6.75, dpi = 320)

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

p_cross <- ggplot(q4_cross, aes(era, share, color = sport, group = sport)) +
  geom_hline(yintercept = 0.25, linetype = "dashed", color = "grey40") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = sport_palette) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(title = "Richest-quartile hometown share, across four sports",
       subtitle = "Share of players whose hometown is in the top income quartile (population-weighted, era-matched vintages)\nDashed line = proportional (25%)",
       x = "Career-start era", y = "Share of players from a Q4 (richest-quartile) hometown",
       color = "Sport",
       caption = paste0(
         "Data: nflverse+ESPN, Lahman, NHL API, Basketball-Reference + US Census. US-born players only.\n",
         "Era-matched income vintages: 2000 SF3 for 1990s/2000s cohorts, ACS 2023 for 2010s/2020s.\n",
         "Cohort edges differ by up to one season across sports (source conventions).\n",
         "NHL's US-born sample is small (n~180-370/era, ~45-90/quartile-era after income match) -- read that line cautiously.")) +
  theme_minimal(base_size = 16) +
  theme(panel.grid.minor = element_blank()) +
  caption_theme
ggsave("docs/figures/crosssport_income.png", p_cross, width = 12, height = 6.75, dpi = 320)
cat("\nwrote docs/figures/crosssport_income.png\n")
