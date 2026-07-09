suppressMessages({
  library(dplyr); library(arrow); library(ggplot2); library(tidyr)
  library(purrr); library(sf); library(usmap); library(lubridate); library(tibble)
})
source("R/lib/bins.R")
source("R/lib/places.R")

dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

places <- read_parquet("data/processed/census_places.parquet")
co_pop <- read_parquet("data/processed/census_counties.parquet")
counties_sf <- st_read(list.files("data/raw/census/cb_2023_us_county_500k",
                                  pattern = "[.]shp$", full.names = TRUE),
                       quiet = TRUE)

era_palette   <- c("1990s" = "#A6BDDB", "2000s" = "#74A9CF",
                    "2010s" = "#2B8CBE", "2020s" = "#045A8D")
sport_palette <- c(NFL = "#0072B2", MLB = "#D55E00", NHL = "#009E73", NBA = "#CC79A7")
caption_theme <- theme(plot.caption = element_text(hjust = 0, size = rel(0.7)))

sport_meta <- tribble(
  ~sport,  ~label, ~source_label,
  "mlb",   "MLB",  "Lahman database",
  "nhl",   "NHL",  "NHL API",
  "nba",   "NBA",  "Basketball-Reference"
)

us_filter <- function(df) {
  df |> filter(birth_country == "USA", birth_state %in% c(state.abb, "DC"))
}

# --- Population share by bin, per era vintage. Sport-independent: same census
# places, same vintages, regardless of which sport's players are being compared. ---
pl_vintage <- c("1990s" = "pop2000", "2000s" = "pop2000",
                "2010s" = "pop2010", "2020s" = "pop_now")
pop_bins <- purrr::map_dfr(names(pl_vintage), function(e) {
  col <- pl_vintage[[e]]
  places |>
    filter(!is.na(.data[[col]])) |>
    mutate(bin = cote_bin(.data[[col]])) |>
    group_by(bin) |>
    summarise(pop = sum(.data[[col]]), .groups = "drop") |>
    mutate(era = e, pop_share = pop / sum(pop))
})

# ================= Per sport: match, bins figure, county map =================
match_tbl_list  <- list()
effect_list     <- list()
county_rate_list <- list()

for (i in seq_len(nrow(sport_meta))) {
  s <- sport_meta$sport[i]; lab <- sport_meta$label[i]; src <- sport_meta$source_label[i]

  players <- read_parquet(sprintf("data/processed/players_%s.parquet", s)) |> us_filter()
  matched <- match_places(players, places)

  report <- matched |>
    filter(!is.na(era)) |>
    group_by(era) |>
    summarise(n = n(), matched = sum(match_tier != "unmatched"),
              match_rate = matched / n, .groups = "drop") |>
    mutate(sport = lab)
  match_tbl_list[[s]] <- report
  cat(sprintf("\n=== %s match report (US-born) ===\n", lab)); print(report)

  matched_ok <- matched |>
    filter(match_tier != "unmatched", !is.na(era)) |>
    vintage_pop()
  no_vintage_pop <- matched_ok |> summarise(share = mean(is.na(pop))) |> pull(share)

  player_bins <- matched_ok |>
    filter(!is.na(pop)) |>
    mutate(bin = cote_bin(pop)) |>
    count(era, bin, name = "players") |>
    group_by(era) |> mutate(player_share = players / sum(players)) |> ungroup()

  effect <- player_bins |>
    left_join(pop_bins, by = c("era", "bin")) |>
    mutate(rep_ratio = player_share / pop_share, sport = lab)
  effect_list[[s]] <- effect

  p1 <- ggplot(effect, aes(bin, rep_ratio, fill = era)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    scale_fill_manual(values = era_palette) +
    labs(title = sprintf("Where %s players come from, relative to where people live", lab),
         subtitle = "Representation ratio: share of players born in each place size ÷ share of population living there",
         x = "Birthplace population (Census places incl. CDPs)",
         y = "Representation ratio (1 = proportional)",
         fill = "Career-start era",
         caption = sprintf(
           "Data: %s + US Census (US-born players only). Population vintages 2000/2010/2023 by era.\n%.0f%% of matched players lacked a vintage population and are excluded. Places incl. incorporated cities/towns and CDPs.",
           src, 100 * no_vintage_pop)) +
    theme_minimal(base_size = 16) +
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 30, hjust = 1)) +
    caption_theme
  ggsave(sprintf("docs/figures/cote_bins_%s.png", s), p1, width = 12, height = 6.75, dpi = 320)

  # County map: hometown = birthplace for these sports (no HS data available).
  # Era filter keeps the per-capita denominator (2024 county pop) honest and
  # matches the NFL map's 1990+ window.
  pts_df <- matched |>
    filter(match_tier != "unmatched", !is.na(era), !is.na(lat), !is.na(lon))
  pts <- st_as_sf(pts_df, coords = c("lon", "lat"), crs = st_crs(counties_sf))
  hit <- st_join(pts, counties_sf["GEOID"], join = st_within)
  county_counts <- hit |>
    st_drop_geometry() |>
    filter(!is.na(GEOID)) |>
    count(county_fips = GEOID, name = "players")
  rates <- co_pop |>
    left_join(county_counts, by = "county_fips") |>
    mutate(players = coalesce(players, 0L), per_million = 1e6 * players / pop2024)
  county_rate_list[[s]] <- rates

  p_map <- plot_usmap(regions = "counties",
                      data = rates |> select(fips = county_fips, per_million),
                      values = "per_million", linewidth = 0) +
    scale_fill_viridis_c(option = "magma", direction = -1, trans = "sqrt",
                         na.value = "grey92",
                         name = paste0(lab, " players\nper 1M residents")) +
    labs(title = sprintf("Where %s players are from, per capita", lab),
         subtitle = "US-born players with career-start seasons 1990–2025, by birthplace county",
         caption = sprintf("Data: %s + US Census. Grey: no matched players.", src)) +
    theme(plot.title = element_text(size = 20, face = "bold"),
          plot.subtitle = element_text(size = 14),
          legend.position = "right",
          plot.background = element_rect(fill = "white", colour = NA)) +
    caption_theme
  ggsave(sprintf("docs/figures/county_map_%s.png", s), p_map, width = 12, height = 8, dpi = 320, bg = "white")

  cat(sprintf("wrote cote_bins_%s.png + county_map_%s.png\n", s, s))
}

# ================= NHL bins chart: pooled eras (small-sample call) =================
# NHL's US-born sample (n≈1.5k) spread across 4 eras x 8 bins leaves most
# individual era×bin cells under 30 players (23 of 32 cells, see the noisy-cell
# report below) — too thin to read era-by-era with a straight face. Pooling
# 1990s+2000s (both already share the pop2000 vintage, so pop_share is
# identical — no approximation needed there) and 2010s+2020s (pop2010 + pop_now
# averaged per bin, a modeling approximation, documented in the caption) drops
# noisy cells to 2 of 16. crosssport_bins.png keeps the unpooled 1990s/2020s
# cells for direct cross-sport comparability instead (with a caption caveat).
nhl_matched_ok <- match_places(
    read_parquet("data/processed/players_nhl.parquet") |> us_filter(),
    places) |>
  filter(match_tier != "unmatched", !is.na(era)) |>
  vintage_pop()

nhl_super <- nhl_matched_ok |>
  mutate(super_era = if_else(era %in% c("1990s", "2000s"), "1990s–2000s", "2010s–2020s"))

nhl_player_bins <- nhl_super |>
  filter(!is.na(pop)) |>
  mutate(bin = cote_bin(pop)) |>
  count(super_era, bin, name = "players") |>
  group_by(super_era) |> mutate(player_share = players / sum(players)) |> ungroup()

nhl_pop_bins <- pop_bins |>
  mutate(super_era = if_else(era %in% c("1990s", "2000s"), "1990s–2000s", "2010s–2020s")) |>
  group_by(super_era, bin) |>
  summarise(pop_share = mean(pop_share), .groups = "drop")

nhl_super_effect <- nhl_player_bins |>
  left_join(nhl_pop_bins, by = c("super_era", "bin")) |>
  mutate(rep_ratio = player_share / pop_share)

p_nhl_pooled <- ggplot(nhl_super_effect, aes(bin, rep_ratio, fill = super_era)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("1990s–2000s" = "#74A9CF", "2010s–2020s" = "#045A8D")) +
  labs(title = "Where NHL players come from, relative to where people live",
       subtitle = "Representation ratio: share of players born in each place size ÷ share of population living there",
       x = "Birthplace population (Census places incl. CDPs)",
       y = "Representation ratio (1 = proportional)",
       fill = "Career-start era",
       caption = "Data: NHL API + US Census (US-born players only). Eras pooled: 1990s+2000s, 2010s+2020s.\nNHL's small US-born sample (n≈1.5k) left most single-era×bin cells under 30 players; pooling stabilizes them. Places incl. incorporated cities/towns and CDPs.") +
  theme_minimal(base_size = 16) +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  caption_theme
ggsave("docs/figures/cote_bins_nhl.png", p_nhl_pooled, width = 12, height = 6.75, dpi = 320)
cat("wrote cote_bins_nhl.png (pooled eras, overwrites the per-era version above)\n")

# ================= Cross-sport match table =================
nfl_match <- read.csv("data/processed/match_report.csv", stringsAsFactors = FALSE) |>
  select(era, n, matched, match_rate) |> mutate(sport = "NFL")

multisport_match <- bind_rows(match_tbl_list) |>
  select(sport, era, n, matched, match_rate) |>
  bind_rows(nfl_match) |>
  mutate(sport = factor(sport, levels = c("NFL", "MLB", "NHL", "NBA"))) |>
  arrange(sport, era)
write.csv(multisport_match, "data/processed/multisport_match.csv", row.names = FALSE)
cat("\n=== multisport match rates (all sports) ===\n")
print(multisport_match, n = 30)

# ================= RAE per sport: mlb, nhl, nba (all birth countries) =================
days_in_month <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
expected <- tibble(month = 1:12, expected_share = days_in_month / sum(days_in_month))

rae_tbl_list <- list()
for (i in seq_len(nrow(sport_meta))) {
  s <- sport_meta$sport[i]; lab <- sport_meta$label[i]; src <- sport_meta$source_label[i]

  players_all <- read_parquet(sprintf("data/processed/players_%s.parquet", s)) |>
    filter(!is.na(birth_date)) |>
    mutate(birth_date = as.Date(birth_date))

  rae <- players_all |>
    mutate(era2 = coalesce(era, "pre-1990"), month = month(birth_date)) |>
    count(era2, month) |>
    group_by(era2) |> mutate(share = n / sum(n)) |> ungroup() |>
    left_join(expected, by = "month") |>
    mutate(ratio = share / expected_share)
  rae_tbl_list[[s]] <- rae |> mutate(sport = lab)

  extra_caption <- ""
  if (s == "nhl") {
    canada_share <- mean(players_all$birth_country == "CAN")
    extra_caption <- sprintf("\nAll birth countries included — %.0f%% of these players are Canadian-born.",
                             100 * canada_share)
  } else if (s == "mlb") {
    intl_share <- mean(players_all$birth_country != "USA")
    extra_caption <- sprintf("\nAll birth countries included — %.0f%% born outside the US (mostly Latin America).",
                             100 * intl_share)
  }

  p_rae <- rae |>
    mutate(month_lab = factor(month.abb[month], levels = month.abb),
           era2 = factor(era2, levels = c("pre-1990", "1990s", "2000s", "2010s", "2020s"))) |>
    ggplot(aes(month_lab, ratio, group = era2)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
    geom_col(fill = "#2C7FB8") +
    facet_wrap(~era2, nrow = 1) +
    labs(title = sprintf("%s births by month vs. expected", lab),
         subtitle = "Ratio of player birth-month share to days-adjusted uniform baseline",
         x = NULL, y = "Observed / expected",
         caption = paste0(sprintf("Data: %s. Baseline: days-in-month adjusted uniform.", src),
                          extra_caption)) +
    theme_minimal(base_size = 16) +
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.65))) +
    caption_theme

  ggsave(sprintf("docs/figures/rae_%s.png", s), p_rae, width = 12, height = 6.75, dpi = 320)
  cat(sprintf("wrote rae_%s.png\n", s))
}

# ================= Cross-sport comparison (the payoff figure) =================
nfl_effect_full <- readRDS("data/processed/effect_tables.rds")$effect |> mutate(sport = "NFL")

cross_effect <- bind_rows(
  nfl_effect_full |> select(era, bin, rep_ratio, sport),
  effect_list$mlb |> select(era, bin, rep_ratio, sport),
  effect_list$nhl |> select(era, bin, rep_ratio, sport),
  effect_list$nba |> select(era, bin, rep_ratio, sport)
) |>
  filter(era %in% c("1990s", "2020s")) |>
  mutate(sport = factor(sport, levels = c("NFL", "MLB", "NHL", "NBA")))

p_cross <- ggplot(cross_effect, aes(bin, rep_ratio, color = sport, group = sport)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = sport_palette) +
  facet_wrap(~era) +
  labs(title = "The small-town effect across four sports",
       subtitle = "Representation ratio by birthplace population size: 1990s vs. 2020s career-start cohorts",
       x = "Birthplace population (Census places incl. CDPs)",
       y = "Representation ratio (1 = proportional)",
       color = "Sport",
       caption = "Data: nflverse+ESPN, Lahman, NHL API, Basketball-Reference + US Census (US-born players only).\nCohort edges differ by up to one season across sports (source conventions).\nNHL's sample is small (n≈1.5k); most NHL cells here have under 30 players — read that line with caution.") +
  theme_minimal(base_size = 16) +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  caption_theme
ggsave("docs/figures/crosssport_bins.png", p_cross, width = 12, height = 6.75, dpi = 320)
cat("wrote crosssport_bins.png\n")

# ================= Save all tables =================
saveRDS(list(
  match      = multisport_match,
  bins       = list(nfl = nfl_effect_full, mlb = effect_list$mlb,
                    nhl = effect_list$nhl, nba = effect_list$nba,
                    nhl_pooled = nhl_super_effect),
  county     = county_rate_list,
  rae        = rae_tbl_list,
  crosssport = cross_effect
), "data/processed/multisport_tables.rds")
cat("\nwrote data/processed/multisport_tables.rds\n")

# ================= Sanity interrogation =================
cat("\n=== 250k-500k and 500k+ rep_ratio by sport & era ===\n")
bind_rows(
  nfl_effect_full |> select(sport, era, bin, rep_ratio),
  effect_list$mlb |> select(sport, era, bin, rep_ratio),
  effect_list$nhl |> select(sport, era, bin, rep_ratio),
  effect_list$nba |> select(sport, era, bin, rep_ratio)
) |>
  filter(bin %in% c("250k–500k", "500k+")) |>
  mutate(sport = factor(sport, levels = c("NFL", "MLB", "NHL", "NBA"))) |>
  pivot_wider(names_from = bin, values_from = rep_ratio) |>
  arrange(sport, era) |>
  print(n = 20)

cat("\n=== noisy era×bin cells (players < 30) ===\n")
bind_rows(
  nfl_effect_full |> select(sport, era, bin, players),
  effect_list$mlb |> select(sport, era, bin, players),
  effect_list$nhl |> select(sport, era, bin, players),
  effect_list$nba |> select(sport, era, bin, players)
) |>
  filter(players < 30) |>
  arrange(sport, era, bin) |>
  print(n = 100)
