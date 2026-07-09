suppressMessages({
  library(dplyr); library(arrow); library(ggplot2); library(tidyr)
  library(purrr); library(sf); library(usmap); library(lubridate); library(tibble)
})
source("R/lib/bins.R")
source("R/lib/places.R")
source("R/lib/theme_hometown.R")

dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)

places <- read_parquet("data/processed/census_places.parquet")
co_pop <- read_parquet("data/processed/census_counties.parquet")
counties_sf <- st_read(list.files("data/raw/census/cb_2023_us_county_500k",
                                  pattern = "[.]shp$", full.names = TRUE),
                       quiet = TRUE)

# Legacy theme kept ONLY for the superseded rae_{sport}.png loop below
# (replaced on the page by R/17 calendar heatmaps; code left as is).
caption_theme <- theme(plot.caption = element_text(hjust = 0, size = rel(0.7)))

# Axis labels: bin names carry en dashes in the data; render them as hyphens
# on the axes without touching the underlying tables.
label_bins <- function(x) gsub("–", "-", x)

sport_meta <- tribble(
  ~sport,  ~label, ~source_label,
  ~bins_title,
  ~map_title,
  "mlb",   "MLB",  "Lahman database",
  "MLB players come disproportionately from mid-size cities in every era",
  "Per capita, MLB talent flows from the South and California",
  "nhl",   "NHL",  "NHL API",
  "US-born NHL players cluster in mid-size cities",
  "US hockey is northern: Minnesota towers over the rest per capita",
  "nba",   "NBA",  "Basketball-Reference",
  "The NBA is the big-city league: places over 250k produce twice their share",
  "NBA talent per capita peaks in big cities like DC, Baltimore, and New Orleans"
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

  # In-panel era labels were tried first (spec rule 8): four 5-char labels over
  # dodged bars 0.2 x-units apart physically collide, so this keeps the
  # sanctioned one-row bottom legend for the 4-era dodge.
  n_thin_bins <- sum(effect$players < 30)
  thin_bins_note <- if (n_thin_bins > 0) {
    sprintf(" %d era x bin cells have under 30 players; read thin bars cautiously.",
            n_thin_bins)
  } else ""

  p1 <- ggplot(effect, aes(bin, rep_ratio, fill = era)) +
    geom_baseline(1) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    scale_fill_manual(values = pal_era) +
    scale_x_discrete(labels = label_bins) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(title = sport_meta$bins_title[i],
         subtitle = "Representation ratio: share of players born in each place size / share of population living there. US-born players, career starts 1990-2025.",
         x = "Birthplace population (Census places incl. CDPs)",
         y = "Representation ratio (1 = proportional)",
         caption = fig_caption(
           paste0(src, " + US Census"),
           "US-born players, career starts 1990-2025; population vintages 2000/2010/2023 by era.",
           sprintf("\n%.0f%% of matched players lacked a vintage population and are excluded. Places include incorporated cities, towns, and CDPs.%s",
                   100 * no_vintage_pop, thin_bins_note))) +
    theme_hometown() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.key.size = unit(0.7, "lines"))
  save_fig(sprintf("docs/figures/cote_bins_%s.png", s), p1)

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
    labs(title = sport_meta$map_title[i],
         subtitle = "US-born players with career-start seasons 1990-2025, by birthplace county",
         caption = fig_caption(paste0(src, " + US Census"),
                               "US-born players, career starts 1990-2025.",
                               "Grey: no matched players.")) +
    theme_hometown_legend(grid = "none", position = "inside") +
    # coord_sf draws graticules with the parent panel.grid element, which the
    # theme's .x/.y blanks do not reach; erase it outright (spec: no map grid).
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          legend.position.inside = c(0.9, 0.3),
          legend.key.height = unit(0.9, "lines"),
          legend.key.width = unit(0.45, "lines"))
  save_fig(sprintf("docs/figures/county_map_%s.png", s), p_map, w = 12, h = 8)
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
  mutate(super_era = if_else(era %in% c("1990s", "2000s"), "1990s-2000s", "2010s-2020s"))

nhl_player_bins <- nhl_super |>
  filter(!is.na(pop)) |>
  mutate(bin = cote_bin(pop)) |>
  count(super_era, bin, name = "players") |>
  group_by(super_era) |> mutate(player_share = players / sum(players)) |> ungroup()

nhl_pop_bins <- pop_bins |>
  mutate(super_era = if_else(era %in% c("1990s", "2000s"), "1990s-2000s", "2010s-2020s")) |>
  group_by(super_era, bin) |>
  summarise(pop_share = mean(pop_share), .groups = "drop")

nhl_super_effect <- nhl_player_bins |>
  left_join(nhl_pop_bins, by = c("super_era", "bin")) |>
  mutate(rep_ratio = player_share / pop_share)

# Two series only, so the era key lives in the panel (spec rule 8): each pooled
# era labeled above its own bar in the tallest (250k-500k) group, staggered
# vertically so the long labels cannot collide. Text inks: pooled light hue
# darkened for contrast (#74A9CF -> #517690), dark hue used as is.
n_thin_pooled <- sum(nhl_super_effect$players < 30)
nhl_pool_lab <- nhl_super_effect |>
  filter(grepl("^250k", bin)) |>
  mutate(x = as.integer(bin) + if_else(super_era == "1990s-2000s", -0.2, 0.2),
         ink = if_else(super_era == "1990s-2000s", "#517690", "#045A8D"))

p_nhl_pooled <- ggplot(nhl_super_effect, aes(bin, rep_ratio, fill = super_era)) +
  geom_baseline(1) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = pal_era_pooled) +
  scale_x_discrete(labels = label_bins) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  geom_text(data = nhl_pool_lab,
            aes(x = x, y = rep_ratio + 0.05, label = super_era, colour = ink),
            inherit.aes = FALSE, vjust = 0, size = 3, fontface = "bold") +
  scale_colour_identity() +
  labs(title = "US-born NHL players cluster in mid-size cities; the smallest towns are catching up",
       subtitle = "Representation ratio: share of players born in each place size / share of population living there. US-born players, career starts 1990-2025, eras pooled.",
       x = "Birthplace population (Census places incl. CDPs)",
       y = "Representation ratio (1 = proportional)",
       caption = fig_caption(
         "NHL API + US Census",
         "US-born players, career starts 1990-2025; population vintages 2000/2010/2023 by era.",
         sprintf("\nEras pooled (1990s+2000s, 2010s+2020s): the small US-born sample (about 1.5k players) left most single-era cells under 30 players.\n%d pooled era x bin cells still have under 30 players. Places include incorporated cities, towns, and CDPs.",
                 n_thin_pooled))) +
  theme_hometown() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
save_fig("docs/figures/cote_bins_nhl.png", p_nhl_pooled)
cat("cote_bins_nhl.png is the pooled-era version (overwrites the per-era one above)\n")

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

# End-of-line sport labels in each facet, legend erased. Deterministic nudges:
# in the 2020s facet MLB (0.90) and NFL (0.87) end 0.03 apart and NHL (0.75)
# sits just below, so they fan apart; MLB's 1990s end (1.02) moves up so the
# dashed baseline at 1 does not strike through its label.
cross_lab <- cross_effect |>
  filter(bin == "500k+") |>
  mutate(label_y = rep_ratio + case_when(
    era == "1990s" & sport == "MLB" ~ 0.05,
    era == "2020s" & sport == "MLB" ~ 0.04,
    era == "2020s" & sport == "NFL" ~ -0.045,
    era == "2020s" & sport == "NHL" ~ -0.085,
    TRUE ~ 0))

p_cross <- ggplot(cross_effect, aes(bin, rep_ratio, colour = sport, group = sport)) +
  geom_baseline(1) +
  geom_line(aes(alpha = sport), linewidth = 1) +
  geom_point(aes(alpha = sport), size = 2) +
  scale_color_manual(values = pal_sport) +
  scale_alpha_manual(values = c(NFL = 1, MLB = 1, NHL = 0.65, NBA = 1)) +
  scale_x_discrete(labels = label_bins) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  facet_wrap(~era) +
  direct_label(cross_lab,
               aes(x = bin, y = label_y, label = sport, colour = sport),
               nudge_x = 0.18, size = 3.2, inherit.aes = FALSE) +
  coord_cartesian(clip = "off") +
  labs(title = "Mid-size cities overproduce players in all four sports; basketball is the most urban",
       subtitle = "Representation ratio: share of players born in each place size / share of population living there. US-born players, 1990s vs 2020s career-start cohorts.",
       x = "Birthplace population (Census places incl. CDPs)",
       y = "Representation ratio (1 = proportional)",
       caption = fig_caption(
         "nflverse+ESPN, Lahman, NHL API, Basketball-Reference + US Census",
         "US-born players only; cohort edges differ by up to one season across sports (source conventions).",
         "\nNHL's US-born sample is small (about 1.5k; most era x bin cells under 30 players), so its line is drawn lighter: read it with caution.")) +
  theme_hometown() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.spacing = unit(2.4, "lines"),
        plot.margin = margin(10, 46, 8, 10))
save_fig("docs/figures/crosssport_bins.png", p_cross)

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
