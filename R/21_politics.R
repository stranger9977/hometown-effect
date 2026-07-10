# =============================================================================
# 21_politics.R -- political geography of each league's talent base.
#
# County-level presidential results (tonmcg US_County_Level_Election_Results,
# 2020 as the primary map, 2008 for robustness) joined to player hometown
# counties via point-in-county. NFL hometown = high school where known, else
# birthplace; MLB/NHL/NBA = birthplace (US-born only). County lean = two-party
# GOP vote share; players are scored on a FIXED map (the 2020 vote), so era
# differences reflect WHERE players come from, not vote swings.
#
# Outputs:
#   data/processed/politics_by_sport.csv  sport x era table + baseline row
#   docs/figures/politics_leagues.png     per-capita rate by county lean bucket
#   docs/figures/politics_nfl_shift.png   NFL hometown-county lean by era
#
# Run from repo root: Rscript R/21_politics.R
# =============================================================================

suppressMessages({
  library(dplyr); library(tidyr); library(readr); library(stringr)
  library(arrow); library(sf); library(ggplot2)
})
source("R/lib/theme_hometown.R")
source("R/lib/places.R")

# ---------------------------------------------------------------------------
# 1. Player -> county assignment (point-in-county, cb_2023 county boundaries)
# ---------------------------------------------------------------------------
counties_sf <- st_read(
  list.files("data/raw/census/cb_2023_us_county_500k",
             pattern = "[.]shp$", full.names = TRUE),
  quiet = TRUE)

point_in_county <- function(df) {
  pts <- st_as_sf(df, coords = c("lon", "lat"), crs = st_crs(counties_sf),
                  remove = FALSE)
  st_join(pts, counties_sf["GEOID"], join = st_within) |>
    st_drop_geometry() |>
    rename(county_fips = GEOID)
}

nfl <- read_parquet("data/processed/hometown.parquet") |>
  filter(!is.na(hometown_lat), !is.na(hometown_lon), !is.na(era)) |>
  transmute(sport = "NFL", player = display_name, era,
            lat = hometown_lat, lon = hometown_lon) |>
  point_in_county()

places <- read_parquet("data/processed/census_places.parquet")
other <- lapply(c("mlb", "nhl", "nba"), function(s) {
  read_parquet(sprintf("data/processed/players_%s.parquet", s)) |>
    filter(birth_country == "USA", !is.na(era)) |>
    match_places(places) |>
    filter(!is.na(lat), !is.na(lon)) |>
    transmute(sport = toupper(s), player = player_name, era, lat, lon) |>
    point_in_county()
}) |> bind_rows()

players <- bind_rows(nfl, other)
cat("== point-in-county coverage ==\n")
players |> count(sport, matched = !is.na(county_fips)) |> print()
players <- players |> filter(!is.na(county_fips))

# ---------------------------------------------------------------------------
# 2. County election lean: two-party GOP share, 2020 primary map + 2008
#    robustness map. Connecticut reports 2020 on legacy counties but the
#    cb_2023 boundaries use planning regions, so those nine regions fall back
#    to their 2024 result (reported on planning regions). Alaska reports on
#    election districts that never match county FIPS and is dropped (a handful
#    of players).
# ---------------------------------------------------------------------------
e2020 <- read_csv("data/raw/elections/2020_US_County_Level_Presidential_Results.csv",
                  show_col_types = FALSE) |>
  transmute(county_fips,
            gop2020 = votes_gop, dem2020 = votes_dem)
e2024 <- read_csv("data/raw/elections/2024_US_County_Level_Presidential_Results.csv",
                  show_col_types = FALSE) |>
  transmute(county_fips,
            gop2024 = votes_gop, dem2024 = votes_dem)
e2008 <- read_csv("data/raw/elections/US_County_Level_Presidential_Results_08-16.csv",
                  show_col_types = FALSE) |>
  transmute(county_fips = str_pad(fips_code, 5, pad = "0"),
            gop2008 = gop_2008, dem2008 = dem_2008)

co <- read_parquet("data/processed/census_counties.parquet") |>
  left_join(e2020, by = "county_fips") |>
  left_join(e2024, by = "county_fips") |>
  left_join(e2008, by = "county_fips") |>
  mutate(pg2020 = gop2020 / (gop2020 + dem2020),
         pg2024 = gop2024 / (gop2024 + dem2024),
         pg2008 = gop2008 / (gop2008 + dem2008),
         lean2020 = coalesce(pg2020, pg2024)) |>   # CT planning-region fallback
  filter(!is.na(lean2020))

cat(sprintf("\ncounties with 2020 lean: %d (CT 2024 fallback: %d, with 2008: %d)\n",
            nrow(co), sum(is.na(co$pg2020)), sum(!is.na(co$pg2008))))

# Lean buckets: county two-party GOP share <35 / 35-50 / 50-65 / >65 percent.
bucket_breaks <- c(0, .35, .50, .65, 1)
bucket_keys   <- c("lt35", "b35_50", "b50_65", "gt65")
co <- co |>
  mutate(bucket = cut(lean2020, bucket_breaks, labels = bucket_keys))
bucket_pop <- co |>
  group_by(bucket) |>
  summarise(n_counties = n(), pop = sum(pop2024), .groups = "drop")

pl <- players |>
  inner_join(co |> select(county_fips, lean2020, pg2008, pop2024, bucket),
             by = "county_fips")
cat(sprintf("players joined to a county lean: %d of %d\n\n",
            nrow(pl), nrow(players)))

# Population baselines (2020 two-party GOP share, 2024 county populations).
natl_lean <- with(co, sum(lean2020 * pop2024) / sum(pop2024))
natl_red  <- with(co, sum(pop2024[lean2020 > .5]) / sum(pop2024))
co08      <- co |> filter(!is.na(pg2008))
natl_lean08 <- with(co08, sum(pg2008 * pop2024) / sum(pop2024))
cat(sprintf("population baseline: mean county GOP share %.3f | share in red counties %.3f | 2008 map %.3f\n\n",
            natl_lean, natl_red, natl_lean08))

# ---------------------------------------------------------------------------
# 3. Deliverable (a): sport x era table (+ pooled rows + baseline row)
# ---------------------------------------------------------------------------
summarise_block <- function(df) {
  base <- df |>
    summarise(n_players = n(),
              mean_gop_share = mean(lean2020),
              share_red_county = mean(lean2020 > .5),
              .groups = "drop")
  buckets <- df |>
    count(bucket, .drop = FALSE, name = "players") |>
    left_join(bucket_pop, by = "bucket") |>
    mutate(per_million = 1e6 * players / pop)
  bind_cols(base,
            buckets |> select(bucket, players) |>
              pivot_wider(names_from = bucket, values_from = players,
                          names_prefix = "n_"),
            buckets |> select(bucket, per_million) |>
              pivot_wider(names_from = bucket, values_from = per_million,
                          names_prefix = "per_million_"))
}

tab <- bind_rows(
  pl |> group_by(sport, era) |> group_modify(~summarise_block(.x)) |> ungroup(),
  pl |> group_by(sport) |> group_modify(~summarise_block(.x)) |>
    ungroup() |> mutate(era = "all")
) |>
  mutate(small_cell = if_any(starts_with("n_"), ~.x < 30))

baseline_row <- tibble(
  sport = "US population", era = "all",
  n_players = NA_integer_,
  mean_gop_share = natl_lean,
  share_red_county = natl_red) |>
  bind_cols(bucket_pop |> select(bucket, pop) |>
              pivot_wider(names_from = bucket, values_from = pop,
                          names_prefix = "n_")) |>
  mutate(across(starts_with("per_million"), ~NA_real_), small_cell = FALSE)
# note: in the baseline row, n_* columns hold county POPULATION per bucket.

tab_out <- bind_rows(tab, baseline_row) |>
  arrange(factor(sport, c("NFL", "MLB", "NHL", "NBA", "US population")),
          factor(era, c("1990s", "2000s", "2010s", "2020s", "all"))) |>
  mutate(across(where(is.numeric), ~round(.x, 4)))
write_csv(tab_out, "data/processed/politics_by_sport.csv")
cat("wrote data/processed/politics_by_sport.csv\n")
cat("\nsmall cells (any bucket n < 30):\n")
tab |> filter(small_cell) |> select(sport, era, starts_with("n_")) |>
  as.data.frame() |> print()

# ---------------------------------------------------------------------------
# 4. Deliverable (b): per-capita rate by lean bucket, one panel per league
# ---------------------------------------------------------------------------
bucket_labs <- c(lt35   = "Deep\nblue\n<35%",
                 b35_50 = "Lean\nblue\n35-50%",
                 b50_65 = "Lean\nred\n50-65%",
                 gt65   = "Deep\nred\n>65%")

fig1 <- pl |>
  count(sport, bucket, .drop = FALSE, name = "players") |>
  left_join(bucket_pop, by = "bucket") |>
  mutate(per_million = 1e6 * players / pop,
         sport = factor(sport, c("NFL", "MLB", "NHL", "NBA")))

league_avg <- pl |>
  count(sport, name = "players") |>
  mutate(avg = 1e6 * players / sum(bucket_pop$pop),
         sport = factor(sport, c("NFL", "MLB", "NHL", "NBA")),
         label = ifelse(sport == "NFL", "league average:\nan even draw from\nthe US population", ""))

p1 <- ggplot(fig1, aes(bucket, per_million)) +
  geom_col(aes(fill = sport), width = 0.72) +
  geom_hline(data = league_avg, aes(yintercept = avg),
             linetype = "dashed", colour = ink_baseline, linewidth = 0.45) +
  geom_text(aes(label = sprintf("%.1f", per_million)),
            vjust = -0.45, size = 3.3, fontface = "bold", colour = ink_body) +
  geom_text(data = league_avg, aes(x = 4.55, y = avg, label = label),
            hjust = 1, vjust = -0.25, size = 2.9, lineheight = 0.95,
            colour = ink_baseline, fontface = "italic") +
  scale_fill_manual(values = pal_sport) +
  scale_x_discrete(labels = bucket_labs) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  facet_wrap(~sport, nrow = 1, scales = "free_y") +
  labs(title = "Every league's talent base tilts toward blue-county America",
       subtitle = "Players per one million residents, by the 2020 presidential lean of the player's hometown county (two-party GOP vote\nshare). All eras pooled, 1990s-2020s. If talent were drawn evenly from the population, every bar would sit on the dashed line.",
       x = NULL, y = "Players per 1M residents",
       caption = paste0(
         "Data: nflverse, ESPN, MLB/NHL/NBA league sources, US Census, tonmcg county-level 2020 presidential results. US-born players;\n",
         "NFL hometown = high school where known, else birthplace; other leagues birthplace. County lean describes where players grew up,\n",
         "not how any player votes. Bucket populations (2024): 78M / 125M / 76M / 61M residents.")) +
  theme_hometown(grid = "none") +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(lineheight = 1.05, size = rel(0.72)))
save_fig("docs/figures/politics_leagues.png", p1, w = 12, h = 5.6)

# ---------------------------------------------------------------------------
# 5. Deliverable (c): NFL hometown-county lean by era, fixed 2020 map
# ---------------------------------------------------------------------------
nfl_shift <- pl |>
  filter(sport == "NFL") |>
  group_by(era) |>
  summarise(n = n(), mean_gop = mean(lean2020), .groups = "drop") |>
  mutate(era_n = as.integer(factor(era, c("1990s", "2000s", "2010s", "2020s"))))

nfl_shift08 <- pl |>
  filter(sport == "NFL", !is.na(pg2008)) |>
  summarise(first = mean(pg2008[era == "1990s"]),
            last  = mean(pg2008[era == "2020s"]))
cat(sprintf("\nNFL robustness, 2008 map: 1990s %.3f -> 2020s %.3f (baseline %.3f)\n",
            nfl_shift08$first, nfl_shift08$last, natl_lean08))

p2 <- ggplot(nfl_shift, aes(era_n, mean_gop)) +
  geom_hline(yintercept = natl_lean, linetype = "dashed",
             colour = ink_baseline, linewidth = 0.45) +
  annotate("text", x = 4.42, y = natl_lean,
           label = sprintf("US population baseline: %.1f%%", 100 * natl_lean),
           hjust = 1, vjust = -0.6, size = 3.2, colour = ink_baseline,
           fontface = "italic") +
  geom_line(colour = pal_sport[["NFL"]], linewidth = 1.2) +
  geom_point(colour = pal_sport[["NFL"]], size = 3.2) +
  geom_text(aes(label = sprintf("%.1f%%", 100 * mean_gop)),
            vjust = 1.9, size = 3.6, fontface = "bold",
            colour = pal_sport[["NFL"]]) +
  scale_x_continuous(breaks = 1:4, labels = c("1990s", "2000s", "2010s", "2020s"),
                     limits = c(0.8, 4.45)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                     limits = c(0.42, 0.49)) +
  labs(title = "NFL hometowns are drifting toward redder counties, but still sit bluer than America",
       subtitle = "Mean 2020 two-party GOP vote share of NFL players' hometown counties, by rookie era. The county map is fixed at the\n2020 vote, so the drift reflects where players grew up, not vote swings.",
       x = NULL, y = "Mean hometown-county GOP share (2020 vote)",
       caption = paste0(
         "Data: nflverse, ESPN, US Census, tonmcg county-level presidential results. US-born players; hometown = high school where\n",
         "known, else birthplace. Robustness: scoring the same counties on the 2008 map shows the same drift (43.4% to 46.5%,\n",
         "baseline 46.9%). County lean describes where players grew up, not how any player votes.")) +
  theme_hometown(grid = "y")
save_fig("docs/figures/politics_nfl_shift.png", p2, w = 9, h = 5.6)

# ---------------------------------------------------------------------------
# 6. Console QA: the numbers the page cites
# ---------------------------------------------------------------------------
cat("\n== pooled league means (2020 map) ==\n")
tab |> filter(era == "all") |>
  select(sport, n_players, mean_gop_share, share_red_county) |>
  arrange(mean_gop_share) |> as.data.frame() |> print(digits = 3)

cat("\n== deep-blue vs deep-red per-capita ratio (pooled) ==\n")
tab |> filter(era == "all") |>
  transmute(sport, ratio = per_million_lt35 / per_million_gt65) |>
  as.data.frame() |> print(digits = 3)

cat("\n== NFL era drift (2020 map) ==\n")
nfl_shift |> select(era, n, mean_gop) |> as.data.frame() |> print(digits = 4)
