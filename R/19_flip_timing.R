# =============================================================================
# 19_flip_timing.R -- date the suburban flip precisely.
#
# The published pooled figure showed the 15-45 mi ring as the NFL's best band;
# this script asks WHEN that happened. Representation ratio by distance band
# for all four eras (1990s/2000s/2010s/2020s), NFL for the figure and MLB for
# the text. Baseline = US census-place population weighted by pop_now, banded
# by distance to the nearest 1M+ CBSA anchor, identical to the pooled figure
# (base_share published in metro_distance_bands.csv, built by
# output/exploration/metro_cbsa_02_analysis.R from census_places.parquet).
#
# Inputs:  data/processed/metro_distance.parquet
#          data/processed/metro_distance_bands.csv   (baseline band shares)
# Outputs: data/processed/flip_timing.csv
#          docs/figures/suburban_flip.png            (replaces two-era dumbbell)
# =============================================================================
suppressMessages({
  library(arrow); library(dplyr); library(tidyr); library(readr); library(ggplot2)
})
source("R/lib/theme_hometown.R")

md <- read_parquet("data/processed/metro_distance.parquet")

# ---- baseline population share per band -------------------------------------
# Constant across sport and era (it is a property of where people live, not of
# who plays). Read from the published band table so this figure and the pooled
# one share one baseline to the last decimal.
base <- read_csv("data/processed/metro_distance_bands.csv",
                 show_col_types = FALSE) |>
  filter(sport == "NFL", era == "pooled", cut == "dist_band") |>
  select(group, base_share)
stopifnot(nrow(base) == 4, abs(sum(base$base_share) - 1) < 1e-9)

band_levels <- c("0-15mi", "15-45mi", "45-100mi", "100mi+")
band_labels <- c("Urban core", "Suburban ring", "Exurban", "Remote")

# ---- four-era representation ratios, NFL + MLB ------------------------------
flip <- md |>
  filter(sport %in% c("NFL", "MLB"), !is.na(era)) |>
  count(sport, era, group = as.character(band), name = "n_players") |>
  group_by(sport, era) |>
  mutate(player_share = n_players / sum(n_players)) |>
  ungroup() |>
  left_join(base, by = "group") |>
  mutate(rep_ratio  = player_share / base_share,
         small_cell = n_players < 30,
         band       = factor(group, band_levels, band_labels)) |>
  arrange(sport, band, era)

write_csv(flip |>
            select(sport, era, band = group, band_label = band, n_players,
                   player_share, base_share, rep_ratio, small_cell),
          "data/processed/flip_timing.csv")
cat("wrote data/processed/flip_timing.csv (", nrow(flip), "rows )\n\n")

for (s in c("NFL", "MLB")) {
  cat("==", s, "representation ratio by band x era ==\n")
  print(flip |> filter(sport == s) |>
          mutate(rep_ratio = round(rep_ratio, 3)) |>
          select(band, era, n_players, rep_ratio) |>
          pivot_wider(names_from = era,
                      values_from = c(n_players, rep_ratio)) |>
          as.data.frame())
  cat("\n")
}
if (any(flip$small_cell)) {
  cat("WARNING: cells with n < 30:\n")
  print(flip |> filter(small_cell) |> as.data.frame())
} else cat("no cells with n < 30 (min cell n =", min(flip$n_players), ")\n")

# ---- figure: NFL four-era lines, suburban ring as the accent ----------------
nfl <- flip |>
  filter(sport == "NFL") |>
  mutate(era_n = as.integer(factor(era, c("1990s", "2000s", "2010s", "2020s"))))

# Greys recede, the accent carries the story; direct labels name every line.
pal_band <- c("Urban core"    = "grey62",
              "Suburban ring" = "#0072B2",
              "Exurban"       = "grey76",
              "Remote"        = "grey45")
lab_band <- c("Urban core"    = "grey45",
              "Suburban ring" = "#0072B2",
              "Exurban"       = "grey55",
              "Remote"        = "grey30")

ends <- nfl |>
  filter(era == "2020s") |>
  mutate(lab = sprintf("%s %.2f", band, rep_ratio))

p <- ggplot(nfl, aes(era_n, rep_ratio, colour = band, group = band)) +
  geom_baseline(1) +
  geom_line(aes(linewidth = band)) +
  geom_point(aes(size = band)) +
  geom_text(data = ends, aes(label = lab),
            colour = lab_band[as.character(ends$band)],
            hjust = 0, nudge_x = 0.12, size = 4.2, fontface = "bold") +
  scale_colour_manual(values = pal_band, guide = "none") +
  scale_linewidth_manual(values = c("Urban core" = 0.9, "Suburban ring" = 2,
                                    "Exurban" = 0.9, "Remote" = 0.9),
                         guide = "none") +
  scale_size_manual(values = c("Urban core" = 2.2, "Suburban ring" = 3.4,
                               "Exurban" = 2.2, "Remote" = 2.2),
                    guide = "none") +
  scale_x_continuous(breaks = 1:4,
                     labels = c("1990s", "2000s", "2010s", "2020s"),
                     expand = expansion(mult = c(0.04, 0.02))) +
  scale_y_continuous(limits = c(0.55, 1.3),
                     breaks = seq(0.6, 1.2, 0.2)) +
  coord_cartesian(clip = "off") +
  labs(title = "The flip happened in the 2010s: the suburban ring went from the NFL's worst band to its best",
       subtitle = paste0(
         "NFL representation ratio by hometown distance to the nearest 1M+ metro, by rookie-cohort era\n",
         "Bands: urban core 0-15 mi, suburban ring 15-45 mi, exurban 45-100 mi, remote 100+ mi"),
       x = NULL, y = "Representation ratio (1 = proportional)",
       caption = paste0(
         "Data: nflverse+ESPN, Census CBSA delineation + places. US-born players; hometown = high school where known, else birthplace.\n",
         "Distance from hometown place centroid to the nearest principal city of a metro area (CBSA) of 1M+ people. Baseline = share of\n",
         "US census-place population living in each band, so a ratio of 1 means players come from a band in proportion to who lives there.")) +
  theme_hometown() +
  theme(plot.margin = margin(10, 130, 8, 10))

save_fig("docs/figures/suburban_flip.png", p)
