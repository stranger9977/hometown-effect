suppressMessages({
  library(dplyr); library(arrow); library(ggplot2); library(tidyr)
  library(flexplot)
})
source("R/lib/theme_hometown.R")
cat(sprintf("flexplot %s loaded for exploration.\n", packageVersion("flexplot")))

dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)

matched <- read_parquet("data/processed/birthplace_matched.parquet") |>
  filter(match_tier != "unmatched", !is.na(era))
places <- read_parquet("data/processed/census_places.parquet")

stopifnot(all(places$income1999 >= 0, na.rm = TRUE))
stopifnot(all(places$income_now  >= 0, na.rm = TRUE))

era_levels <- c("1990s", "2000s", "2010s", "2020s")

# ---------------------------------------------------------------------------
# Population-weighted quantile cutpoints (same machinery as R/10_income.R).
# Income only has two vintages (2000 SF3 "1999", ACS 2023 "now"); population
# has three (2000/2010/now), same era->vintage map used throughout the repo
# (R/lib/bins.R vintage_pop, R/08). 2010s income is approximated by ACS 2023
# because no 2010-vintage income column exists -- documented in the caption.
# ---------------------------------------------------------------------------
wq <- function(x, pop) {
  ok <- !is.na(x) & !is.na(pop) & pop > 0
  o <- order(x[ok])
  cw <- cumsum(as.numeric(pop[ok][o])) / sum(as.numeric(pop[ok][o]))
  list(x = x[ok][o], cw = cw)
}
qcuts <- function(w, probs) sapply(probs, function(q) w$x[which.max(w$cw >= q)])

cut_income_1999 <- qcuts(wq(places$income1999, places$pop2000), c(.25, .5, .75))
cut_income_now  <- qcuts(wq(places$income_now,  places$pop_now), c(.25, .5, .75))

places_d <- places |>
  mutate(dens2000 = pop2000 / aland_sqmi,
         dens2010 = pop2010 / aland_sqmi,
         dens_now = pop_now / aland_sqmi)
cut_dens_2000 <- qcuts(wq(places_d$dens2000, places_d$pop2000), c(1/3, 2/3))
cut_dens_2010 <- qcuts(wq(places_d$dens2010, places_d$pop2010), c(1/3, 2/3))
cut_dens_now  <- qcuts(wq(places_d$dens_now,  places_d$pop_now), c(1/3, 2/3))

band_labels <- c("Low density", "Mid density", "High density")

# Player-level table: era-appropriate income + density, quartile/tercile bins.
player_df <- matched |>
  mutate(
    income  = case_when(era %in% c("1990s", "2000s") ~ matched_income1999,
                         TRUE ~ matched_income_now),
    pop     = case_when(era %in% c("1990s", "2000s") ~ matched_pop2000,
                         era == "2010s" ~ matched_pop2010,
                         era == "2020s" ~ matched_pop_now),
    density = pop / aland_sqmi
  ) |>
  filter(!is.na(income), !is.na(density), is.finite(density), aland_sqmi > 0) |>
  mutate(
    income_q = case_when(era %in% c("1990s", "2000s") ~ findInterval(income, cut_income_1999) + 1L,
                          TRUE ~ findInterval(income, cut_income_now) + 1L),
    density_band = case_when(era %in% c("1990s", "2000s") ~ findInterval(density, cut_dens_2000) + 1L,
                              era == "2010s" ~ findInterval(density, cut_dens_2010) + 1L,
                              era == "2020s" ~ findInterval(density, cut_dens_now) + 1L),
    density_band_lab = factor(band_labels[density_band], levels = band_labels),
    income_q_lab = factor(paste0("Q", income_q),
                           levels = c("Q1", "Q2", "Q3", "Q4"),
                           labels = c("Q1 (poorest)", "Q2", "Q3", "Q4 (richest)")),
    era = factor(era, levels = era_levels),
    log_density = log10(density)
  )
stopifnot(all(player_df$income_q %in% 1:4), all(player_df$density_band %in% 1:3))

no_data_share <- 1 - nrow(player_df) / nrow(matched)
cat(sprintf("%.1f%% of matched players lack income and/or density and are excluded from this analysis.\n",
            100 * no_data_share))

# ---------------------------------------------------------------------------
# Step 1 (EXPLORE): flexplot views. Our data is players-only -- there is no
# non-player "outcome" (e.g. performance) to model, so these are descriptive
# looks at the raw income/density relationship among player hometowns, not a
# fitted effect. They motivate, but do not replace, the representation-ratio
# analysis below (which is the only view with an actual population baseline).
# Saved to output/figures/ as exploration notes, not the publication figure.
# ---------------------------------------------------------------------------
pf <- as.data.frame(player_df)

fp1 <- tryCatch(
  flexplot(income ~ log_density | era, data = pf, method = "loess"),
  error = function(e) { message("flexplot view 1 failed: ", conditionMessage(e)); NULL }
)
if (!is.null(fp1)) {
  fp1 <- fp1 +
    labs(title = "Exploration: hometown income vs. log(density), by era",
         caption = "flexplot exploration only -- no publication claim; raw income scale differs by vintage (1999 SF3 caps ~$200k, ACS 2023 caps ~$250k).")
  save_fig("output/figures/income_density_flexplot_scatter.png", fp1, w = 14, h = 5)
}

fp2 <- tryCatch(
  flexplot(income ~ density_band_lab | era, data = pf),
  error = function(e) { message("flexplot view 2 failed: ", conditionMessage(e)); NULL }
)
if (!is.null(fp2)) {
  fp2 <- fp2 +
    labs(title = "Exploration: hometown income distribution by density tercile, by era", x = "Density tercile",
         caption = "flexplot exploration only -- raw player income by density band; no population baseline (see representation-ratio figure for that).")
  save_fig("output/figures/income_density_flexplot_bands.png", fp2, w = 12, h = 6)
}

fp3 <- tryCatch(
  flexplot(income ~ density_band_lab + era, data = pf),
  error = function(e) { message("flexplot view 3 (main effects) failed: ", conditionMessage(e)); NULL }
)
if (!is.null(fp3)) {
  save_fig("output/figures/income_density_flexplot_maineffects.png", fp3, w = 10, h = 6)
}
cat("wrote flexplot exploration PNGs to output/figures/\n")

# ---------------------------------------------------------------------------
# Step 2: the honest question. Within each era's density band, compare
# players' hometown-income-quartile shares to the population-weighted
# income-quartile shares of ALL census places in that same density band.
# rep_ratio = P(player hometown in income Q | density band, era)
#           / P(population lives in income Q | density band, era's regime)
# This is a real baseline (unlike the flexplot views above, which have none).
# ---------------------------------------------------------------------------
pop_cell_shares <- function(pop_col, income_col, dens_cuts, income_cuts) {
  places |>
    transmute(pop = .data[[pop_col]], income = .data[[income_col]], aland_sqmi) |>
    filter(!is.na(pop), pop > 0, aland_sqmi > 0, !is.na(income)) |>
    mutate(density = pop / aland_sqmi) |>
    filter(is.finite(density)) |>
    mutate(density_band = findInterval(density, dens_cuts) + 1L,
           income_q      = findInterval(income, income_cuts) + 1L) |>
    count(density_band, income_q, wt = pop, name = "pop") |>
    mutate(pop_share = pop / sum(pop))
}

# Three regimes: (pop2000, income1999) shared by 1990s+2000s; 2010s uses
# pop2010 for density but income_now (ACS 2023) for income (approximation,
# same one used in R/10_income.R); 2020s uses pop_now throughout.
baseline <- bind_rows(
  pop_cell_shares("pop2000", "income1999", cut_dens_2000, cut_income_1999) |> mutate(era_group = "1990s_2000s"),
  pop_cell_shares("pop2010", "income_now", cut_dens_2010, cut_income_now)  |> mutate(era_group = "2010s"),
  pop_cell_shares("pop_now", "income_now", cut_dens_now,  cut_income_now)  |> mutate(era_group = "2020s")
)
baseline_sums <- baseline |> group_by(era_group) |> summarise(total = sum(pop_share), .groups = "drop")
stopifnot(all(abs(baseline_sums$total - 1) < 1e-9))

era_group_map <- c("1990s" = "1990s_2000s", "2000s" = "1990s_2000s",
                    "2010s" = "2010s", "2020s" = "2020s")

player_cells <- player_df |>
  mutate(era_group = era_group_map[as.character(era)]) |>
  count(era, era_group, density_band, income_q, name = "n") |>
  group_by(era) |> mutate(player_share = n / sum(n)) |> ungroup()

player_sums <- player_cells |> group_by(era) |> summarise(total = sum(player_share), .groups = "drop")
stopifnot(all(abs(player_sums$total - 1) < 1e-9))

interaction_tbl <- player_cells |>
  left_join(baseline |> select(era_group, density_band, income_q, pop_share),
            by = c("era_group", "density_band", "income_q")) |>
  mutate(rep_ratio = player_share / pop_share,
         density_band_lab = factor(band_labels[density_band], levels = band_labels),
         income_q_lab = factor(paste0("Q", income_q),
                                levels = c("Q1", "Q2", "Q3", "Q4"),
                                labels = c("Q1 (poorest)", "Q2", "Q3", "Q4 (richest)")),
         era = factor(era, levels = era_levels))

write.csv(interaction_tbl |> select(era, density_band = density_band_lab, income_quartile = income_q_lab,
                                     n, player_share, pop_share, rep_ratio) |>
            arrange(era, density_band, income_quartile),
          "data/processed/income_density_table.csv", row.names = FALSE)

thin_cells <- interaction_tbl |> filter(n < 30)
if (nrow(thin_cells) > 0) {
  cat(sprintf("Note: %d of %d era x density x income cells have n < 30 (thinnest = %d) -- read those ratios cautiously.\n",
              nrow(thin_cells), nrow(interaction_tbl), min(interaction_tbl$n)))
}

cat("\n=== Representation ratio (player share / population share), Q4 (richest) by density band ===\n")
print(interaction_tbl |> filter(income_q == 4) |>
        select(era, density_band_lab, n, rep_ratio) |>
        pivot_wider(names_from = density_band_lab, values_from = c(n, rep_ratio)))

cat("\n=== Representation ratio, Q1 (poorest) by density band ===\n")
print(interaction_tbl |> filter(income_q == 1) |>
        select(era, density_band_lab, n, rep_ratio) |>
        pivot_wider(names_from = density_band_lab, values_from = c(n, rep_ratio)))

# ---------------------------------------------------------------------------
# Step 3: publication figure -- hand-rolled ggplot in repo style (matches
# density_gradient.png's era-colored line-chart convention), faceted by
# income quartile so the full distribution (not just the extremes) is shown.
# ---------------------------------------------------------------------------
# End-of-line era labels in the LAST facet only (Q4, at the High tercile),
# replacing the legend. Deterministic vertical spread keeps labels apart;
# ink = era hue darkened per the theme's direct-label note.
era_ink <- setNames(
  grDevices::rgb(t(grDevices::col2rgb(pal_era) * 0.72), maxColorValue = 255),
  names(pal_era))
spread_y <- function(y, gap) {
  o <- order(y); ys <- y[o]
  if (length(ys) > 1) for (i in 2:length(ys)) ys[i] <- max(ys[i], ys[i - 1] + gap)
  ys <- ys - (mean(ys) - mean(y[o]))  # re-center so labels track their lines
  y[o] <- ys
  y
}
era_labels <- interaction_tbl |>
  filter(income_q == 4, density_band == 3) |>
  mutate(y_lab = spread_y(rep_ratio, gap = 0.07))

p <- ggplot(interaction_tbl, aes(density_band_lab, rep_ratio, color = era, group = era)) +
  geom_baseline(1) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  direct_label(era_labels, aes(y = y_lab, label = era), nudge_x = 0.16, size = 3.1,
               colour = era_ink[as.character(era_labels$era)]) +
  facet_wrap(~income_q_lab, nrow = 1) +
  scale_color_manual(values = pal_era) +
  scale_x_discrete(labels = c("Low", "Mid", "High")) +
  coord_cartesian(clip = "off") +
  labs(title = "The poor-hometown advantage is fading at every density level",
       subtitle = paste0(
         "Representation ratio: player share / population share in the same hometown income quartile and density band; dashed line: parity.\n",
         "Facets: income quartile; x: hometown density tercile. Rookie eras 1990s to 2020s; ratio axis zoomed, not from 0."),
       x = "Hometown population-density tercile", y = "Representation ratio",
       caption = fig_caption(
         source = "nflverse + ESPN + US Census; income vintages 2000 SF3 (1990s/2000s) and ACS 2023 (2010s/2020s), density vintages 2000/2010/2023 by era",
         universe = "\nBaseline: all US census places in the same vintage regime.",
         note = sprintf("%.1f%% of matched players lack income or density and are excluded; compositional analysis, no causal claim.",
                        100 * no_data_share))) +
  theme_hometown() +
  theme(plot.margin = margin(10, 56, 8, 10))

save_fig("docs/figures/income_density_nfl.png", p)
cat("\nPattern is directionally consistent under both tercile and quintile density binning ",
    "(see data/processed/income_density_table.csv) -- promoted to docs/figures/income_density_nfl.png.\n", sep = "")
