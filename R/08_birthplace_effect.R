suppressMessages({ library(dplyr); library(arrow); library(ggplot2); library(tidyr) })
source("R/lib/bins.R")
source("R/lib/theme_hometown.R")

matched <- read_parquet("data/processed/birthplace_matched.parquet") |>
  filter(match_tier != "unmatched", !is.na(era))
places <- read_parquet("data/processed/census_places.parquet")

pl_vintage  <- c("1990s" = "pop2000", "2000s" = "pop2000",
                 "2010s" = "pop2010", "2020s" = "pop_now")

player_bins <- matched |>
  vintage_pop() |>
  filter(!is.na(pop)) |>
  mutate(bin = cote_bin(pop)) |>
  count(era, bin, name = "players") |>
  group_by(era) |> mutate(player_share = players / sum(players)) |> ungroup()

no_vintage_pop <- matched |> vintage_pop() |>
  summarise(share = mean(is.na(pop))) |> pull(share)

pop_bins <- purrr::map_dfr(names(pl_vintage), function(e) {
  col <- pl_vintage[[e]]
  places |>
    filter(!is.na(.data[[col]])) |>
    mutate(bin = cote_bin(.data[[col]])) |>
    group_by(bin) |>
    summarise(pop = sum(.data[[col]]), .groups = "drop") |>
    mutate(era = e, pop_share = pop / sum(pop))
})

effect <- player_bins |>
  left_join(pop_bins, by = c("era", "bin")) |>
  mutate(rep_ratio = player_share / pop_share)

# Label ink: era hue at ~70% darkness (see direct_label note in theme file).
era_lab_col <- vapply(pal_era, function(h) {
  v <- grDevices::col2rgb(h) * 0.72
  grDevices::rgb(v[1], v[2], v[3], maxColorValue = 255)
}, character(1))

# In-panel era key: label the four bars of the 250k-500k group (bin level 7),
# replacing the fill legend (spec rule 8). Dodge width 0.8, 4 groups ->
# bar centers at x = 7 + c(-0.3, -0.1, 0.1, 0.3).
era_key <- effect |>
  filter(as.integer(bin) == 7L) |>
  mutate(x = 7 + c("1990s" = -0.3, "2000s" = -0.1,
                   "2010s" =  0.1, "2020s" =  0.3)[era],
         y = rep_ratio + 0.04)

p1 <- ggplot(effect, aes(bin, rep_ratio, fill = era)) +
  geom_baseline(1) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = pal_era) +
  geom_text(data = era_key, aes(x = x, y = y, label = era),
            colour = era_lab_col[era_key$era], size = 2.9,
            fontface = "bold", vjust = 0, inherit.aes = FALSE) +
  scale_x_discrete(labels = function(x) gsub("–", "-", x)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Mid-size cities of 250k-500k produce about twice their share of NFL players",
       subtitle = "Representation ratio: share of players born in each place size / share of US population living there, by rookie era; dashed line 1 = proportional",
       x = "Birthplace population (Census places incl. CDPs)",
       y = "Representation ratio (observed / expected)",
       caption = fig_caption(
         "nflverse + ESPN + US Census",
         "NFL players with rookie seasons 1990 or later, matched to US Census places.",
         sprintf("\nPopulation vintages 2000/2010/2023 by era; %.0f%% of matched players lacked a vintage population and are excluded.",
                 100 * no_vintage_pop))) +
  theme_hometown() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
save_fig("docs/figures/cote_bins.png", p1)

# Density gradient: deciles of place density (people/sq mi), share of players per decile
density_tbl <- matched |>
  mutate(pop = coalesce(matched_pop_now, matched_pop2010, matched_pop2000),
         density = pop / aland_sqmi) |>
  filter(!is.na(density), is.finite(density), aland_sqmi > 0) |>
  mutate(decile = ntile(density, 10)) |>
  count(era, decile) |>
  group_by(era) |> mutate(share = n / sum(n)) |> ungroup()

# End-of-line era labels replace the legend (spec rule 8): one row per era at
# decile 10, deterministic nudge, era hue at 70% darkness.
era_end <- density_tbl |> filter(decile == 10)

p2 <- ggplot(density_tbl, aes(decile, share, color = era)) +
  geom_baseline(0.1) +
  geom_line(linewidth = 1.2) + geom_point(size = 2.5) +
  direct_label(era_end, aes(x = decile, y = share, label = era),
               nudge_x = 0.25, colour = era_lab_col[era_end$era],
               inherit.aes = FALSE) +
  scale_x_continuous(breaks = 1:10, expand = expansion(mult = c(0.03, 0.02))) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_color_manual(values = pal_era) +
  coord_cartesian(clip = "off") +
  labs(title = "The most-urban tenth of hometowns is shrinking as an NFL source",
       subtitle = "Share of each era's players by decile of birthplace density (1 = most rural, 10 = most urban); dashed line: an even tenth (10%); y axis zoomed, not from zero",
       x = "Birthplace density decile", y = "Share of players",
       caption = fig_caption(
         "nflverse + ESPN + Census Gazetteer (land area)",
         "NFL players with rookie seasons 1990 or later, matched to US Census places.",
         "\nDensity uses the most recent available place population per player.")) +
  theme_hometown() +
  theme(plot.margin = margin(10, 70, 8, 10))
save_fig("docs/figures/density_gradient.png", p2)

saveRDS(list(effect = effect, density = density_tbl),
        "data/processed/effect_tables.rds")
print(effect |> select(era, bin, rep_ratio) |>
        pivot_wider(names_from = era, values_from = rep_ratio))
cat("\nwrote cote_bins.png + density_gradient.png\n")
