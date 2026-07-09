suppressMessages({ library(dplyr); library(arrow); library(ggplot2); library(tidyr) })
source("R/lib/bins.R")

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

p1 <- ggplot(effect, aes(bin, rep_ratio, fill = era)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("1990s" = "#A6BDDB", "2000s" = "#74A9CF",
                               "2010s" = "#2B8CBE", "2020s" = "#045A8D")) +
  labs(title = "Where NFL players come from, relative to where people live",
       subtitle = "Representation ratio: share of players born in each place size ÷ share of population living there",
       x = "Birthplace population (Census places incl. CDPs)",
       y = "Representation ratio (1 = proportional)",
       fill = "Rookie era",
       caption = sprintf(
         "Data: nflverse + ESPN + US Census. Population vintages 2000/2010/2023 by era; %.0f%% of matched players lacked a vintage population and are excluded.",
         100 * no_vintage_pop)) +
  theme_minimal(base_size = 16) +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1),
        plot.caption = element_text(hjust = 0, size = rel(0.7)))
ggsave("docs/figures/cote_bins.png", p1, width = 12, height = 6.75, dpi = 320)

# Density gradient: deciles of place density (people/sq mi), share of players per decile
density_tbl <- matched |>
  mutate(pop = coalesce(matched_pop_now, matched_pop2010, matched_pop2000),
         density = pop / aland_sqmi) |>
  filter(!is.na(density), is.finite(density), aland_sqmi > 0) |>
  mutate(decile = ntile(density, 10)) |>
  count(era, decile) |>
  group_by(era) |> mutate(share = n / sum(n)) |> ungroup()

p2 <- ggplot(density_tbl, aes(decile, share, color = era)) +
  geom_line(linewidth = 1.2) + geom_point(size = 2.5) +
  scale_x_continuous(breaks = 1:10) +
  scale_color_manual(values = c("1990s" = "#A6BDDB", "2000s" = "#74A9CF",
                                "2010s" = "#2B8CBE", "2020s" = "#045A8D")) +
  labs(title = "NFL player birthplaces by population density",
       subtitle = "Share of players by decile of birthplace density (1 = most rural, 10 = most urban); deciles over matched places",
       x = "Birthplace density decile", y = "Share of players", color = "Rookie era",
       caption = "Data: nflverse + ESPN + Census Gazetteer (land area).") +
  theme_minimal(base_size = 16) +
  theme(panel.grid.minor = element_blank())
ggsave("docs/figures/density_gradient.png", p2, width = 12, height = 6.75, dpi = 320)

saveRDS(list(effect = effect, density = density_tbl),
        "data/processed/effect_tables.rds")
print(effect |> select(era, bin, rep_ratio) |>
        pivot_wider(names_from = era, values_from = rep_ratio))
cat("\nwrote cote_bins.png + density_gradient.png\n")
