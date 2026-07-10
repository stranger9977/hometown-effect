suppressMessages({ library(dplyr); library(readr); library(ggplot2); library(tidyr) })
source("R/lib/theme_hometown.R")

# ---- Figure 1: the suburban flip (NFL distance bands, 1990s vs 2020s) -------
b <- read_csv("data/processed/metro_distance_bands.csv", show_col_types = FALSE)

flip <- b |>
  filter(sport == "NFL", cut == "dist_band", era %in% c("1990s", "2020s")) |>
  mutate(group = factor(group, levels = c("0-15mi", "15-45mi", "45-100mi", "100mi+"),
                        labels = c("0-15 mi\n(urban core)", "15-45 mi\n(suburban ring)",
                                   "45-100 mi\n(exurban)", "100+ mi\n(remote)")))

flip_wide <- flip |> select(group, era, rep_ratio) |>
  pivot_wider(names_from = era, values_from = rep_ratio)

p1 <- ggplot(flip_wide, aes(x = group)) +
  geom_baseline(1) +
  geom_segment(aes(xend = group, y = `1990s`, yend = `2020s`),
               arrow = arrow(length = unit(0.14, "in"), type = "closed"),
               linewidth = 1.1, color = "grey55") +
  geom_point(aes(y = `1990s`), size = 3.4, color = "#A6BDDB") +
  geom_point(aes(y = `2020s`), size = 3.4, color = "#045A8D") +
  annotate("text", x = 0.62, y = flip_wide$`1990s`[1] + 0.06, label = "1990s",
           color = "#7da4c4", size = 4.6, fontface = "bold", hjust = 0) +
  annotate("text", x = 0.62, y = flip_wide$`2020s`[1] - 0.07, label = "2020s",
           color = "#045A8D", size = 4.6, fontface = "bold", hjust = 0) +
  scale_y_continuous(limits = c(0.5, 1.4)) +
  labs(title = "The suburbs flipped: once the NFL's dead zone, now its best band",
       subtitle = "NFL representation ratio by hometown distance to the nearest 1M+ metro, 1990s vs 2020s rookie cohorts",
       x = NULL, y = "Representation ratio (1 = proportional)",
       caption = paste0(
         "Data: nflverse+ESPN, Census CBSA delineation + places. US-born players; hometown = high school where known, else birthplace.\n",
         "Distance from hometown place centroid to the nearest principal city of a metro area (CBSA) of 1M+ people.")) +
  theme_hometown()
save_fig("docs/figures/suburban_flip.png", p1)

# ---- Figure 2: SuperZip ladder (top-5% hometowns by sport by era) -----------
s <- read_csv("data/processed/superzip_players.csv", show_col_types = FALSE) |>
  filter(level == "place", group == "sport_x_era") |>
  mutate(sport = factor(sport, levels = c("NFL", "MLB", "NHL", "NBA")),
         era_n = as.integer(factor(key, levels = c("1990s", "2000s", "2010s", "2020s"))))

ends <- s |> filter(key == "2020s")

p2 <- ggplot(s, aes(era_n, rep_ratio, color = sport)) +
  geom_baseline(1) +
  geom_line(aes(alpha = sport), linewidth = 2) +
  geom_point(aes(alpha = sport), size = 3) +
  geom_text(data = ends, aes(label = sprintf("%s %.1fx", sport, rep_ratio)),
            hjust = -0.15, size = 4.8, fontface = "bold", show.legend = FALSE) +
  scale_color_manual(values = pal_sport) +
  scale_alpha_manual(values = c(NFL = 1, MLB = 1, NHL = 0.65, NBA = 1), guide = "none") +
  scale_x_continuous(breaks = 1:4, labels = c("1990s", "2000s", "2010s", "2020s"),
                     limits = c(1, 4.75)) +
  scale_y_continuous(limits = c(0, 3.3)) +
  labs(title = "Hockey players now come from elite zip-code America at 3x the norm",
       subtitle = "Representation of top-5% hometowns (combined income + college-education percentile, the SuperZip method), by career-start era",
       x = NULL, y = "Representation ratio (1 = proportional)",
       caption = paste0(
         "Data: league sources + US Census ACS 2023 (income B19013, education B15003). US-born players; place-level index,\n",
         "population-weighted percentiles, top 5% threshold, after the Washington Post SuperZip method. NHL line lighter: small samples.")) +
  theme_hometown() +
  theme(legend.position = "none")
save_fig("docs/figures/superzip_sports.png", p2)

cat("wrote suburban_flip.png + superzip_sports.png\n")
