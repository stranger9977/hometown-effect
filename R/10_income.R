suppressMessages({ library(dplyr); library(arrow); library(ggplot2); library(tidyr) })
source("R/lib/theme_hometown.R")

matched <- read_parquet("data/processed/birthplace_matched.parquet") |>
  filter(match_tier != "unmatched", !is.na(era))
places <- read_parquet("data/processed/census_places.parquet")

# Sanity: suppressed-median sentinels were cleaned to NA upstream; make sure
# nothing negative snuck through (zero is a legitimate, if rare, value for
# tiny places with no reported households).
stopifnot(all(places$income1999 >= 0, na.rm = TRUE))
stopifnot(all(places$income_now  >= 0, na.rm = TRUE))

# Population-weighted income quartile cutpoints per vintage
wq <- function(income, pop) {
  ok <- !is.na(income) & !is.na(pop) & pop > 0
  o <- order(income[ok])
  cw <- cumsum(as.numeric(pop[ok][o])) / sum(as.numeric(pop[ok][o]))
  list(income = income[ok][o], cw = cw)
}
cuts <- function(w) sapply(c(.25, .5, .75), function(q) w$income[which.max(w$cw >= q)])

cut99  <- cuts(wq(places$income1999, places$pop2000))
cutnow <- cuts(wq(places$income_now, places$pop_now))

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

write.csv(income_tbl, "data/processed/income_table.csv", row.names = FALSE)

# --- Step 4 interrogation: shares must sum to 1 per era; a quartile share
# over 0.6 anywhere would suggest the weighted cutpoints are off, not that
# players are actually this concentrated. ---
era_sums <- income_tbl |> group_by(era) |> summarise(total = sum(share), .groups = "drop")
stopifnot(all(abs(era_sums$total - 1) < 1e-9))
if (any(income_tbl$share > 0.6)) {
  warning("A quartile share exceeds 0.6 -- inspect the weighted cutpoints before trusting this figure.")
}

# In-panel era key: label the four bars of the Q1 group once, in era hue
# darkened to label ink (theme direct-label note), replacing the legend.
era_ink <- setNames(
  grDevices::rgb(t(grDevices::col2rgb(pal_era) * 0.72), maxColorValue = 255),
  names(pal_era))
era_key <- income_tbl |> filter(income_quartile == 1)

p <- ggplot(income_tbl, aes(factor(income_quartile), share, fill = era)) +
  geom_baseline(0.25) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(data = era_key,
            aes(label = era, colour = era, group = era),
            position = position_dodge(width = 0.8),
            vjust = -0.7, size = 3.1, fontface = "bold") +
  scale_fill_manual(values = pal_era) +
  scale_colour_manual(values = era_ink) +
  scale_x_discrete(labels = c("Q1 (poorest)", "Q2", "Q3", "Q4 (richest)")) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1),
                     expand = expansion(mult = c(0, 0.05))) +
  coord_cartesian(clip = "off") +
  labs(title = "The richest hometowns went from 10% of NFL players to 25%",
       subtitle = paste0(
         "Share of players by hometown median household income quartile, population-weighted with era-matched vintages.\n",
         "Rookie eras 1990s to 2020s. Dashed line: proportional share, 25%."),
       x = NULL, y = "Share of players",
       caption = fig_caption(
         source = "nflverse + ESPN + US Census (2000 SF3 income for 1990s/2000s cohorts, ACS 2023 for 2010s/2020s)",
         universe = "NFL players matched to a US census place, by rookie era.",
         note = sprintf("\n%.0f%% of matched players lack a vintage income value and are excluded.",
                        100 * no_income_share))) +
  theme_hometown()
save_fig("docs/figures/income_gradient.png", p)

print(income_tbl |> pivot_wider(names_from = era, values_from = share, id_cols = income_quartile))
cat("\nwrote income_gradient.png + income_table.csv\n")
