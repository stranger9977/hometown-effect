suppressMessages({ library(dplyr); library(arrow); library(ggplot2); library(tidyr) })

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

p <- ggplot(income_tbl, aes(factor(income_quartile), share, fill = era)) +
  geom_hline(yintercept = 0.25, linetype = "dashed", color = "grey40") +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("1990s" = "#A6BDDB", "2000s" = "#74A9CF",
                               "2010s" = "#2B8CBE", "2020s" = "#045A8D")) +
  scale_x_discrete(labels = c("Q1 (poorest)", "Q2", "Q3", "Q4 (richest)")) +
  labs(title = "Do NFL players increasingly come from richer hometowns?",
       subtitle = "Share of players by hometown income quartile (population-weighted, era-matched vintages)\nDashed line = proportional (25%)",
       x = "Hometown median household income quartile", y = "Share of players",
       fill = "Rookie era",
       caption = sprintf(
         "Data: nflverse + ESPN + US Census (2000 SF3 income for 1990s/2000s cohorts, ACS 2023 for 2010s/2020s).\n%.0f%% of matched players lacked a vintage income value and are excluded.",
         100 * no_income_share)) +
  theme_minimal(base_size = 16) +
  theme(panel.grid.minor = element_blank(),
        plot.caption = element_text(hjust = 0, size = rel(0.7)))
ggsave("docs/figures/income_gradient.png", p, width = 12, height = 6.75, dpi = 320)

print(income_tbl |> pivot_wider(names_from = era, values_from = share, id_cols = income_quartile))
cat("\nwrote income_gradient.png + income_table.csv\n")
