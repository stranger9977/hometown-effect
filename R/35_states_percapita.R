# =============================================================================
# 35_states_percapita.R -- which states produce the most and fewest NFL
# players per resident? Aggregates R/09's county rates up to the state level.
#
# District of Columbia is excluded from the state ranking: it's a city, not
# a state, and its tiny population (700K) makes its rate an outlier that
# doesn't belong next to 50-state comparisons (noted in the caption).
# =============================================================================

suppressMessages({
  library(dplyr); library(readr); library(ggplot2)
})
source("R/lib/theme_hometown.R")

rates <- read_csv("data/processed/county_rates.csv", show_col_types = FALSE)

state_tbl_all <- rates |>
  group_by(state) |>
  summarise(players = sum(players), pop2024 = sum(pop2024), .groups = "drop") |>
  mutate(per_million = 1e6 * players / pop2024) |>
  arrange(desc(per_million))

cat("=== NFL players per million residents, all states + DC, sorted ===\n")
print(state_tbl_all |> mutate(per_million = round(per_million, 1)) |> as.data.frame(),
      row.names = FALSE)

# ---- state-only ranking (DC excluded) ---------------------------------------
state_tbl <- state_tbl_all |> filter(state != "District of Columbia")
us_rate <- 1e6 * sum(state_tbl$players) / sum(state_tbl$pop2024)
n_states <- nrow(state_tbl)

cat(sprintf("\nUS average (50 states, DC excluded): %.1f players per million\n", us_rate))
cat(sprintf("(DC alone: %.1f per million, %d players -- excluded as a non-state outlier)\n",
            state_tbl_all$per_million[state_tbl_all$state == "District of Columbia"],
            state_tbl_all$players[state_tbl_all$state == "District of Columbia"]))

top12 <- state_tbl |> slice_max(per_million, n = 12) |> arrange(per_million)
bot8  <- state_tbl |> slice_min(per_million, n = 8)  |> arrange(per_million)

lead_state <- top12$state[nrow(top12)]
lead_rate  <- top12$per_million[nrow(top12)]
lead_ratio <- lead_rate / us_rate

# One blank spacer "row" marks the 38 omitted states and doubles as open
# space for the US-average annotation, so the label never fights a bar.
gap_label <- sprintf("(38 states in between, ranks 13-42)")
state_levels <- c(bot8$state, gap_label, top12$state)

plot_tbl <- bind_rows(bot8, top12) |>
  mutate(state = factor(state, levels = state_levels))

avg_row <- data.frame(state = factor(gap_label, levels = state_levels),
                      per_million = NA_real_)

p <- ggplot(plot_tbl, aes(state, per_million)) +
  geom_col(fill = pal_sport[["NFL"]], width = 0.68) +
  geom_hline(yintercept = us_rate, linetype = "dashed", colour = ink_baseline,
             linewidth = 0.4) +
  geom_text(data = avg_row, aes(x = state, y = us_rate),
            label = sprintf("US average, 50 states: %.1f", us_rate),
            hjust = 0, nudge_y = 2, size = 3.2, colour = ink_body) +
  geom_text(aes(label = sprintf("%.1f", per_million)), hjust = -0.2,
            size = 3.5, fontface = "bold", colour = ink_body, na.rm = TRUE) +
  coord_flip() +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(
    title = sprintf(
      "%s produces NFL players at %.1fx the national state rate", lead_state, lead_ratio),
    subtitle = "NFL players per million residents by home state: the 12 highest and 8 lowest",
    x = NULL, y = "NFL players per million residents",
    caption = fig_caption(
      "nflverse + ESPN + Sleeper (players, rookie seasons 1990-2025); US Census county population estimates (pop2024)",
      "\nPlayers placed by hometown county (high school where known, else birthplace); rates summed from county to state.",
      "\nDistrict of Columbia (148.0 per million) is excluded as a non-state outlier; the national average above is the 50-state figure."
    )) +
  theme_hometown(grid = "none")

save_fig("docs/figures/ba_states_percapita.png", p, w = 9.5, h = 9)

cat("\ndone\n")
