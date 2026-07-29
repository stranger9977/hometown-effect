# =============================================================================
# 36_sport_percapita.R -- "the where for the other sports": which US states
# produce MLB, NBA, and NHL players at the highest rate per capita
# (birthplace-based, US-born only).
# =============================================================================

suppressMessages({
  library(arrow); library(dplyr); library(tidyr); library(ggplot2); library(stringr)
})
source("R/lib/theme_hometown.R")

# --- load players, US-born only ---------------------------------------------
players <- bind_rows(
  read_parquet("data/processed/players_mlb.parquet"),
  read_parquet("data/processed/players_nba.parquet"),
  read_parquet("data/processed/players_nhl.parquet")
) |>
  filter(birth_country == "USA")

# birth_state here is a 2-letter abbreviation (verified: 100% 2-char, 45-52
# distinct codes per sport). Reconcile to full state names via base R's
# state.abb/state.name, adding DC by hand since it isn't in those vectors.
state_lookup <- setNames(c(state.name, "District of Columbia"),
                         c(state.abb, "DC"))

dropped <- players |> filter(!birth_state %in% names(state_lookup))
if (nrow(dropped) > 0) {
  cat(sprintf("dropping %d non-state rows (e.g. Canal Zone): ", nrow(dropped)))
  print(table(dropped$birth_state, dropped$sport))
}

players <- players |>
  filter(birth_state %in% names(state_lookup)) |>
  mutate(state = state_lookup[birth_state],
         sport = factor(sport, levels = c("MLB", "NBA", "NHL")))

# --- state population (2024) -------------------------------------------------
pop_by_state <- read_parquet("data/processed/census_counties.parquet") |>
  group_by(state) |>
  summarise(pop = sum(pop2024), .groups = "drop")

# --- per-capita rate, every sport x every state (0 filled where no players) --
counts <- players |> count(sport, state, name = "n")

rate_tbl <- expand_grid(sport = levels(players$sport), state = pop_by_state$state) |>
  left_join(counts, by = c("sport", "state")) |>
  mutate(n = coalesce(n, 0)) |>
  left_join(pop_by_state, by = "state") |>
  mutate(per_million = 1e6 * n / pop)

# DC is a dense city, not a state, and its rate isn't comparable to whole
# states (it would top both MLB and NBA here). Our NFL states chart excludes
# it for the same reason, so pull its numbers for the caption, then drop it
# before ranking.
dc_row <- rate_tbl |> filter(state == "District of Columbia")
dc_rates <- setNames(round(dc_row$per_million, 1), dc_row$sport)
cat("Washington D.C. rates (excluded from ranking, noted in caption):\n")
print(dc_rates)

# --- top 8 actual states per sport --------------------------------------------
top8 <- rate_tbl |>
  filter(state != "District of Columbia") |>
  group_by(sport) |>
  slice_max(per_million, n = 8, with_ties = FALSE) |>
  ungroup() |>
  arrange(sport, per_million) |>
  mutate(state_facet = factor(paste(sport, state, sep = "___"),
                              levels = paste(sport, state, sep = "___")))

cat("=== top 8 US states per capita, by sport ===\n")
for (s in levels(players$sport)) {
  tot <- sum(rate_tbl$n[rate_tbl$sport == s])
  cat(sprintf("\n%s (total US-born n = %d):\n", s, tot))
  top8 |> filter(sport == s) |> arrange(desc(per_million)) |>
    transmute(state, n, per_million = round(per_million, 1)) |>
    as.data.frame() |> print(row.names = FALSE)
}

# ============================================================================
# Chart: top 8 states per sport, players per million residents
# ============================================================================
p <- ggplot(top8, aes(state_facet, per_million, fill = sport)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = sprintf("%.0f", per_million)), hjust = -0.2,
            size = 3.2, fontface = "bold", colour = ink_body) +
  scale_x_discrete(labels = function(x) sub("^.*___", "", x)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  scale_fill_manual(values = pal_sport) +
  coord_flip() +
  facet_wrap(~sport, scales = "free", ncol = 3) +
  labs(
    title = "Pennsylvania leads baseball, Mississippi leads basketball, and Minnesota leads hockey in players per capita",
    subtitle = "US-born players per million residents by birth state, top 8 states per sport, 1990s-2020s pooled",
    x = NULL, y = "Players per million residents",
    caption = fig_caption(
      "MLB/NBA/NHL league sources + US Census population estimates (2024)",
      "\nUS-born players only, placed by birthplace (not hometown or high school); state population is 2024 residents, not births.",
      paste0("\nWashington D.C. is excluded as a non-state outlier (a dense city, not comparable to whole states); it would ",
             "otherwise top MLB at 148 and NBA at 111 per million, matching the treatment in our NFL states chart.\n",
             "Small states with few players (e.g. North Dakota, Alaska in hockey) can post high rates off modest counts; see printed n's."))) +
  theme_hometown(grid = "none")

save_fig("docs/figures/ba_sport_percapita.png", p, w = 12, h = 5.4)

cat("\ndone\n")
