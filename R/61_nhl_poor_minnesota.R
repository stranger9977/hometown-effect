# =============================================================================
# 61_nhl_poor_minnesota.R -- Michael's question: of the NHL players who come
# from the poorest-quartile hometowns, how many are from Minnesota (his theory:
# it is the one place you can play hockey cheaply)? Reproduces R/15's income-
# quartile matching for NHL, then breaks the poorest quartile down by home state.
# =============================================================================

suppressMessages({
  library(dplyr); library(arrow); library(ggplot2); library(tidyr); library(tibble); library(forcats)
})
source("R/lib/places.R")
source("R/lib/theme_hometown.R")

places <- read_parquet("data/processed/census_places.parquet")

# Same population-weighted income-quartile cutpoints as R/15 / R/10.
wq <- function(income, pop) {
  ok <- !is.na(income) & !is.na(pop) & pop > 0
  o <- order(income[ok])
  cw <- cumsum(as.numeric(pop[ok][o])) / sum(as.numeric(pop[ok][o]))
  list(income = income[ok][o], cw = cw)
}
cuts <- function(w) sapply(c(.25, .5, .75), function(q) w$income[which.max(w$cw >= q)])
cut99  <- cuts(wq(places$income1999, places$pop2000))
cutnow <- cuts(wq(places$income_now, places$pop_now))

nhl <- read_parquet("data/processed/players_nhl.parquet") |>
  filter(birth_country == "USA", birth_state %in% c(state.abb, "DC"))
matched <- match_places(nhl, places) |>
  filter(match_tier != "unmatched", !is.na(era)) |>
  mutate(vintage = if_else(era %in% c("1990s", "2000s"), "1999", "now"),
         income = if_else(vintage == "1999", matched_income1999, matched_income_now),
         q = case_when(is.na(income) ~ NA_integer_,
                       vintage == "1999" ~ findInterval(income, cut99) + 1L,
                       TRUE              ~ findInterval(income, cutnow) + 1L)) |>
  filter(!is.na(q))

# Overall poorest-quartile (Q1) share, to anchor the "about 10 percent" figure.
q1_share <- mean(matched$q == 1)
cat(sprintf("NHL players with a matched hometown income: %d\n", nrow(matched)))
cat(sprintf("Share from the poorest quartile (Q1) hometown: %.1f%% (n = %d)\n\n",
            100 * q1_share, sum(matched$q == 1)))

# Of the Q1 (poorest) NHL players, break down by home state.
q1 <- matched |> filter(q == 1)
by_state <- q1 |>
  count(birth_state, name = "n") |>
  mutate(share = 100 * n / sum(n)) |>
  arrange(desc(n))
mn_share <- by_state$share[by_state$birth_state == "MN"]
cat(sprintf("Of the %d poorest-quartile NHL players, Minnesota accounts for %.0f%% (n = %d).\n",
            nrow(q1), mn_share, by_state$n[by_state$birth_state == "MN"]))
cat("Top home states among poorest-quartile NHL players:\n")
print(as.data.frame(head(by_state, 10)))

# --- chart: state breakdown of the poorest-quartile NHL players --------------
top <- by_state |> slice_max(n, n = 8) |>
  mutate(state = fct_reorder(birth_state, n),
         hl = birth_state == "MN")
p <- ggplot(top, aes(state, n, fill = hl)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = sprintf("%d (%.0f%%)", n, share)), hjust = -0.15,
            size = 3.3, fontface = "bold", colour = ink_body) +
  coord_flip() +
  scale_fill_manual(values = c(`TRUE` = pal_sport[["NHL"]], `FALSE` = "grey70"), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(
    title = "The poorest-hometown NHL players are mostly from New York and Michigan, not Minnesota",
    subtitle = "Home states of US-born NHL players whose hometown is in the poorest income quartile, all eras pooled.",
    x = NULL, y = "NHL players from a poorest-quartile hometown",
    caption = fig_caption(
      "NHL API + US Census place income (same income-quartile method as our cross-sport income charts)",
      sprintf("\nUS-born NHL players with a matched hometown, career starts 1990-2025. The poorest quartile is %.0f%% of them (n = %d); this chart breaks that group down by home state.", 100 * q1_share, nrow(q1)),
      sprintf("\nMichael's hunch that these players come from Minnesota is not supported: Minnesota is only %.0f%%, third behind New York and Michigan. Minnesota produces the\nmost NHL players of any state overall, but from solidly middle-income hockey towns, not the poorest; the poorest-hometown players skew to dense urban areas.\nNHL is a small sample (poorest-quartile n = %d, and per-state cells are dozens or fewer), so read the exact shares with caution.", mn_share, nrow(q1)))) +
  theme_hometown(grid = "none")
save_fig("docs/figures/ba_nhl_poor_minnesota.png", p, w = 11, h = 5.4)
cat("\ndone\n")
