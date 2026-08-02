# =============================================================================
# 62_mlb_pitchers_income.R -- Michael's question: in baseball, do PITCHERS come
# from wealthier hometowns than HITTERS? His theory: you can "engineer arms" now
# if a kid is 6'4+, so pitching is more training/money dependent. Reproduces
# R/15's income-quartile matching for MLB, then splits by role (pitcher vs
# position player). Role comes from Lahman's Appearances table (career games by
# position); primary position wins, pitcher if that is P.
# HONEST OR BUST: report whichever direction the data goes, not the guess.
# =============================================================================

suppressMessages({
  library(dplyr); library(arrow); library(ggplot2); library(tidyr); library(forcats)
})
source("R/lib/bins.R")
source("R/lib/places.R")
source("R/lib/theme_hometown.R")

# --- Lahman Appearances (career games by position) --------------------------
appear_raw <- "data/raw/Appearances.RData"
if (!file.exists(appear_raw)) {
  cat("downloading Lahman Appearances table...\n")
  download.file("https://raw.githubusercontent.com/cdalzell/Lahman/master/data/Appearances.RData",
                appear_raw, mode = "wb")
}
aenv <- new.env(); load(appear_raw, envir = aenv)
appearances <- get("Appearances", envir = aenv)

# Career games at each position, summed over a player's seasons.
role <- appearances |>
  group_by(playerID) |>
  summarise(across(c(G_p, G_c, G_1b, G_2b, G_3b, G_ss, G_lf, G_cf, G_rf, G_dh),
                   ~ sum(.x, na.rm = TRUE)), .groups = "drop") |>
  mutate(G_of = G_lf + G_cf + G_rf) |>
  # Primary position = the one with the most career games. Pitcher if that is P.
  mutate(prim = c("P","C","1B","2B","3B","SS","OF","DH")[
           max.col(across(c(G_p, G_c, G_1b, G_2b, G_3b, G_ss, G_of, G_dh)), ties.method = "first")],
         role = if_else(prim == "P", "Pitcher", "Hitter")) |>
  select(playerID, role, prim)

# --- People: birth info + debut era (keep playerID for the join) ------------
penv <- new.env(); load("data/raw/People.RData", envir = penv)
people <- get("People", envir = penv) |> filter(!is.na(debut), debut != "")
mlb <- people |>
  transmute(
    playerID,
    player_name = trimws(paste(coalesce(nameFirst, ""), nameLast)),
    birth_city  = birthCity,
    birth_state = birthState,
    birth_country = birthCountry,
    era = era_cohort(as.integer(substr(debut, 1, 4)))
  ) |>
  filter(birth_country == "USA", birth_state %in% c(state.abb, "DC"), !is.na(era)) |>
  left_join(role, by = "playerID") |>
  filter(!is.na(role))

# --- income-quartile cutpoints (same population-weighted method as R/15/R/61)-
places <- read_parquet("data/processed/census_places.parquet")
wq <- function(income, pop) {
  ok <- !is.na(income) & !is.na(pop) & pop > 0
  o <- order(income[ok]); cw <- cumsum(as.numeric(pop[ok][o])) / sum(as.numeric(pop[ok][o]))
  list(income = income[ok][o], cw = cw)
}
cuts <- function(w) sapply(c(.25, .5, .75), function(q) w$income[which.max(w$cw >= q)])
cut99  <- cuts(wq(places$income1999, places$pop2000))
cutnow <- cuts(wq(places$income_now, places$pop_now))

matched <- match_places(mlb, places) |>
  filter(match_tier != "unmatched") |>
  mutate(vintage = if_else(era %in% c("1990s", "2000s"), "1999", "now"),
         income  = if_else(vintage == "1999", matched_income1999, matched_income_now),
         q = case_when(is.na(income) ~ NA_integer_,
                       vintage == "1999" ~ findInterval(income, cut99) + 1L,
                       TRUE              ~ findInterval(income, cutnow) + 1L)) |>
  filter(!is.na(q))

# --- compare pitchers vs hitters --------------------------------------------
dist <- matched |>
  count(role, q) |>
  group_by(role) |> mutate(share = n / sum(n), grp_n = sum(n)) |> ungroup()

summ <- matched |>
  group_by(role) |>
  summarise(n = n(),
            med_income = median(income, na.rm = TRUE),
            q4_share = mean(q == 4),   # richest-quartile hometowns
            q1_share = mean(q == 1),   # poorest-quartile hometowns
            .groups = "drop")
cat("=== MLB pitchers vs hitters, hometown income (US-born, 1990+ debut) ===\n")
print(as.data.frame(summ))
p_q4 <- summ$q4_share[summ$role == "Pitcher"]; h_q4 <- summ$q4_share[summ$role == "Hitter"]
cat(sprintf("\nRichest-quartile hometown share: pitchers %.1f%%, hitters %.1f%% (gap %+.1f pts)\n",
            100*p_q4, 100*h_q4, 100*(p_q4 - h_q4)))
cat(sprintf("Median hometown income: pitchers $%s, hitters $%s\n",
            format(round(summ$med_income[summ$role=="Pitcher"]), big.mark=","),
            format(round(summ$med_income[summ$role=="Hitter"]), big.mark=",")))

# HONEST OR BUST: this is a near-tie, and the two measures disagree slightly
# (pitchers a touch lower in the richest quartile, a touch higher on median
# income), so we call it a wash rather than pick a flattering direction.
diff_pts <- 100 * (p_q4 - h_q4)
med_gap  <- summ$med_income[summ$role=="Pitcher"] - summ$med_income[summ$role=="Hitter"]
verdict <- "come out about even. Pitchers are marginally less likely to be from the richest-quartile hometowns, but their median hometown income is a few hundred dollars higher, so the two measures cancel out"
title <- "In baseball, pitchers and hitters come from almost the same hometown income mix"

# --- chart: income-quartile distribution, pitchers vs hitters ---------------
qlab <- c("Poorest\nquartile", "2nd", "3rd", "Richest\nquartile")
plotdf <- dist |> mutate(qf = factor(q, levels = 1:4, labels = qlab),
                         role = factor(role, levels = c("Hitter", "Pitcher")))
pal_role <- c(Hitter = "#D55E00", Pitcher = "#0072B2")
p <- ggplot(plotdf, aes(qf, share, fill = role)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.66) +
  geom_hline(yintercept = 0.25, linetype = "22", colour = ink_baseline, linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.0f%%", 100*share)),
            position = position_dodge(width = 0.72), vjust = -0.5,
            size = 3.1, fontface = "bold", colour = ink_body) +
  scale_fill_manual(values = pal_role, name = NULL) +
  scale_y_continuous(labels = function(x) paste0(round(100*x), "%"),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = title,
    subtitle = "Hometown income quartile of US-born MLB players (1990 or later debut), pitchers versus position players. Dashed line: even 25%.",
    x = NULL, y = "Share of that role's players",
    caption = fig_caption(
      "Lahman database (People + Appearances) + US Census place income; same income-quartile method as our cross-sport income charts",
      sprintf("\nRole is each player's primary career position (most games); pitcher if that is P. US-born players with a matched hometown, 1990+ debut\n(pitchers n=%d, hitters n=%d). Income vintage: 2000 census for 1990s/2000s debuts, recent ACS for 2010s/2020s.",
              summ$n[summ$role=="Pitcher"], summ$n[summ$role=="Hitter"]),
      "\nMichael's hunch was that pitchers come from wealthier hometowns (arms can be 'engineered' with training). The data barely moves.\nPitchers are marginally LESS likely to come from a richest-quartile hometown (20% vs 22%), and median hometown income is nearly identical.\nEither way, not the clear split the hunch predicted.")) +
  theme_hometown(grid = "y") +
  theme(legend.position = "top", legend.justification = "left",
        legend.key.size = unit(11, "pt"),
        legend.text = element_text(size = rel(0.85)))
save_fig("docs/figures/ba_mlb_pitchers_income.png", p, w = 11, h = 5.8)
cat("\ndone\n")
