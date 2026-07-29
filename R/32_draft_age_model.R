# =============================================================================
# 32_draft_age_model.R -- does being young for your draft class predict a better
# NFL career, at the SAME draft capital? A model with error bars, per position,
# for three outcomes, so we can see which effect is strongest and most reliable.
#
# For each position (QB, RB, WR, TE) and each outcome we fit:
#     outcome ~ age_group + log(draft_pick) + draft_year
# log(draft_pick) holds draft capital roughly fixed (so we are not just
# rediscovering that younger guys go higher), and draft_year absorbs cohort and
# career-length differences. We then read off the model-adjusted mean for each
# age group with a 95% confidence interval (the error bars). We also fit the
# interaction age x log(pick) and report whether the age edge depends on where a
# player was picked.
#
# Outcomes (all keyed by gsis_id, draft classes 2000-2016 for mature careers):
#   1. Fantasy points, first 4 seasons (PPR) -- equal exposure for everyone,
#      the rookie-contract production window. load_player_stats.
#   2. Career value (weighted AV). load_draft_picks.
#   3. Second-contract APY as a share of the cap -- the market's verdict, era-
#      adjusted. First veteran deal signed >= draft_year + 3. load_contracts.
#      (Only players who earned one; that selection is noted.)
# =============================================================================

suppressMessages({
  library(nflreadr); library(dplyr); library(ggplot2); library(tidyr)
  library(purrr); library(stringr); library(tibble)
})
source("R/lib/theme_hometown.R")

POS <- c("QB","RB","WR","TE")
Y0 <- 2000; Y1 <- 2016

# --- base: drafted skill players with age -----------------------------------
base <- load_draft_picks() |> as_tibble() |>
  filter(position %in% POS, season >= Y0, season <= Y1, !is.na(age), !is.na(gsis_id),
         gsis_id != "", !is.na(pick)) |>
  transmute(gsis_id, player = pfr_player_name, position, draft_year = season,
            pick = as.integer(pick), age = as.integer(age),
            w_av = as.numeric(w_av))
cat("base drafted skill players 2000-2016:", nrow(base), "\n")

# --- outcome 1: first-4-season PPR fantasy points ---------------------------
ps <- load_player_stats(seasons = Y0:2024) |> as_tibble()
if ("season_type" %in% names(ps)) ps <- ps |> filter(season_type == "REG")
fp <- ps |>
  select(gsis_id = player_id, season, fpts = fantasy_points_ppr) |>
  inner_join(base |> select(gsis_id, draft_year), by = "gsis_id") |>
  filter(season >= draft_year, season <= draft_year + 3) |>
  group_by(gsis_id) |> summarise(fp4 = sum(fpts, na.rm = TRUE), .groups = "drop")

# --- outcome 3: second contract (first veteran deal, apy % of cap) -----------
ct <- load_contracts() |> as_tibble() |>
  filter(!is.na(gsis_id), gsis_id != "", year_signed > 0, !is.na(apy_cap_pct)) |>
  select(gsis_id, year_signed, apy_cap_pct, apy) |>
  inner_join(base |> select(gsis_id, draft_year), by = "gsis_id") |>
  filter(year_signed >= draft_year + 3) |>              # after the rookie deal
  group_by(gsis_id) |> slice_min(year_signed, n = 1, with_ties = FALSE) |>
  ungroup() |> transmute(gsis_id, c2_cap = 100 * apy_cap_pct, c2_apy = apy)

dat <- base |>
  left_join(fp, by = "gsis_id") |>
  left_join(ct, by = "gsis_id") |>
  mutate(age_grp = factor(cut(age, c(-Inf,21,22,23,Inf),
                              labels = c("21 or under","22","23","24 or older")),
                          levels = c("24 or older","23","22","21 or under")),
         log_pick = log(pick))

cat(sprintf("join rates: fantasy %.0f%%, 2nd-contract %.0f%% of drafted players\n",
            100*mean(!is.na(dat$fp4)), 100*mean(!is.na(dat$c2_cap))))

OUT <- tibble::tribble(
  ~key,       ~col,     ~label,
  "fantasy",  "fp4",    "Fantasy points, first 4 seasons (PPR)",
  "av",       "w_av",   "Career value (weighted AV)",
  "contract", "c2_cap", "Second contract APY (% of the cap)")

age_levels <- c("21 or under","22","23","24 or older")

# --- fit per outcome x position, collect adjusted means + interaction test ---
fit_cell <- function(okey, ocol, pos) {
  d <- dat |> filter(position == pos, !is.na(.data[[ocol]]))
  d$y <- d[[ocol]]
  d <- d |> filter(is.finite(y))
  if (nrow(d) < 40 || n_distinct(d$age_grp) < 3) return(NULL)
  m <- lm(y ~ age_grp + log_pick + draft_year, data = d)
  nd <- tibble(age_grp = factor(age_levels, levels = levels(dat$age_grp)),
               log_pick = median(d$log_pick), draft_year = median(d$draft_year))
  pr <- predict(m, nd, se.fit = TRUE)
  adj <- nd |> mutate(fit = pr$fit, lo = pr$fit - 1.96*pr$se.fit,
                      hi = pr$fit + 1.96*pr$se.fit,
                      outcome = okey, position = pos,
                      n = as.integer(table(d$age_grp)[as.character(age_grp)]))
  # interaction: does the age slope depend on draft pick? (age continuous)
  mi <- lm(y ~ age * log_pick + draft_year, data = d |> mutate(age = as.integer(as.character(
              factor(age_grp, levels = age_levels, labels = c(21,22,23,25))))))
  ic <- summary(mi)$coefficients
  inter_p <- if ("age:log_pick" %in% rownames(ic)) ic["age:log_pick","Pr(>|t|)"] else NA_real_
  # standardized young-vs-old effect (z-scored outcome)
  d$yz <- as.numeric(scale(d$y))
  mz <- lm(yz ~ age_grp + log_pick + draft_year, data = d)
  cz <- summary(mz)$coefficients
  eff <- if ("age_grp21 or under" %in% rownames(cz)) cz["age_grp21 or under", ] else c(NA,NA,NA,NA)
  list(adj = adj,
       eff = tibble(outcome = okey, position = pos,
                    d_young_old = eff[1], d_se = eff[2],
                    inter_p = inter_p, n = nrow(d)))
}

grid <- tidyr::crossing(OUT |> select(okey = key, ocol = col), pos = POS)
res <- pmap(list(grid$okey, grid$ocol, grid$pos), fit_cell) |> compact()
adj_all <- map_dfr(res, "adj") |>
  left_join(OUT, by = c("outcome" = "key")) |>
  mutate(age_grp = factor(age_grp, levels = age_levels),
         position = factor(position, levels = POS))
eff_all <- map_dfr(res, "eff")

cat("\n=== standardized young(<=21) vs old(24+) effect, in SD units (95% CI), by position/outcome ===\n")
print(as.data.frame(eff_all |>
  transmute(outcome, position,
            d = round(d_young_old,2), ci_lo = round(d_young_old-1.96*d_se,2),
            ci_hi = round(d_young_old+1.96*d_se,2),
            interaction_p = round(inter_p,3), n)))

# strongest effect = largest mean standardized young-old gap that is positive
verdict <- eff_all |> group_by(outcome) |>
  summarise(mean_d = mean(d_young_old, na.rm=TRUE),
            share_pos = mean(d_young_old > 0, na.rm=TRUE), .groups="drop") |>
  arrange(desc(mean_d))
cat("\n=== which outcome shows the strongest young-player effect? ===\n")
print(as.data.frame(verdict |> mutate(mean_d = round(mean_d,2), share_pos = round(share_pos,2))))

# --- charts: one per outcome, facet by position, adjusted mean + 95% CI ------
render_outcome <- function(okey, fname, ytitle, subtitle) {
  d <- adj_all |> filter(outcome == okey)
  p <- ggplot(d, aes(age_grp, fit)) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.22, colour = ink_baseline,
                  linewidth = 0.5) +
    geom_point(colour = pal_sport[["NFL"]], size = 2.8) +
    geom_line(aes(group = 1), colour = pal_sport[["NFL"]], linewidth = 0.7) +
    facet_wrap(~position, nrow = 1, scales = "free_y") +
    scale_y_continuous(expand = expansion(mult = c(0.08, 0.12))) +
    labs(title = ytitle, subtitle = subtitle,
         x = "Age at the NFL draft", y = NULL,
         caption = fig_caption(
           "nflverse draft picks, player stats, and Over-The-Cap contracts (joined by player id)",
           "\nSkill-position players drafted 2000-2016. Points are model-adjusted means holding draft pick and draft year fixed; bars are 95% confidence intervals.",
           "\nControls: log(draft pick) + draft year. If younger points sit above older with non-overlapping bars, youth predicts the outcome even at equal draft capital.")) +
    theme_hometown(grid = "y") +
    theme(strip.text = element_text(hjust = 0.5),
          axis.text.x = element_text(size = rel(0.68)),
          panel.spacing = unit(1.2, "lines"))
  save_fig(fname, p, w = 12, h = 4.8)
}

render_outcome("fantasy", "docs/figures/ba_draftage_fantasy.png",
  "Younger picks score more fantasy points on their rookie deal, at every skill spot but quarterback",
  "Model-adjusted first-4-season PPR points by age at the draft, one panel per position. Young QBs sit and develop, so they are the exception.")
render_outcome("av", "docs/figures/ba_draftage_av.png",
  "Younger draft picks return more career value, holding draft capital fixed",
  "Model-adjusted career weighted AV by age at the draft, one panel per position")
render_outcome("contract", "docs/figures/ba_draftage_contract.png",
  "Younger picks land a bigger second contract, at every skill spot but quarterback",
  "Model-adjusted second-contract APY (share of cap) by age at the draft, one panel per position. Quarterback is again the exception.")

# --- comparison chart: standardized young-old effect across the 3 outcomes ---
eff_plot <- eff_all |>
  left_join(OUT, by = c("outcome" = "key")) |>
  mutate(position = factor(position, levels = POS),
         outcome_lab = factor(label, levels = OUT$label),
         lo = d_young_old - 1.96*d_se, hi = d_young_old + 1.96*d_se)
pc <- ggplot(eff_plot, aes(position, d_young_old, colour = outcome_lab)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = ink_baseline, linewidth = 0.4) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.18,
                position = position_dodge(width = 0.6), linewidth = 0.6) +
  geom_point(position = position_dodge(width = 0.6), size = 2.6) +
  scale_colour_manual(values = c(pal_sport[["NFL"]], pal_sport[["MLB"]], pal_sport[["NHL"]]),
                      name = NULL) +
  labs(title = "How much does being young for your class help, and where is it most convincing?",
       subtitle = "Effect of being 21-or-under vs 24-or-older at the draft, in standard deviations of each outcome (positive = youth helps)",
       x = NULL, y = "Standardized effect (SD), 95% CI",
       caption = fig_caption(
         "nflverse draft picks + player stats + Over-The-Cap contracts",
         "\nEach point is the model-adjusted gap between the youngest and oldest draft-age groups for that outcome and position, in SD units, controlling for log(pick) + draft year.",
         "\nBars clear of the dashed zero line mean the youth edge is statistically reliable for that outcome and position.")) +
  theme_hometown(grid = "y") +
  theme(legend.position = "top", legend.justification = "left")
save_fig("docs/figures/ba_draftage_compare.png", pc, w = 11.5, h = 5.6)

cat("\ndone\n")
