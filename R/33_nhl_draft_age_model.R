# =============================================================================
# 33_nhl_draft_age_model.R -- the NHL version of R/32: do relative age and draft
# capital predict an NHL career? The NFL model used age at the draft (21-24,
# driven by college years). NHL players are almost all drafted at 18, so the
# analog of "young for your class" is BIRTH QUARTER: within one draft cohort, a
# Q4 (Oct-Dec) player is nearly a year younger than a Q1 (Jan-Mar) player. We
# also check actual age at the draft (18 vs 19 vs 20+).
#
# For drafted players who reached the NHL we fit, mirroring R/32:
#     career games ~ birth_quarter + log(draft_overall)
#     career points ~ birth_quarter + log(draft_overall)
# log(draft_overall) holds draft capital fixed, so a surviving Q4 advantage
# means the late-born outproduce the early-born even at the same draft slot.
# Data: the cached NHL landing pages from R/12 (draftDetails + careerTotals).
# =============================================================================

suppressMessages({
  library(dplyr); library(ggplot2); library(jsonlite); library(purrr); library(tidyr); library(tibble)
})
source("R/lib/theme_hometown.R")

files <- list.files("data/raw/nhl_landing", pattern = "[.]json$", full.names = TRUE)
cat(sprintf("parsing %d cached NHL landing pages...\n", length(files)))

parse_one <- function(path) {
  p <- tryCatch(fromJSON(path, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(p)) return(NULL)
  rs <- p$careerTotals$regularSeason
  dd <- p$draftDetails
  tibble(
    birth_date = if (!is.null(p$birthDate) && length(p$birthDate)) as.character(p$birthDate) else NA_character_,
    position   = if (!is.null(p$position) && length(p$position)) as.character(p$position) else NA_character_,
    gp         = if (!is.null(rs$gamesPlayed)) as.integer(rs$gamesPlayed) else NA_integer_,
    points     = if (!is.null(rs$points)) as.integer(rs$points) else NA_integer_,
    draft_overall = if (!is.null(dd) && !is.null(dd$overallPick)) as.integer(dd$overallPick) else NA_integer_,
    draft_year    = if (!is.null(dd) && !is.null(dd$year)) as.integer(dd$year) else NA_integer_)
}

raw <- map_dfr(files, parse_one)

nhl <- raw |>
  filter(!is.na(birth_date), !is.na(gp), gp >= 1, !is.na(draft_overall), !is.na(draft_year)) |>
  mutate(birth_date = as.Date(birth_date),
         month = as.integer(format(birth_date, "%m")),
         quarter = ceiling(month / 3),
         birth_q = factor(paste0("Q", quarter), levels = paste0("Q", 1:4)),
         age_at_draft = draft_year - as.integer(format(birth_date, "%Y")),
         age_grp = cut(age_at_draft, c(-Inf, 18, 19, Inf),
                       labels = c("18 or younger", "19", "20 or older")),
         log_pick = log(draft_overall))
cat(sprintf("drafted NHL players who played >=1 game: %d\n\n", nrow(nhl)))

# --- adjusted career outcomes by birth quarter, controlling for draft pick ---
adj_by_q <- function(ycol) {
  d <- nhl; d$y <- d[[ycol]]
  m  <- lm(y ~ birth_q + log_pick, data = d)
  m0 <- lm(y ~ birth_q, data = d)   # unadjusted, for comparison
  nd <- tibble(birth_q = factor(paste0("Q",1:4), levels = paste0("Q",1:4)),
               log_pick = median(d$log_pick))
  pr  <- predict(m,  nd, se.fit = TRUE)
  pr0 <- predict(m0, nd, se.fit = TRUE)
  nd |> mutate(metric = ycol,
               adj = pr$fit, lo = pr$fit - 1.96*pr$se.fit, hi = pr$fit + 1.96*pr$se.fit,
               unadj = pr0$fit)
}

gp_tbl  <- adj_by_q("gp")
pts_tbl <- adj_by_q("points")

cat("=== career games by birth quarter (n, unadjusted mean, pick-adjusted mean) ===\n")
print(as.data.frame(gp_tbl |> mutate(across(c(adj,lo,hi,unadj), round))))
cat("\n=== career points by birth quarter ===\n")
print(as.data.frame(pts_tbl |> mutate(across(c(adj,lo,hi,unadj), round))))

# interaction: does the birth-quarter effect depend on draft pick?
mi <- lm(gp ~ quarter * log_pick, data = nhl)
ip <- summary(mi)$coefficients
cat(sprintf("\ninteraction quarter x log(pick) on games: p = %.3f\n",
            ip["quarter:log_pick","Pr(>|t|)"]))

# actual age at draft (18 vs 19 vs 20+), controlling for pick
ma <- lm(gp ~ age_grp + log_pick, data = nhl)
cat("\n=== career games by actual age at draft (pick-controlled coefficients vs '18 or younger') ===\n")
print(round(summary(ma)$coefficients[,c(1,2,4)], 2))
cat("age-at-draft group counts:\n"); print(table(nhl$age_grp))

# --- chart: adjusted career games + points by birth quarter, with error bars -
plot_df <- bind_rows(
  gp_tbl  |> transmute(birth_q, adj, lo, hi, panel = "Career games"),
  pts_tbl |> transmute(birth_q, adj, lo, hi, panel = "Career points")) |>
  mutate(panel = factor(panel, levels = c("Career games","Career points")))

p <- ggplot(plot_df, aes(birth_q, adj)) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2, colour = ink_baseline, linewidth = 0.5) +
  geom_line(aes(group = 1), colour = pal_sport[["NHL"]], linewidth = 0.7) +
  geom_point(colour = pal_sport[["NHL"]], size = 2.9) +
  facet_wrap(~panel, scales = "free_y") +
  scale_y_continuous(expand = expansion(mult = c(0.08, 0.12))) +
  labs(
    title = "In the NHL too, the late-born outproduce the early-born at the same draft slot",
    subtitle = "Model-adjusted career games and points by birth quarter, holding draft position fixed. Q4 players are the youngest in their draft class.",
    x = "Birth quarter (Q1 = Jan-Mar ... Q4 = Oct-Dec)", y = NULL,
    caption = fig_caption(
      "NHL API player landing pages (draft pick + career totals, cached by R/12)",
      sprintf("\nDrafted NHL players with at least one game (n = %s). Points are model-adjusted means holding draft pick fixed; bars are 95%% confidence intervals.",
              format(nrow(nhl), big.mark = ",")),
      "\nControl: log(draft overall pick). Q4 (youngest) sitting above Q1 with clear bars means relative age still predicts the career even after draft capital.")) +
  theme_hometown(grid = "y") +
  theme(strip.text = element_text(hjust = 0.5),
        panel.spacing = unit(1.8, "lines"))
save_fig("docs/figures/ba_nhl_draftage_model.png", p, w = 11, h = 5.2)
cat("\ndone\n")
