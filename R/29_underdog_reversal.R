# =============================================================================
# 29_underdog_reversal.R -- Michael's "Underdog (opposite)" thread.
#
# The relative age effect says early-in-the-year births DOMINATE the NHL (we
# showed Q1 at ~32% of players, Q4 at ~19%). The underdog hypothesis is the
# flip side: among the players who actually MADE it, the late-born ones should
# be BETTER on average, because they had to be genuinely elite to survive a
# youth system tilted against them. If true, that is the counterintuitive hook:
# the pipeline is filtering out talented late-born kids for being small at 11,
# and the few who slip through prove it.
#
# We test it on NHL players using the landing pages already cached by R/12
# (data/raw/nhl_landing/*.json): draft overall pick + career regular-season
# games and points, by birth quarter, among players who reached the NHL.
# No new network calls. Reports the by-quarter table, then charts it.
# =============================================================================

suppressMessages({
  library(dplyr); library(ggplot2); library(jsonlite); library(tibble); library(purrr)
})
source("R/lib/theme_hometown.R")

files <- list.files("data/raw/nhl_landing", pattern = "[.]json$", full.names = TRUE)
cat(sprintf("parsing %d cached NHL landing pages...\n", length(files)))

parse_one <- function(path) {
  p <- tryCatch(fromJSON(path, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(p)) return(NULL)
  rs <- p$careerTotals$regularSeason
  gp <- if (!is.null(rs$gamesPlayed)) rs$gamesPlayed else NA_integer_
  pts <- if (!is.null(rs$points)) rs$points else NA_integer_
  dd <- p$draftDetails
  overall <- if (!is.null(dd) && !is.null(dd$overallPick)) dd$overallPick else NA_integer_
  bdate <- if (!is.null(p$birthDate) && length(p$birthDate)) p$birthDate else NA_character_
  pos <- if (!is.null(p$position) && length(p$position)) p$position else NA_character_
  tibble(birth_date = as.character(bdate), position = as.character(pos),
         gp = as.integer(gp), points = as.integer(pts), draft_overall = as.integer(overall))
}

raw <- map_dfr(files, parse_one)

# Players who actually reached the NHL (>=1 regular-season game) with a birthday.
nhl <- raw |>
  filter(!is.na(birth_date), !is.na(gp), gp >= 1) |>
  mutate(birth_date = as.Date(birth_date),
         month = as.integer(format(birth_date, "%m")),
         quarter = ceiling(month / 3),
         quarter_lab = factor(paste0("Q", quarter), levels = paste0("Q", 1:4)),
         drafted = !is.na(draft_overall),
         first_round = !is.na(draft_overall) & draft_overall <= 31)
cat(sprintf("NHL players who played >=1 game, with birthday: %d\n\n", nrow(nhl)))

# --- by-quarter outcomes -----------------------------------------------------
by_q <- nhl |>
  group_by(quarter_lab) |>
  summarise(
    n            = n(),
    share        = n() / nrow(nhl),
    med_gp       = median(gp),
    mean_gp      = mean(gp),
    med_points   = median(points, na.rm = TRUE),
    mean_points  = mean(points, na.rm = TRUE),
    pct_drafted  = mean(drafted),
    pct_first_rd = mean(first_round),
    med_draft    = median(draft_overall, na.rm = TRUE),
    .groups = "drop")

cat("=== NHL outcomes by birth quarter (players who made it) ===\n")
print(as.data.frame(by_q |>
  mutate(share = round(100 * share, 1), med_gp = round(med_gp),
         mean_gp = round(mean_gp), med_points = round(med_points),
         mean_points = round(mean_points),
         pct_drafted = round(100 * pct_drafted, 1),
         pct_first_rd = round(100 * pct_first_rd, 1))))

q1 <- by_q |> filter(quarter_lab == "Q1")
q4 <- by_q |> filter(quarter_lab == "Q4")
cat(sprintf("\nQ1 vs Q4 among those who made it:\n"))
cat(sprintf("  players:        Q1 %d  vs  Q4 %d  (%.2fx more Q1)\n", q1$n, q4$n, q1$n/q4$n))
cat(sprintf("  median games:   Q1 %.0f  vs  Q4 %.0f\n", q1$med_gp, q4$med_gp))
cat(sprintf("  mean games:     Q1 %.0f  vs  Q4 %.0f\n", q1$mean_gp, q4$mean_gp))
cat(sprintf("  median points:  Q1 %.0f  vs  Q4 %.0f\n", q1$med_points, q4$med_points))
cat(sprintf("  %% first-round:  Q1 %.1f  vs  Q4 %.1f\n", 100*q1$pct_first_rd, 100*q4$pct_first_rd))

saveRDS(by_q, "data/processed/underdog_by_quarter.rds")
cat("\nwrote data/processed/underdog_by_quarter.rds\n")

# --- one pasteable chart: how many make it vs how good they are -------------
library(tidyr)
plot_df <- by_q |>
  transmute(quarter_lab,
            `Share of NHL players` = 100 * share,
            `Median career games`  = med_gp) |>
  pivot_longer(-quarter_lab, names_to = "measure", values_to = "value") |>
  mutate(measure = factor(measure, levels = c("Share of NHL players", "Median career games")),
         lab = ifelse(measure == "Share of NHL players",
                      sprintf("%.0f%%", value), sprintf("%.0f", value)))

p_underdog <- ggplot(plot_df, aes(quarter_lab, value)) +
  geom_col(fill = pal_sport[["NHL"]], width = 0.7) +
  geom_text(aes(label = lab), vjust = -0.5, size = 3.6, fontface = "bold",
            colour = ink_body) +
  facet_wrap(~measure, scales = "free_y") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(
    title = "Fewer late-born players reach the NHL, but the ones who do outlast the early-born",
    subtitle = "NHL players with at least one game, by birth quarter. Left: how many make it. Right: how long they stay.",
    x = NULL, y = NULL,
    caption = fig_caption(
      "NHL API player landing pages (career games + draft, cached by R/12)",
      "All NHL players with at least one regular-season game and a known birthday (n = 5,639), all eras and birth countries.",
      paste0("\nQ1 = Jan-Mar ... Q4 = Oct-Dec. The youngest in the cutoff year (Q4) are the least likely to reach the NHL, yet post the higher career:\n",
             "median 170 games and 42 points, vs 135 games and 30 points for Q1. Surviving a youth system tilted toward older kids selects for talent."))) +
  theme_hometown(grid = "y") +
  theme(strip.text = element_text(hjust = 0.5, size = rel(0.95)),
        panel.spacing = unit(1.8, "lines"))
save_fig("docs/figures/ba_underdog_reversal.png", p_underdog, w = 11, h = 5.2)

