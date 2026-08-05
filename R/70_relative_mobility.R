# =============================================================================
# 70_relative_mobility.R -- Michael's ask: get at RELATIVE mobility, not just
# absolute. His point: a conservative can wave off the absolute-mobility chart
# ("of course 1940s kids out-earned their Depression-era parents"), because
# absolute mobility rides on overall economic growth. RELATIVE mobility strips
# that out: it asks how much your parents' RANK predicts your rank, which in a
# perfect meritocracy would be zero (where you end up would not depend on where
# you started). This is the rank-rank relationship, the standard measure.
#
# SOURCE (the line): Chetty, Hendren, Kline & Saez (2014), "Where is the Land of
# Opportunity?", Quarterly Journal of Economics 129(4). The US intergenerational
# rank-rank slope is 0.341: a child's expected adult income rank rises 0.341
# ranks for each rank of parent income. Anchored so a mid-rank child averages
# mid-rank (child rank = 0.341 * parent rank + 32.95). The same paper's quintile
# transition matrix gives the concrete version we already chart: a bottom-fifth
# child has a 7.5% shot at the top fifth versus 36.5% for a top-fifth child.
#
# SOURCE (the trend): Chetty, Dobbie, Goldman, Porter & Yang (2024, rev. 2025),
# "Changing Opportunity", NBER WP 32697. Between the 1978 and 1992 birth cohorts
# the class gap in economic mobility GREW about 30%, so relative mobility has
# been worsening, not improving. Numbers typed from the paper (pdftotext).
# PUBLISHED RESEARCH, NOT THIS PROJECT'S DATA. No race/ethnicity on the chart.
# =============================================================================

suppressMessages({library(dplyr); library(ggplot2); library(tibble)})
source("R/lib/theme_hometown.R")
nfl <- pal_sport[["NFL"]]; fair_col <- ink_baseline

slope <- 0.341; intercept <- 50 - slope*50   # anchor: parent rank 50 -> child rank 50
line <- tibble(parent = 0:100, child = intercept + slope*parent)
pts  <- tibble(parent = c(0, 25, 75, 100),
               child  = intercept + slope*c(0, 25, 75, 100),
               lab    = sprintf("%.0f", intercept + slope*c(0, 25, 75, 100)))

cat("=== US rank-rank line (child expected adult income rank by parent rank) ===\n")
cat(sprintf("parent 0 -> child %.1f | parent 100 -> child %.1f | spread = %.0f ranks (the slope x 100)\n",
            intercept, intercept+slope*100, slope*100))

p <- ggplot() +
  # perfect meritocracy: flat at 50
  geom_hline(yintercept = 50, linetype = "dashed", colour = fair_col, linewidth = 0.5) +
  # the actual US relationship
  geom_line(data = line, aes(parent, child), colour = nfl, linewidth = 1.1) +
  geom_point(data = pts, aes(parent, child), colour = nfl, size = 2.8) +
  geom_text(data = pts[c(1,4),], aes(parent, child, label = sprintf("%sth", lab)),
            vjust = c(1.9, -1.0), hjust = c(0, 1), size = 3.3, fontface = "bold", colour = ink_body) +
  annotate("text", x = 2, y = 53.5, hjust = 0, size = 3.1, colour = fair_col,
           label = "A perfect meritocracy: where you end up\ndoes not depend on where you started") +
  annotate("text", x = 74, y = 40, hjust = 1, size = 3.1, colour = nfl, fontface = "bold",
           label = "The US: your parents' rank\nstill predicts yours") +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 25),
                     labels = function(x) paste0(x, "th")) +
  scale_y_continuous(limits = c(28, 72), breaks = seq(30, 70, 10),
                     labels = function(x) paste0(x, "th")) +
  labs(
    title = "Where you end up still tracks where you started: born at the bottom you average the 33rd percentile, at the top the 67th",
    subtitle = "A child's expected adult income rank, by their parents' income rank. In a perfect meritocracy the line would be flat at the 50th.",
    x = "Parents' income rank", y = "Child's expected adult income rank",
    caption = fig_caption(
      "Rank-rank slope 0.34 from Chetty, Hendren, Kline & Saez (2014), QJE 129(4); recent trend from Chetty, Dobbie, Goldman, Porter & Yang (2024), NBER WP 32697",
      "\nFrom published research, not this project's data. Relative mobility compares a child's income RANK to their parents' rank, so unlike the out-earn-your-\nparents measure it strips out overall economic growth. The line's slope of 0.34 means a full jump from the bottom to the top of the parent distribution\nlifts a child's expected adult rank by 34 places; a flat line would mean birth rank does not matter at all.",
      "\nThe concrete version: a child born in the poorest fifth has about a 7.5 percent shot at the richest fifth, versus 36.5 percent for a child born at the top.\nAnd it is not improving: the class gap in mobility grew about 30 percent between the 1978 and 1992 birth cohorts, so the ladder has gotten a little steeper,\nnot flatter. This is the answer to 'the 1940s just had an easy baseline': relative mobility nets out growth, and by this measure the US has not become fairer.")) +
  theme_hometown(grid = "y")
save_fig("docs/figures/ba_relative_mobility.png", p, w = 12, h = 6.6)
cat("\ndone\n")
