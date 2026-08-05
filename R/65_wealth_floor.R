# =============================================================================
# 65_wealth_floor.R -- Michael asked about the BOTTOM of the distribution:
# (1) how much a stable, higher-income family lifts the tail end, and
# (2) the reverse -- how UNLIKELY it is for a kid from a wealthy family to score
# low or end up poor. He called it "the floor that money buys." Two charts.
#
# FIGURE 1 (ba_wealth_floor.png) -- the SAT floor.
#   SOURCE: Chetty, Deming & Friedman (2023, revised Aug 2025), "Diversifying
#   Society's Leaders?", NBER Working Paper 31492 / Opportunity Insights,
#   Appendix Table A.3, Panel B ("Distribution of Test Scores Conditional on
#   Parent Income"), page with the appendix tables. This is the SAME table
#   R/60 used for the TOP score ranges; here I use the LOW ranges. Every row
#   below is typed straight from the paper's PDF (pdftotext -layout). Panel B
#   gives, for each parent-income percentile bin, the share of ALL kids in that
#   bin scoring in each range, plus the share who did not take the test (the
#   column sums to 100%). To ask "how often does a kid who TAKES the test score
#   low" I condition on taking: P(score under 1000 | took) = [sum of the five
#   sub-1000 rows] / [1 - did-not-take]. That is exact conditional-probability
#   arithmetic on published shares, no assumptions. I condition because a poor
#   kid who never sits the test is absent, not "not scoring low," so counting
#   only test-takers is the fair floor comparison. HONEST NOTE: only about a
#   fifth of bottom-decile kids take the test versus ~88% at the top, and the
#   poor kids who do take are positively selected, so the TRUE floor gap is even
#   wider than the test-taker line shows.
#
# FIGURE 2 (ba_wealth_floor_income.png) -- the income floor.
#   SOURCE: Chetty, Hendren, Kline & Saez (2014), Quarterly Journal of Economics
#   129(4), Table II "National Quintile Transition Matrix", the CHILD BOTTOM-
#   FIFTH row, for the 9,867,736 children in the 1980-82 birth cohorts. R/58
#   already used this table's child-TOP-fifth row (7.5 ... 36.5); here I use the
#   child-BOTTOM-fifth row (the chance of LANDING in the poorest fifth). Typed
#   straight from the paper PDF (NBER w19843, pdftotext -layout).
#
# BOTH ARE PUBLISHED RESEARCH, NOT THIS PROJECT'S DATA. No race/ethnicity.
# =============================================================================

suppressMessages({
  library(dplyr); library(ggplot2); library(tibble)
})
source("R/lib/theme_hometown.R")

nfl <- pal_sport[["NFL"]]

# ---------------------------------------------------------------------------
# FIGURE 1 -- the SAT floor: share of TEST-TAKERS scoring under 1000, by income.
# ---------------------------------------------------------------------------
# Appendix Table A.3, Panel B. Columns = parent income percentile bins. The five
# rows below are the sub-1000 SAT ranges (percent of ALL kids in that bin);
# dnt = "Did not take SAT or ACT" row. All typed from the paper.
tab <- tribble(
  ~bin,       ~ord, ~s900, ~s800, ~s700, ~s600, ~sbelow600, ~dnt,
  "0-10",      1,    3.8,   3.9,   3.6,   2.5,   1.2,        79.8,
  "10-20",     2,    5.2,   5.6,   5.4,   3.7,   1.8,        71.9,
  "20-30",     3,    6.1,   6.4,   5.8,   3.8,   1.7,        68.3,
  "30-40",     4,    7.2,   7.1,   6.0,   3.7,   1.6,        64.5,
  "40-50",     5,    8.7,   7.8,   5.9,   3.3,   1.4,        59.5,
  "50-60",     6,   10.4,   8.3,   5.7,   2.9,   1.0,        53.1,
  "60-70",     7,   12.3,   8.8,   5.3,   2.4,   0.8,        45.6,
  "70-80",     8,   14.0,   9.0,   4.8,   2.0,   0.6,        36.4,
  "80-90",     9,   14.7,   8.6,   4.1,   1.5,   0.4,        25.8,
  "90-95",    10,   14.0,   7.3,   3.0,   1.0,   0.3,        16.9,
  "95-96",    11,   12.8,   6.3,   2.4,   0.8,   0.2,        13.2,
  "96-97",    12,   12.2,   5.7,   2.2,   0.7,   0.2,        12.2,
  "97-98",    13,   11.4,   5.3,   2.0,   0.6,   0.2,        11.1,
  "98-99",    14,   10.6,   4.7,   1.8,   0.6,   0.2,        10.3,
  "99-99.9",  15,    9.6,   4.2,   1.5,   0.5,   0.2,        10.3,
  "Top 0.1%", 16,    8.3,   3.8,   1.3,   0.4,   0.2,        12.1
) |>
  mutate(
    below1000 = s900 + s800 + s700 + s600 + sbelow600, # share of ALL kids scoring under 1000
    took      = 100 - dnt,                             # share who took the test
    floor     = below1000 / took * 100,                # P(under 1000 | took the test)
    bin = factor(bin, levels = bin[order(ord)])
  )

cat("=== FIGURE 1: share scoring under 1000 by parent income percentile ===\n")
print(as.data.frame(tab |> select(bin, below1000, took, floor) |>
                      mutate(across(c(below1000, took, floor), ~round(., 1)))))
cat(sprintf("\nbottom decile (0-10): %.1f%% of test-takers under 1000   top 0.1%%: %.1f%%\n",
            tab$floor[1], tab$floor[16]))
cat(sprintf("middle (50-60): %.1f%%   90-95: %.1f%%\n", tab$floor[6], tab$floor[10]))

lab1 <- tab |> filter(bin %in% c("0-10", "50-60", "90-95", "Top 0.1%"))

p1 <- ggplot(tab, aes(bin, floor, group = 1)) +
  geom_line(colour = nfl, linewidth = 1.0) +
  geom_point(colour = nfl, size = 2.6) +
  geom_text(data = lab1, aes(label = sprintf("%.0f%%", floor)),
            vjust = -1.15, size = 3.3, fontface = "bold", colour = ink_body) +
  scale_y_continuous(limits = c(0, 88), breaks = seq(0, 80, 20),
                     labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.02, 0.06))) +
  labs(
    title = "Money buys a floor on the SAT: a rich kid who takes the test rarely scores low, while most low-income test-takers do",
    subtitle = "Among students who took the SAT or ACT, the share who scored under 1000 (on the 1600 scale), by their parents' income percentile.",
    x = "Parents' income percentile", y = "Share of test-takers scoring under 1000",
    caption = fig_caption(
      "Chetty, Deming & Friedman (2023, rev. 2025), NBER Working Paper 31492, Appendix Table A.3 Panel B (distribution of SAT/ACT scores by parent income)",
      "\nFrom published research, not this project's data. For each parent-income bin, the share of test-takers scoring under 1000 is the sum of the paper's five\nsub-1000 ranges (900-990, 800-890, 700-790, 600-690, below 600) divided by the share of kids in that bin who took the SAT or ACT.",
      "\nThe floor falls steeply: about 3 in 4 test-takers from the bottom third of families score under 1000, versus about 1 in 6 in the top 0.1 percent. The\ngap is understated here, because only about a fifth of bottom-decile kids sit the test (versus ~88 percent at the top), and those who do are the more\nprepared ones, so the poor would score even lower if all of them tested.")) +
  theme_hometown(grid = "y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.72)))
save_fig("docs/figures/ba_wealth_floor.png", p1, w = 12, h = 6.4)

# ---------------------------------------------------------------------------
# FIGURE 2 -- the income floor: chance of LANDING in the poorest fifth, by
# parents' income fifth. Chetty, Hendren, Kline & Saez (2014), QJE Table II,
# child bottom-fifth row. If family did not matter, every bar would be 20%.
# ---------------------------------------------------------------------------
mob <- tribble(
  ~short,           ~pct,
  "Poorest\n20%",   33.7,
  "Lower\nmiddle",  24.2,
  "Middle",         17.8,
  "Upper\nmiddle",  13.4,
  "Richest\n20%",   10.9
) |>
  mutate(short = factor(short, levels = c("Poorest\n20%","Lower\nmiddle","Middle","Upper\nmiddle","Richest\n20%")))

cat("\n=== FIGURE 2: chance of landing in the POOREST fifth, by parents' fifth ===\n")
print(as.data.frame(mob))
cat(sprintf("poorest-fifth / richest-fifth ratio: %.2fx  (%.1f%% vs %.1f%%)\n",
            mob$pct[1] / mob$pct[5], mob$pct[1], mob$pct[5]))

p2 <- ggplot(mob, aes(short, pct)) +
  geom_hline(yintercept = 20, linetype = "dashed",
             colour = ink_baseline, linewidth = 0.4) +
  annotate("text", x = "Middle", y = 22.9, hjust = 0.5, size = 3.0, colour = ink_baseline,
           label = "If family did not matter, every bar would sit at 20 percent") +
  geom_col(fill = nfl, width = 0.68) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), vjust = -0.6, size = 3.5,
            fontface = "bold", colour = ink_body) +
  scale_y_continuous(limits = c(0, 38), breaks = seq(0, 30, 10),
                     labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.04))) +
  labs(
    title = "A child from the richest fifth has about a 1-in-9 shot at ending up poorest; from the bottom it is 1-in-3",
    subtitle = "Chance a child lands in the poorest fifth of the income distribution as an adult, by their parents' income fifth.",
    x = "Parents' income fifth", y = "Chance of landing in the poorest income fifth",
    caption = fig_caption(
      "Chetty, Hendren, Kline & Saez (2014), Quarterly Journal of Economics 129(4), Table II National Quintile Transition Matrix (child bottom-fifth row)",
      "\nFrom published research, not this project's data. Based on 9.87 million US children in the 1980 to 1982 birth cohorts, linked to their parents\nthrough tax records.",
      "\nThe floor money buys: a child from the richest fifth has just a 10.9 percent chance of falling to the poorest fifth, less than a third the 33.7\npercent chance faced by a child from the poorest fifth. No sports here, just the plain link from family income to where a child lands as an adult.")) +
  theme_hometown(grid = "y")
save_fig("docs/figures/ba_wealth_floor_income.png", p2, w = 12, h = 6.2)

cat("\ndone\n")
