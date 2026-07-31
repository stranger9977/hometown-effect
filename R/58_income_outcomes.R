# =============================================================================
# 58_income_outcomes.R -- Michael's feedback: the intuitive "rich family = better
# outcomes" story, for the GENERAL POPULATION, nothing to do with sports. Two
# published, citable relationships between FAMILY INCOME and a child's outcomes:
#
#   (1) TEST SCORES rise with family income. College Board's own 2013 Total Group
#       Profile Report reports mean SAT by family income band: a clean monotonic
#       gradient, 1326 (under $20k) up to 1714 (over $200k) out of 2400.
#
#   (2) FUTURE ADULT INCOME rises with parent income. Chetty, Hendren, Kline &
#       Saez (2014 QJE) National Quintile Transition Matrix: the chance a child
#       reaches the TOP income fifth climbs from 7.5% (poorest-fifth parents) to
#       36.5% (richest-fifth parents), almost a five-fold gap.
#
# BOTH ARE PUBLISHED DATA, NOT THIS PROJECT'S DATA. Numbers are quoted from the
# primary sources (College Board report table; QJE Table II) and reprinted in the
# console below so they can be spot-checked. No race/ethnicity anywhere.
# =============================================================================

suppressMessages({
  library(dplyr); library(ggplot2); library(tibble)
})
source("R/lib/theme_hometown.R")

nfl <- pal_sport[["NFL"]]

# ---------------------------------------------------------------------------
# CHART 1 -- SAT total score by family income band.
# Source: College Board, 2013 College-Bound Seniors Total Group Profile Report.
# Mean section scores (Critical Reading, Mathematics, Writing) read straight from
# the report's family-income table; total = the three sections summed (out of 2400).
# ---------------------------------------------------------------------------
sat <- tribble(
  ~band,             ~short,       ~cr,  ~math, ~writing, ~ord,
  "$0 - $20,000",    "Under\n$20k", 435,  462,  429,      1,
  "$20,000-$40,000", "$20-\n40k",   465,  482,  455,      2,
  "$40,000-$60,000", "$40-\n60k",   487,  500,  474,      3,
  "$60,000-$80,000", "$60-\n80k",   500,  511,  486,      4,
  "$80,000-$100,000","$80-\n100k",  512,  524,  499,      5,
  "$100,000-$120,000","$100-\n120k",522,  536,  511,      6,
  "$120,000-$140,000","$120-\n140k",526,  540,  515,      7,
  "$140,000-$160,000","$140-\n160k",533,  548,  523,      8,
  "$160,000-$200,000","$160-\n200k",539,  555,  531,      9,
  "More than $200,000","Over\n$200k",565, 586,  563,     10
) |>
  mutate(total = cr + math + writing,
         short = factor(short, levels = short[order(ord)]))

cat("=== CHART 1: mean SAT (out of 2400) by family income, 2013 College-Bound Seniors ===\n")
print(as.data.frame(sat |> select(band, cr, math, writing, total)))
cat(sprintf("bottom band %d, top band %d, gap %d points\n\n",
            sat$total[1], sat$total[10], sat$total[10] - sat$total[1]))

p1 <- ggplot(sat, aes(short, total, group = 1)) +
  geom_line(colour = nfl, linewidth = 1.0) +
  geom_point(colour = nfl, size = 2.8) +
  geom_text(aes(label = total), vjust = -1.15, size = 3.1,
            fontface = "bold", colour = ink_body) +
  scale_y_continuous(limits = c(1300, 1770),
                     breaks = seq(1300, 1700, 100),
                     expand = expansion(mult = c(0.02, 0.12))) +
  labs(
    title = "Kids from richer families score higher on the SAT, and it climbs at every step up in family income",
    subtitle = "Average total SAT score (out of 2400) by family income band, 2013 college-bound seniors.",
    x = "Family income", y = "Average total SAT score (out of 2400)",
    caption = fig_caption(
      "College Board 2013 College-Bound Seniors Total Group Profile Report, mean SAT by family income (Critical Reading + Mathematics + Writing)",
      "\nFrom published data, not this project's data. Mean total score, out of 2400, for the roughly 741,000 of 2013's college-bound seniors who\nreported a family income band.",
      "\nThe score rises at every income step, from 1326 for families under $20k to 1714 for families over $200k, a gap of almost 400 points. No sports\nhere: this is the plain link from family money to a child's test scores, for the general population.")) +
  theme_hometown(grid = "y") +
  theme(axis.text.x = element_text(size = rel(0.72), lineheight = 0.85))
save_fig("docs/figures/ba_testscore_income.png", p1, w = 12, h = 5.8)

# ---------------------------------------------------------------------------
# CHART 2 -- chance a child reaches the TOP income fifth, by parents' income fifth.
# Source: Chetty, Hendren, Kline & Saez (2014), QJE 129(4), Table II "National
# Quintile Transition Matrix", the child-top-quintile (row 5) values. 1980-1982
# birth cohorts, 9,867,736 children linked to parents via tax records.
# ---------------------------------------------------------------------------
mob <- tribble(
  ~parent,          ~short,           ~pct,  ~ord,
  "Poorest fifth",  "Poorest\n20%",   7.5,   1,
  "Second fifth",   "Lower\nmiddle",  12.3,  2,
  "Middle fifth",   "Middle",         18.3,  3,
  "Fourth fifth",   "Upper\nmiddle",  25.4,  4,
  "Richest fifth",  "Richest\n20%",   36.5,  5
) |>
  mutate(short = factor(short, levels = short[order(ord)]))

cat("=== CHART 2: chance of reaching the TOP income fifth, by parents' income fifth ===\n")
print(as.data.frame(mob |> select(parent, pct)))
cat(sprintf("richest-fifth / poorest-fifth ratio: %.2fx\n\n", mob$pct[5] / mob$pct[1]))

bench_lab <- tibble(short = factor("Poorest\n20%", levels = levels(mob$short)),
                    y = 33,
                    lab = "If family did not matter, every bar would sit at 20 percent")

p2 <- ggplot(mob, aes(short, pct)) +
  geom_hline(yintercept = 20, linetype = "dashed",
             colour = ink_baseline, linewidth = 0.4) +
  geom_text(data = bench_lab, aes(short, y, label = lab), hjust = 0, nudge_x = -0.42,
            size = 3.0, colour = ink_baseline) +
  geom_col(fill = nfl, width = 0.68) +
  geom_text(aes(label = sprintf("%.1f%%", pct)), vjust = -0.6, size = 3.5,
            fontface = "bold", colour = ink_body) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 10),
                     expand = expansion(mult = c(0, 0.04))) +
  labs(
    title = "A child born into the richest fifth is almost 5 times as likely to end up rich as one born into the poorest",
    subtitle = "Chance a child reaches the top fifth of the income distribution as an adult, by their parents' income fifth.",
    x = "Parents' income fifth", y = "Chance of reaching the top income fifth",
    caption = fig_caption(
      "Chetty, Hendren, Kline & Saez (2014), Quarterly Journal of Economics, Table II National Quintile Transition Matrix (child top-fifth row)",
      "\nFrom published research, not this project's data. Based on 9.87 million US children in the 1980 to 1982 birth cohorts, linked to their\nparents through tax records.",
      "\nA child from the poorest fifth of families has a 7.5 percent shot at the richest fifth; from the richest fifth it is 36.5 percent, almost five\ntimes higher. No sports here: this is the plain link from family income to where a child lands as an adult.")) +
  theme_hometown(grid = "y")
save_fig("docs/figures/ba_mobility.png", p2, w = 12, h = 6.2)

cat("done\n")
