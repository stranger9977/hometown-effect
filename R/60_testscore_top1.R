# =============================================================================
# 60_testscore_top1.R -- Michael's follow-up: our SAT-by-family-income chart
# topped out at "over $200k," which is barely rich in a lot of cities. He asked
# to look all the way into the TOP 1 PERCENT (and top 0.1 percent) and see
# whether the test-score gradient keeps climbing at the very top. It does, and
# it steepens.
#
# SOURCE: Chetty, Deming & Friedman (2023), "Diversifying Society's Leaders?",
# NBER Working Paper 31492 / Opportunity Insights, Appendix Table A.3, Panel B
# ("Distribution of Test Scores Conditional on Parent Income"). I pulled the
# table out of the paper PDF with pdftotext; the numbers below are typed straight
# from it. The paper reports the DISTRIBUTION of SAT/ACT scores by parent income
# bin, not a single mean, so I plot the cleanest directly-verifiable summary: the
# share of students who scored 1300 or higher (the sum of the paper's top three
# SAT ranges: 1500-1600, 1400-1490, 1300-1390). This is exactly the "strong
# score" cut the paper and its press coverage use. HONEST NOTE: the denominator
# includes students who did not take the test (a larger share of them are lower
# income), exactly as the paper defines it. THIS IS PUBLISHED RESEARCH, NOT OUR
# DATA. No race/ethnicity anywhere.
# =============================================================================

suppressMessages({
  library(dplyr); library(ggplot2); library(tibble)
})
source("R/lib/theme_hometown.R")

nfl <- pal_sport[["NFL"]]

# Appendix Table A.3, Panel B. Columns = parent income percentile bins (national
# distribution). s1500/s1400/s1300 are the three top score-range rows, in percent
# of all students in that bin. Their sum = share scoring 1300 or higher.
tab <- tribble(
  ~bin,       ~ord, ~s1500, ~s1400, ~s1300,
  "0-10",      1,    0.0,    0.1,    0.4,
  "10-20",     2,    0.0,    0.1,    0.4,
  "20-30",     3,    0.1,    0.2,    0.6,
  "30-40",     4,    0.1,    0.2,    0.7,
  "40-50",     5,    0.1,    0.3,    1.1,
  "50-60",     6,    0.2,    0.5,    1.7,
  "60-70",     7,    0.3,    0.8,    2.4,
  "70-80",     8,    0.4,    1.3,    3.6,
  "80-90",     9,    0.8,    2.2,    5.6,
  "90-95",    10,    1.7,    3.8,    8.3,
  "95-96",    11,    2.5,    5.0,   10.5,
  "96-97",    12,    2.8,    5.6,   11.3,
  "97-98",    13,    3.4,    6.4,   12.1,
  "98-99",    14,    3.9,    7.3,   13.1,
  "99-99.9",  15,    4.6,    8.6,   14.3,
  "Top 0.1%", 16,    6.8,   11.6,   14.7
) |>
  mutate(p1300 = s1500 + s1400 + s1300,
         bin = factor(bin, levels = bin[order(ord)]))

cat("=== share scoring 1300+ on SAT (or 29+ ACT) by parent income percentile ===\n")
print(as.data.frame(tab |> select(bin, s1500, s1400, s1300, p1300)))
cat(sprintf("\nmiddle (50-60): %.1f%%   top 1%% band (99-99.9): %.1f%%   top 0.1%%: %.1f%%\n",
            tab$p1300[6], tab$p1300[15], tab$p1300[16]))
cat(sprintf("top 0.1%% is %.0fx the middle (50-60) and %.0fx the bottom tenth (0-10)\n",
            tab$p1300[16] / tab$p1300[6], tab$p1300[16] / tab$p1300[1]))

lab_df <- tab |> filter(bin %in% c("0-10", "50-60", "90-95", "99-99.9", "Top 0.1%"))

p <- ggplot(tab, aes(bin, p1300, group = 1)) +
  geom_line(colour = nfl, linewidth = 1.0) +
  geom_point(colour = nfl, size = 2.6) +
  geom_text(data = lab_df, aes(label = sprintf("%.1f%%", p1300)),
            vjust = -1.1, size = 3.3, fontface = "bold", colour = ink_body) +
  scale_y_continuous(limits = c(0, 37), breaks = seq(0, 30, 10),
                     labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.02, 0.08))) +
  labs(
    title = "The share of kids scoring 1300+ on the SAT climbs with family income and jumps to 1 in 3 at the top 0.1 percent",
    subtitle = "Share of students who scored 1300 or higher on the SAT (or 29 or higher on the ACT), by their parents' income percentile.",
    x = "Parents' income percentile", y = "Share scoring 1300+ on the SAT/ACT",
    caption = fig_caption(
      "Chetty, Deming & Friedman (2023), NBER Working Paper 31492, Appendix Table A.3 Panel B (distribution of SAT/ACT scores by parent income)",
      "\nFrom published research, not this project's data. Share is the sum of the paper's top three score ranges (1500-1600, 1400-1490, 1300-1390) for\neach parent income bin, as a percent of all students in that bin.",
      "\nThe denominator includes students who did not take the test, a larger share of them lower income. The gradient keeps rising and steepens at the very\ntop: about 2.4 percent at the middle (50th to 60th percentile) versus 33 percent in the top 0.1 percent. The broad top 1 percent starts at about\n$611,000 of family income, so the rightmost bins are progressively finer, and richer, slices of the very top.")) +
  theme_hometown(grid = "y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.72)))
save_fig("docs/figures/ba_testscore_top1.png", p, w = 12, h = 6.4)
cat("\ndone\n")
