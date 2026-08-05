# =============================================================================
# 68_testscore_income_2023.R -- Michael flagged that our SAT-by-family-income
# chart (R/58) is 2013 College Board data and that the study was refreshed in
# 2023 with much more. This is the modern replacement, built from the 2023
# Opportunity Insights college-admissions study.
#
# SOURCE: Chetty, Deming & Friedman (2023), "Diversifying Society's Leaders? The
# Determinants and Causal Effects of Admission to Highly Selective Private
# Colleges" (July 2023) / NBER Working Paper 31492. Appendix Table A.3, Panel B
# ("Distribution of Test Scores Conditional on Parent Income"). I pulled the
# table straight out of the paper PDF with pdftotext; every share below is typed
# from it and printed to the console for spot-checking.
#
# WHY THIS IS THE RIGHT SUCCESSOR (not a duplicate of R/60): the 2013 chart in
# R/58 plotted the MEAN SAT among test-takers by family-income BAND (topping out
# at "over $200k"). R/60 already plotted the SHARE scoring 1300+ by parent-income
# PERCENTILE. This chart rebuilds the 2013 chart's own metric -- the average SAT
# score -- but on the 2023 data's much finer parent-income percentile grid, all
# the way into the top 0.1 percent. It is the direct, current version of R/58.
#
# HOW THE MEAN IS BUILT (fully transparent, no invented numbers): Table A.3 gives
# the SHARE of students in each 100-point SAT range for each parent-income bin.
# The average score among test-takers in a bin is the share-weighted mean of the
# range midpoints, dropping the "did not take" row and renormalizing over the
# students who actually tested. Midpoints are the obvious ones (1500-1600 -> 1550,
# etc.); the only open bin is "below 600," set to 500 (its shares are tiny, <=2.5%,
# so it barely moves the mean). Every input share is real and from the table; the
# mean is a plain weighted average of those real shares, disclosed in the caption.
#
# SCALE NOTE: these are current-format SAT scores (out of 1600). The 2013 chart in
# R/58 is on the old three-section scale (out of 2400), so the two are NOT
# comparable in absolute points; what carries over is the shape -- a clean climb
# at every income step -- now measured into the very top of the distribution.
# THIS IS PUBLISHED RESEARCH, NOT OUR DATA. No race/ethnicity anywhere.
# =============================================================================

suppressMessages({
  library(dplyr); library(ggplot2); library(tibble)
})
source("R/lib/theme_hometown.R")

nfl <- pal_sport[["NFL"]]

# Range midpoints (SAT total, current 1600 scale). "below 600" -> 500.
mid <- c(s1550 = 1550, s1445 = 1445, s1345 = 1345, s1245 = 1245,
         s1145 = 1145, s1045 = 1045, s945 = 945, s845 = 845,
         s745 = 745, s645 = 645, s500 = 500)

# Appendix Table A.3, Panel B. Columns are parent-income percentile bins in the
# national distribution. Each s* value is the share (percent of ALL students in
# that bin, test-takers and not) scoring in that SAT range; dntake is the share
# who took neither the SAT nor the ACT. Typed straight from the paper.
tab <- tribble(
  ~bin,       ~ord, ~s1550, ~s1445, ~s1345, ~s1245, ~s1145, ~s1045, ~s945, ~s845, ~s745, ~s645, ~s500, ~dntake,
  "0-10",      1,    0.0,   0.1,    0.4,    0.7,    1.6,    2.3,    3.8,   3.9,   3.6,   2.5,   1.2,   79.8,
  "10-20",     2,    0.0,   0.1,    0.4,    0.8,    2.0,    2.9,    5.2,   5.6,   5.4,   3.7,   1.8,   71.9,
  "20-30",     3,    0.1,   0.2,    0.6,    1.1,    2.5,    3.5,    6.1,   6.4,   5.8,   3.8,   1.7,   68.3,
  "30-40",     4,    0.1,   0.2,    0.7,    1.4,    3.1,    4.3,    7.2,   7.1,   6.0,   3.7,   1.6,   64.5,
  "40-50",     5,    0.1,   0.3,    1.1,    2.0,    4.3,    5.7,    8.7,   7.8,   5.9,   3.3,   1.4,   59.5,
  "50-60",     6,    0.2,   0.5,    1.7,    2.9,    6.0,    7.4,   10.4,   8.3,   5.7,   2.9,   1.0,   53.1,
  "60-70",     7,    0.3,   0.8,    2.4,    4.0,    8.1,    9.4,   12.3,   8.8,   5.3,   2.4,   0.8,   45.6,
  "70-80",     8,    0.4,   1.3,    3.6,    5.7,   10.7,   11.6,   14.0,   9.0,   4.8,   2.0,   0.6,   36.4,
  "80-90",     9,    0.8,   2.2,    5.6,    8.3,   14.1,   13.9,   14.7,   8.6,   4.1,   1.5,   0.4,   25.8,
  "90-95",    10,    1.7,   3.8,    8.3,   11.4,   17.1,   15.2,   14.0,   7.3,   3.0,   1.0,   0.3,   16.9,
  "95-96",    11,    2.5,   5.0,   10.5,   13.2,   18.2,   15.1,   12.8,   6.3,   2.4,   0.8,   0.2,   13.2,
  "96-97",    12,    2.8,   5.6,   11.3,   14.0,   18.5,   14.8,   12.2,   5.7,   2.2,   0.7,   0.2,   12.2,
  "97-98",    13,    3.4,   6.4,   12.1,   14.5,   18.8,   14.4,   11.4,   5.3,   2.0,   0.6,   0.2,   11.1,
  "98-99",    14,    3.9,   7.3,   13.1,   15.2,   19.1,   13.6,   10.6,   4.7,   1.8,   0.6,   0.2,   10.3,
  "99-99.9",  15,    4.6,   8.6,   14.3,   15.6,   18.4,   12.7,    9.6,   4.2,   1.5,   0.5,   0.2,   10.3,
  "Top 0.1%", 16,    6.8,  11.6,   14.7,   14.7,   16.2,   10.6,    8.3,   3.8,   1.3,   0.4,   0.2,   12.1
) |>
  mutate(
    # share who tested (renormalization denominator) and mean score among them
    took   = s1550 + s1445 + s1345 + s1245 + s1145 + s1045 + s945 + s845 + s745 + s645 + s500,
    meansc = (s1550*mid["s1550"] + s1445*mid["s1445"] + s1345*mid["s1345"] +
              s1245*mid["s1245"] + s1145*mid["s1145"] + s1045*mid["s1045"] +
              s945*mid["s945"]   + s845*mid["s845"]   + s745*mid["s745"]   +
              s645*mid["s645"]   + s500*mid["s500"]) / took,
    bin = factor(bin, levels = bin[order(ord)])
  )

cat("=== mean SAT among test-takers (1600 scale) by parent income percentile ===\n")
print(as.data.frame(tab |> mutate(meansc = round(meansc, 0),
                                  took = round(took, 1),
                                  dntake = dntake) |>
                      select(bin, took, dntake, meansc)))
cat(sprintf("\nmonotonic climb: %s\n", all(diff(tab$meansc) > 0)))
cat(sprintf("bottom tenth (0-10): %.0f   middle (50-60): %.0f   top 0.1%%: %.0f   gap: %.0f pts\n",
            tab$meansc[1], tab$meansc[6], tab$meansc[16],
            tab$meansc[16] - tab$meansc[1]))
cat(sprintf("share who even took the test: %.1f%% (0-10) vs %.1f%% (top 0.1%%)\n",
            100 - tab$dntake[1], 100 - tab$dntake[16]))

lab_df <- tab |> filter(bin %in% c("0-10", "50-60", "90-95", "99-99.9", "Top 0.1%"))

p <- ggplot(tab, aes(bin, meansc, group = 1)) +
  geom_line(colour = nfl, linewidth = 1.0) +
  geom_point(colour = nfl, size = 2.6) +
  geom_text(data = lab_df, aes(label = sprintf("%.0f", meansc)),
            vjust = -1.1, size = 3.3, fontface = "bold", colour = ink_body) +
  scale_y_continuous(limits = c(840, 1270), breaks = seq(900, 1200, 100),
                     expand = expansion(mult = c(0.02, 0.10))) +
  labs(
    title = "Even among kids who take the SAT, the average score climbs with family income, right into the top 0.1 percent",
    subtitle = "Average SAT total score (out of 1600) among students who took the test, by their parents' income percentile. This is the 2023 successor to our 2013 chart.",
    x = "Parents' income percentile", y = "Average SAT score among test-takers (out of 1600)",
    caption = fig_caption(
      "Chetty, Deming & Friedman (2023), NBER Working Paper 31492, Appendix Table A.3 Panel B (SAT/ACT score distribution by parent income); dollar\nanchors from Appendix Table A.4",
      "\nFrom published research, not this project's data. The average is the share-weighted mean of the table's SAT-range midpoints (1500-1600 scored as\n1550, and so on; the tiny below-600 group as 500), computed over students who actually took the test in each parent-income bin.",
      "\nScores are on the current 1600 scale, so they do not compare in absolute points to our 2013 chart on the old 2400 scale; the shape is what carries\nover. The very poorest tenth actually sits a touch above the next tenth: only about 20 percent of the poorest tenth took the test at all, versus about\n88 percent of the top 0.1 percent, so its average reflects a small, self-selected group. For reference, the 50th percentile of family income is about\n$58,000 and the top 1 percent starts near $611,000 (Table A.4).")) +
  theme_hometown(grid = "y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.72)))
save_fig("docs/figures/ba_testscore_income_2023.png", p, w = 12, h = 6.4)
cat("\ndone\n")
