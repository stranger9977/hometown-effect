# =============================================================================
# 59_income_outcomes_overtime.R -- Michael's follow-up: show the two family-income
# -> outcome relationships OVER TIME. Same rule as 58: published, citable data
# only, every number verified from the primary source, general population, no
# sports, no race/ethnicity.
#
#   (1) TEST-SCORE income gap OVER TIME (widening). Sean Reardon (2011), "The
#       Widening Academic Achievement Gap Between the Rich and the Poor," Figure
#       5.1 (90/10 income achievement gap in reading, in SD, by birth cohort,
#       1943-2001). Verified magnitudes: about 0.9 SD for cohorts of the 1950s to
#       early 1970s, rising to about 1.25 SD for cohorts born about 2000, roughly
#       40 percent larger. HONEST NOTE: the online appendix with the 12 per-study
#       point estimates (Russell Sage table 5.A1) is dead (404), so I plot ONLY
#       the verified plateau and the verified recent level, not invented
#       intermediate cohort values. 0.9 SD and 1.25 SD are quoted from Conwell
#       (2022), which reproduces Reardon's Figure 1; the shape (flat 1950s to
#       mid-1970s, then rising) is quoted from Reardon's own text.
#
#   (2) ABSOLUTE MOBILITY OVER TIME (fading American dream). Chetty, Grusky, Hell,
#       Hendren, Manduca & Narang (2017), Science 356(6336). The full annual
#       series (fraction of children who out-earn their parents, by birth cohort
#       1940-1984) is the "cohort_mean" column of Opportunity Insights Online
#       Data Table 1 (table1_national_absmob_by_cohort_parpctile), read straight
#       from the file. Every one of the 45 annual values below is from that table.
# =============================================================================

suppressMessages({
  library(dplyr); library(ggplot2); library(tibble)
})
source("R/lib/theme_hometown.R")

nfl <- pal_sport[["NFL"]]

# ---------------------------------------------------------------------------
# CHART 1 -- 90/10 income achievement gap (reading) over birth cohorts.
# Verified anchors only: 0.90 SD plateau for 1950s-early1970s cohorts (Conwell
# 2022, reproducing Reardon 2011), rising to 1.25 SD for cohorts born ~2000.
# The rise begins with cohorts born in the mid-1970s (Reardon 2011 text).
# ---------------------------------------------------------------------------
gap_line <- tribble(
  ~cohort, ~sd,
  1955,    0.90,   # "about 0.9 SD ... children born in the 1950s, 1960s, and
  1965,    0.90,   #  early 1970s" (Conwell 2022)
  1972,    0.90,
  1975,    0.90,   # "gap began to widen ... cohorts born in the mid-1970s"
  2001,    1.25    # "roughly 1.25 standard deviations" for cohorts born ~2000
)
gap_pts <- tribble(
  ~cohort, ~sd,  ~lab,
  1955,    0.90, NA,
  1965,    0.90, "about 0.9 SD",
  1972,    0.90, NA,
  2001,    1.25, "about 1.25 SD"
)

cat("=== CHART 1: 90/10 income achievement gap in reading, SD, by birth cohort ===\n")
cat("verified plateau (1950s to early 1970s cohorts): 0.90 SD\n")
cat("verified recent level (cohorts born ~2000):       1.25 SD  (about 40% larger)\n")
cat("shape: flat 1950s to mid-1970s, then rising (Reardon 2011); early-1940s gap even smaller\n")
cat("       (2001 gap about 75% larger than early-1940s gap, Reardon 2011 text)\n\n")

p1 <- ggplot() +
  geom_line(data = gap_line, aes(cohort, sd), colour = nfl, linewidth = 1.0) +
  geom_point(data = gap_pts, aes(cohort, sd), colour = nfl, size = 2.8) +
  geom_text(data = filter(gap_pts, !is.na(lab)), aes(cohort, sd, label = lab),
            vjust = -1.15, size = 3.4, fontface = "bold", colour = ink_body) +
  annotate("text", x = 1976.5, y = 1.03, hjust = 0, size = 3.0, colour = ink_baseline,
           label = "Widening begins with\ncohorts born in the mid-1970s") +
  scale_x_continuous(breaks = seq(1950, 2000, 10), limits = c(1951, 2006)) +
  scale_y_continuous(limits = c(0.6, 1.4), breaks = seq(0.6, 1.4, 0.2),
                     expand = expansion(mult = c(0.02, 0.06))) +
  labs(
    title = "The test-score gap between rich and poor kids keeps widening: it is about 40 percent bigger than a generation ago",
    subtitle = "The reading-score gap between children from the richest and poorest tenth of families, in standard deviations, by birth cohort.",
    x = "Child birth cohort", y = "Rich-poor test-score gap (standard deviations)",
    caption = fig_caption(
      "Reardon (2011), 90/10 income achievement gap in reading (Figure 5.1), as reported in Conwell (2022), from 12 nationally representative studies",
      "\nFrom published research, not this project's data. The gap is in standard deviations of test score, so 1.0 means the average child from a top-income\nfamily scores a full standard deviation above the average child from a bottom-income family.",
      "\nVerified levels only: about 0.9 SD for cohorts of the 1950s to early 1970s, rising to about 1.25 SD for cohorts born around 2000, roughly 40 percent\nlarger. The rise starts with cohorts born in the mid-1970s. Intermediate cohorts are drawn as that stated plateau and rise, not as separate estimates.")) +
  theme_hometown(grid = "y")
save_fig("docs/figures/ba_testscore_income_overtime.png", p1, w = 12, h = 6.0)

# ---------------------------------------------------------------------------
# CHART 2 -- absolute income mobility over birth cohorts, 1940-1984.
# Full annual series = "cohort_mean" (fraction of children earning more than their
# parents) from Opportunity Insights Online Data Table 1. Every value verified.
# ---------------------------------------------------------------------------
mob <- tribble(
  ~cohort, ~pct,
  1940, 91.5, 1941, 89.1, 1942, 89.6, 1943, 88.8, 1944, 89.9,
  1945, 86.4, 1946, 85.8, 1947, 84.0, 1948, 82.1, 1949, 79.6,
  1950, 78.5, 1951, 78.4, 1952, 74.0, 1953, 70.8, 1954, 68.2,
  1955, 69.6, 1956, 67.4, 1957, 67.3, 1958, 67.1, 1959, 65.3,
  1960, 62.3, 1961, 60.1, 1962, 58.3, 1963, 57.6, 1964, 56.5,
  1965, 59.3, 1966, 57.5, 1967, 57.8, 1968, 60.1, 1969, 59.4,
  1970, 61.0, 1971, 61.2, 1972, 60.8, 1973, 59.9, 1974, 58.0,
  1975, 58.6, 1976, 54.9, 1977, 56.6, 1978, 55.7, 1979, 54.3,
  1980, 50.0, 1981, 53.2, 1982, 54.3, 1983, 52.7, 1984, 50.3
)
decades <- mob |> filter(cohort %in% c(1940, 1950, 1960, 1970, 1980))
endpt   <- mob |> filter(cohort == 1984)

cat("=== CHART 2: fraction of children earning more than their parents, by birth cohort ===\n")
print(as.data.frame(mob |> filter(cohort %in% c(1940,1950,1960,1970,1980,1984))))
cat(sprintf("full series spans %.1f%% (1940) down to %.1f%% (1984)\n\n", mob$pct[1], mob$pct[45]))

p2 <- ggplot(mob, aes(cohort, pct)) +
  geom_line(colour = nfl, linewidth = 1.0) +
  geom_point(data = decades, colour = nfl, size = 2.8) +
  geom_point(data = endpt, colour = nfl, size = 2.8) +
  geom_text(data = decades, aes(label = sprintf("%.0f%%", pct)),
            vjust = -1.15, size = 3.3, fontface = "bold", colour = ink_body) +
  geom_text(data = endpt, aes(label = sprintf("%.0f%%\nby 1984", pct)),
            hjust = 0, nudge_x = 0.8, size = 3.3, fontface = "bold",
            colour = ink_body, lineheight = 0.9) +
  scale_x_continuous(breaks = seq(1940, 1980, 10), limits = c(1939, 1990)) +
  scale_y_continuous(limits = c(40, 96), breaks = seq(40, 90, 10),
                     labels = function(x) paste0(x, "%")) +
  labs(
    title = "Fewer and fewer kids grow up to out-earn their parents: the share fell from about 90 percent to about 50 percent",
    subtitle = "Share of US children who earn more than their parents did at the same age, adjusted for inflation, by the child's birth year.",
    x = "Child birth cohort", y = "Share who out-earn their parents",
    caption = fig_caption(
      "Chetty, Grusky, Hell, Hendren, Manduca & Narang (2017), Science 356(6336); Opportunity Insights Online Data Table 1, national mean by cohort",
      "\nFrom published research, not this project's data. Full annual series, birth cohorts 1940 to 1984. Incomes compared at age 30 in real\ninflation-adjusted dollars; children whose parents had zero income are included.",
      "\nThe share fell from 91.5 percent for the 1940 cohort to 50.3 percent for 1984, with the steepest drop for cohorts born between 1940 and the mid-\n1960s. Labeled points mark the decade cohorts. No sports here: this is the general fading of the American dream, over time.")) +
  theme_hometown(grid = "y")
save_fig("docs/figures/ba_mobility_overtime.png", p2, w = 12, h = 6.0)

cat("done\n")
