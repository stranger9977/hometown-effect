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
# Reardon's own reported anchor levels, so the shape is his three-phase trend
# (rise across the 1940s, plateau, steady climb from the mid-1970s), not a
# two-point interpolation. early-1940s 0.73 is derived from his statement that
# the 2001 gap is "roughly 75 percent larger than the early 1940s" (1.27/1.75).
# 0.88 (1974) and 1.27 (2001) are his Model 4 regression estimates; the ~0.9
# plateau is his stated level for the 1950s-early1970s cohorts.
gap_line <- tribble(
  ~cohort, ~sd,
  1943,    0.73,   # early-1940s: 1.27 / 1.75 (Reardon: 2001 gap ~75% larger)
  1953,    0.90,   # plateau begins in the 1950s at about 0.9
  1974,    0.88,   # NELS cohort, Reardon Model 4 (exact)
  2001,    1.27    # ECLS-B cohort, Reardon Model 4 (exact)
)
gap_pts <- tribble(
  ~cohort, ~sd,  ~lab,
  1943,    0.73, "about 0.73",
  1953,    0.90, "about 0.9",
  1974,    0.88, "0.88",
  2001,    1.27, "1.27"
)

cat("=== CHART 1: 90/10 income achievement gap in reading, SD, by birth cohort ===\n")
cat("early-1940s cohort: about 0.73 (1.27/1.75; Reardon: 2001 gap ~75% larger than early 1940s)\n")
cat("plateau 1950s-early1970s: about 0.90; 1974 cohort 0.88 (Model 4)\n")
cat("2001 cohort: 1.27 (Model 4). Shape: rise across 1940s, flat to mid-1970s, steady climb after.\n\n")

p1 <- ggplot() +
  geom_line(data = gap_line, aes(cohort, sd), colour = nfl, linewidth = 1.0) +
  geom_point(data = gap_pts, aes(cohort, sd), colour = nfl, size = 2.8) +
  geom_text(data = gap_pts, aes(cohort, sd, label = lab),
            vjust = -1.15, size = 3.3, fontface = "bold", colour = ink_body) +
  annotate("text", x = 1955, y = 0.66, hjust = 0, size = 3.0, colour = ink_baseline,
           label = "Flat through the early 1970s,\nthen a steady climb to 2001") +
  scale_x_continuous(breaks = seq(1940, 2000, 10), limits = c(1940, 2006)) +
  scale_y_continuous(limits = c(0.6, 1.4), breaks = seq(0.6, 1.4, 0.2),
                     expand = expansion(mult = c(0.02, 0.06))) +
  labs(
    title = "The test-score gap between rich and poor kids keeps widening: it is about 40 percent bigger than a generation ago",
    subtitle = "The reading-score gap between children from the richest and poorest tenth of families, in standard deviations, by birth cohort.",
    x = "Child birth cohort", y = "Rich-poor test-score gap (standard deviations)",
    caption = fig_caption(
      "Reardon (2011), 90/10 income achievement gap in reading, from 13 nationally representative studies (Figure 5.1; summary in Reardon, FRBSF Community Investments 2012)",
      "\nFrom published research, not this project's data. The gap is in standard deviations, so 1.0 means the average child from a top-tenth-income family scores\na full standard deviation above one from a bottom-tenth family. The real underlying data is a scatter of individual study estimates, not a smooth line.",
      "\nThe points are Reardon's reported levels: about 0.73 for the early-1940s cohort (his '2001 gap is ~75 percent larger than the early 1940s'), the about-0.9\nplateau for 1950s to early-1970s cohorts, and his regression estimates of 0.88 for the 1974 cohort and 1.27 for 2001. The line traces his described trend\n(a rise across the 1940s, a plateau, then a steady climb from the mid-1970s), not invented per-year values.")) +
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
