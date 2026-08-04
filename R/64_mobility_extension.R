# =============================================================================
# 64_mobility_extension.R -- Michael's question: is there absolute-mobility data
# past the 1984 birth cohort, and did the share keep dropping? Chetty et al.
# (2017) stop at 1984 because you can only see who out-earns their parents once
# they turn 30 (their tax data ended 2014). We extend the series to the 1994
# cohort by rebuilding Chetty's method with newer survey data:
#
#   METHOD (Chetty et al. 2017, "The Fading American Dream"): absolute mobility
#   = fraction of kids who out-earn their parents, comparing the kid's household
#   income at age 30 to the parents' household income at ~age 30, in real 2014
#   dollars. Computed as  frac0 + (1-frac0) * mean_i sum_k C[k,i]*1(kid_min[k] >=
#   par_mean[i]),  where C is Chetty's fixed 100x100 parent-child rank copula.
#
#   OUR REBUILD (see R/64_mobility_ipums_build.R for the exact code):
#     - Copula + method: Chetty et al. (2017) replication package, Harvard
#       Dataverse doi:10.7910/DVN/B9TEWM. We reproduce their published 1940-1984
#       series from their copula + marginals to within 0.4 pp (engine check).
#     - New marginals: IPUMS CPS ASEC (Flood et al. 2025, v13.0). Parent = couple
#       income of a newborn's parents (richer parent 25-35); kid = age-30 couple
#       income, immigrants dropped; income = (person+spouse) INCTOT minus
#       INCWELFR, real 2014$ (CPI-U-RS, spliced to CPI-U after 2014).
#     - VALIDATION: rebuilding cohorts 1980-1983 from IPUMS reproduces Chetty's
#       published values with a mean offset of -1.7 pp (our zero-income-parent
#       share runs a touch low). We calibrate the new cohorts up by that same
#       1.7 pp so they sit on Chetty's level at the 1984 splice. Per-cohort
#       estimates are noisy (each age-30 sample is ~1,500-3,000 people).
#
#   FINDING: the decline stops. For cohorts born after 1984 the share holds near
#   half (low-50s%) and edges up for the most recent cohorts, rather than
#   continuing to fall. HONEST OR BUST: this is our reconstruction, not Chetty's
#   published number; it is calibrated and noisy, and the recent cohorts are
#   measured across the volatile 2020-2023 period.
#
#   Full IPUMS CPS citation: Sarah Flood, Miriam King, Renae Rodgers, Steven
#   Ruggles, J. Robert Warren, Daniel Backman, Etienne Breton, Grace Cooper,
#   Julia A. Rivera Drew, Stephanie Richards, David Van Riper, and Kari C.W.
#   Williams. IPUMS CPS: Version 13.0 [dataset]. Minneapolis, MN: IPUMS, 2025.
#   https://doi.org/10.18128/D030.V13.0
# =============================================================================

suppressMessages({library(dplyr); library(ggplot2); library(tibble)})
source("R/lib/theme_hometown.R")
nfl <- pal_sport[["NFL"]]; ext_col <- pal_sport[["MLB"]]   # blue = published, orange = our extension

# Chetty et al. (2017), Opportunity Insights Online Data Table 1 (published).
pub <- tribble(
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
# Our extension, calibrated (+1.7 pp) to Chetty's level at the overlap (R/64_mobility_ipums_build.R).
ours <- tribble(
  ~cohort, ~pct,
  1985, 51.0, 1986, 51.5, 1987, 51.1, 1988, 53.1, 1989, 52.7,
  1990, 51.7, 1991, 55.0, 1992, 56.7, 1993, 52.7, 1994, 58.4
)
# connect the extension to Chetty's 1984 endpoint
ours_line <- bind_rows(pub[pub$cohort==1984,], ours)

decades <- pub |> filter(cohort %in% c(1940,1950,1960,1970,1980))
lab84   <- pub |> filter(cohort==1984)
lab94   <- ours |> filter(cohort==1994)

p <- ggplot() +
  # published Chetty series
  geom_line(data = pub, aes(cohort, pct), colour = nfl, linewidth = 1.0) +
  geom_point(data = decades, aes(cohort, pct), colour = nfl, size = 2.6) +
  geom_text(data = decades, aes(cohort, pct, label = sprintf("%.0f%%", pct)),
            vjust = -1.1, size = 3.2, fontface = "bold", colour = ink_body) +
  # our extension
  geom_line(data = ours_line, aes(cohort, pct), colour = ext_col, linewidth = 1.0) +
  geom_point(data = ours, aes(cohort, pct), colour = ext_col, size = 2.3) +
  geom_text(data = lab94, aes(cohort, pct, label = sprintf("%.0f%%\nby 1994", pct)),
            hjust = 0, nudge_x = 0.7, size = 3.1, fontface = "bold", colour = ink_body, lineheight = 0.9) +
  annotate("segment", x = 1984.5, xend = 1984.5, y = 44, yend = 74,
           linetype = "22", colour = ink_baseline, linewidth = 0.4) +
  annotate("text", x = 1983.5, y = 82, hjust = 1, size = 3.0, colour = nfl, fontface = "bold",
           label = "Chetty (published)\nto the 1984 cohort") +
  annotate("text", x = 1985.5, y = 82, hjust = 0, size = 3.0, colour = ext_col, fontface = "bold",
           label = "Our extension\n(IPUMS CPS + Chetty method)") +
  annotate("text", x = 1966, y = 49, hjust = 0.5, size = 3.1, colour = ink_body,
           label = "The half-century slide levels off after 1984") +
  scale_x_continuous(breaks = seq(1940, 1990, 10), limits = c(1939, 1999)) +
  scale_y_continuous(limits = c(44, 96), breaks = seq(50, 90, 10), labels = function(x) paste0(x, "%")) +
  labs(
    title = "After a half-century slide, the share of kids out-earning their parents leveled off for those born after 1984",
    subtitle = "Share of US children who earn more than their parents did at age 30, adjusted for inflation, by the child's birth year.",
    x = "Child birth cohort", y = "Share who out-earn their parents",
    caption = fig_caption(
      "Chetty, Grusky, Hell, Hendren, Manduca & Narang (2017), Science 356 (cohorts to 1984); our extension from IPUMS CPS (Flood et al. 2025), University of Minnesota, ipums.org",
      "\nCohorts to 1984 are Chetty's published series. Cohorts 1985-1994 are our rebuild of his exact method (his fixed parent-child copula plus fresh CPS income\ndistributions), calibrated up 1.7 points to match his level where the two overlap (1980-1983). Each new point is one birth cohort measured at age 30.",
      "\nThe decline halts: for kids born after 1984 the share holds in the low-50s percent and edges up for the most recent cohorts, rather than falling further.\nPer-cohort estimates are noisy (age-30 samples are 1,500-3,000 people) and the newest cohorts are measured across the volatile 2020-2023 economy. This is\nour reconstruction, not an official update.")) +
  theme_hometown(grid = "y")
save_fig("docs/figures/ba_mobility_extended.png", p, w = 12, h = 6.4)
cat("wrote ba_mobility_extended.png\n")
