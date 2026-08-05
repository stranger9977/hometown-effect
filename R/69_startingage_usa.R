# =============================================================================
# 69_startingage_usa.R -- Michael/Nick asked to see the UNITED STATES version of
# the school-starting-age earnings question (we already have Norway, R/39, and
# Sweden, R/67). The cleanest US estimate finds essentially NO effect on adult
# earnings, which is the same evaporation the Norway chart shows.
#
# SOURCE: Dobkin & Ferreira (2010), "Do School Entry Laws Affect Educational
# Attainment and Labor Market Outcomes?", Economics of Education Review 29:40-54,
# Table 3A "Impact of school entry laws on long run adult outcomes, Texas."
# Regression-discontinuity estimates at the state school-entry cutoff, 2000
# Decennial Census long form (15% sample), adults aged 30-79. Every coefficient
# and standard error below is typed straight from the paper (pdftotext -layout).
# The log outcomes are shown here as percent effects (coefficient x 100). The
# authors report the same near-zero pattern for California (Table 3B), and null
# effects for employment (-0.06 pts), home ownership (+0.16 pts) and marriage
# (+0.37 pts). Every estimate is statistically and practically insignificant.
# PUBLISHED RESEARCH, NOT THIS PROJECT'S DATA. No race/ethnicity.
# =============================================================================

suppressMessages({library(dplyr); library(ggplot2); library(tibble)})
source("R/lib/theme_hometown.R")
nfl <- pal_sport[["NFL"]]

# Table 3A, the log (percent) outcomes. coef and se are in percent.
d <- tribble(
  ~outcome,               ~coef,   ~se,    ~ord,
  "Hourly wages",          0.09,   0.75,   1,
  "Household income",     -0.37,   0.57,   2,
  "Home value",           -0.64,   0.61,   3
) |>
  mutate(lo = coef - 1.96*se, hi = coef + 1.96*se,
         sig = (lo > 0) | (hi < 0),                 # none are significant
         outcome = factor(outcome, levels = rev(outcome[order(ord)])))

cat("=== US school-entry-timing effect on adult outcomes (Dobkin & Ferreira 2010, TX) ===\n")
print(as.data.frame(d |> select(outcome, coef, se, lo, hi, sig)))

p <- ggplot(d, aes(coef, outcome)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = ink_baseline, linewidth = 0.5) +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0.16, colour = nfl, linewidth = 0.8) +
  geom_point(aes(fill = sig), shape = 21, colour = nfl, size = 3.4, stroke = 1.0) +
  geom_text(aes(label = sprintf("%+.1f%%", coef)), vjust = -1.2, size = 3.2,
            fontface = "bold", colour = ink_body) +
  scale_fill_manual(values = c(`TRUE` = nfl, `FALSE` = "white"), guide = "none") +
  scale_x_continuous(limits = c(-2.6, 2.6), breaks = seq(-2, 2, 1),
                     labels = function(x) sprintf("%+d%%", x)) +
  annotate("text", x = 0, y = 3.5, label = "no effect", hjust = 0.5, vjust = 0,
           size = 3.0, colour = ink_baseline) +
  labs(
    title = "In the US, when you started school leaves adult earnings essentially unchanged",
    subtitle = "Effect of school starting age on adult wages, household income, and home value, US adults aged 30 to 79. Bars are 95% intervals.",
    x = "Effect on the adult outcome (percent)", y = NULL,
    caption = fig_caption(
      "Dobkin & Ferreira (2010), Economics of Education Review 29:40-54, Table 3A (Texas); regression-discontinuity at the school-entry cutoff, 2000 Census",
      "\nFrom published research, not this project's data. Each estimate compares adults born just on either side of their state's school-entry cutoff, so it\nisolates school starting age. Points are the effect on each log outcome, in percent; the bars are the 95% interval. Open points are not different from zero.",
      "\nEvery estimate is a fraction of a percent and statistically indistinguishable from zero. Employment (-0.06 points), home ownership (+0.16) and marriage\n(+0.37) are null too, and California shows the same pattern. Same story as Norway and Sweden: the school-starting-age advantage is gone by adulthood.")) +
  theme_hometown(grid = "none") +
  theme(panel.grid.major.x = element_line(colour = "grey92", linewidth = 0.4))
save_fig("docs/figures/ba_startingage_usa.png", p, w = 11, h = 5.4)
cat("\ndone\n")
