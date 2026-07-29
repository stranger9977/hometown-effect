# =============================================================================
# 39_income_fades.R -- PUBLISHED-RESEARCH chart (not this project's data).
#
# Does the school-starting-age edge show up in adult earnings? Black, Devereux
# and Salvanes (NBER w13969) track Norwegian cohorts age by age and find that
# any earnings difference tied to when you started school fades to zero by the
# early 30s. Their reported coefficient is the effect of starting school one
# year OLDER (higher school starting age) on log earnings. That effect is
# NEGATIVE at young adult ages (older starters have less labor-market
# experience, so earn a bit less) and climbs to about zero by age 30.
#
# We plot Table 6 (2SLS, all men) verbatim: the exact series their Figure 1
# draws. Every point and standard error below is transcribed from the paper.
# =============================================================================

suppressMessages({
  library(dplyr); library(ggplot2)
})
source("R/lib/theme_hometown.R")

# --- verified estimates, transcribed from the paper --------------------------
# Black, Devereux & Salvanes (2008), NBER Working Paper 13969,
# Table 6 "Effect of School Starting Age on Earnings -- All Men", 2SLS row.
# Coefficient is on LOG earnings (read as approximate percent). One regression
# per age; sample size ranges 243,301 to 247,195 across ages.
bds <- tibble::tribble(
  ~age,  ~coef,   ~se,
  24,   -0.092,  0.013,
  25,   -0.099,  0.013,
  26,   -0.096,  0.011,
  27,   -0.065,  0.011,
  28,   -0.039,  0.011,
  29,   -0.023,  0.009,
  30,   -0.010,  0.008,
  31,   -0.006,  0.009,
  32,   -0.003,  0.009,
  33,    0.006,  0.009,
  34,    0.003,  0.009,
  35,    0.006,  0.007
) |>
  mutate(
    pct    = coef * 100,               # log coefficient read as percent
    lo     = (coef - 1.96 * se) * 100, # approximate 95% interval, +/- 1.96 SE
    hi     = (coef + 1.96 * se) * 100
  )

cat("=== BDS w13969, Table 6, 2SLS all men (effect of starting a year older) ===\n")
print(as.data.frame(bds |> mutate(across(c(pct, lo, hi), \(x) round(x, 1)))))

accent <- "#2166AC"                    # single accent, RdBu blue (below-zero direction)

# points where the interval no longer excludes zero (age 30 onward)
zero_ok <- bds |> filter(age >= 30)

p <- ggplot(bds, aes(age, pct)) +
  geom_baseline(0) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = accent, alpha = 0.14) +
  geom_line(colour = accent, linewidth = 0.9) +
  geom_point(colour = accent, size = 2.6) +
  # highlight the ages where the effect is no longer distinguishable from zero
  geom_point(data = zero_ok, colour = accent, size = 2.6, shape = 21,
             fill = "white", stroke = 0.9) +
  annotate("text", x = 24.05, y = -12.8, hjust = 0,
           label = "At age 24, starting school a year older\nmeant about 9% lower earnings",
           size = 3.5, colour = ink_body, lineheight = 0.95, fontface = "bold") +
  annotate("text", x = 33.5, y = -4.6, hjust = 1,
           label = "By age 30 the gap is no longer\nstatistically different from zero\n(open points, band crosses the line)",
           size = 3.4, colour = ink_body, lineheight = 0.95) +
  annotate("text", x = 35.35, y = 0, hjust = 0, vjust = 0.5,
           label = "no earnings\ndifference", size = 3.1, colour = ink_baseline,
           lineheight = 0.95) +
  scale_x_continuous(breaks = 24:35, expand = expansion(mult = c(0.02, 0.10))) +
  scale_y_continuous(
    breaks = seq(-12, 3, 3),
    labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "%"),
    expand = expansion(mult = c(0.06, 0.08))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Earnings differences tied to school starting age fade to zero by the early 30s",
    subtitle = "Effect of starting school one year older on men's earnings, Norway, measured age by age (instrumental-variable estimate)",
    x = "Age when earnings are measured", y = NULL,
    caption = fig_caption(
      "Black, Devereux and Salvanes (2008), NBER Working Paper 13969, Table 6 (2SLS, all men)",
      paste0("\nEffect of a one year higher school starting age on log earnings, Norwegian men, cohorts born July 1962 to June 1970,",
             "\nestimated separately at each age from 24 to 35 (n from 243,301 to 247,195 per age). Shaded band is the approximate",
             "\n95% interval (estimate plus or minus 1.96 standard errors, both from the table)."),
      paste0("\nFrom published research, not this project's data. Coefficients are on log earnings, read as approximate percent.",
             "\nWomen show a similar but less precisely estimated pattern (their Table 8). Direction note: older starters earn a",
             "\nbit LESS early because they have less work experience at a given age, not more; the difference is gone by about 30."))) +
  theme_hometown(grid = "y") +
  theme(plot.margin = margin(10, 74, 8, 10))

save_fig("docs/figures/ba_income_fades.png", p, w = 11, h = 6.75)

cat("\ndone\n")
