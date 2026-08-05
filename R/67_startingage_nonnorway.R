# =============================================================================
# 67_startingage_nonnorway.R -- PUBLISHED-RESEARCH chart (not this project's
# data). The non-Norway companion to 39_income_fades.R.
#
# Michael asked whether the school-starting-age-and-earnings finding shows up
# anywhere besides Norway. It does, in Sweden. Fredriksson and Ockert study the
# entire Swedish population and, like Black, Devereux and Salvanes in Norway,
# estimate the effect of starting school a year OLDER on adult log earnings.
# The Swedish estimates are smaller but the shape is the same story: starting
# older costs young workers a few percent of earnings (they have a year less
# labor-market experience), that penalty fades with age, and by workers' fifties
# it has actually turned slightly positive (the extra schooling has paid off).
#
# SOURCE: Fredriksson & Ockert (2005), "Is Early Learning Really More
# Productive? The Effect of School Starting Age on School and Labor Market
# Performance", IZA Discussion Paper No. 1659 (also IFAU Working Paper 2006:12),
# Table 8 "IV estimates of school starting age on earnings in 2000", the "All"
# column, Log(Earnings) rows. I pulled the PDF (docs.iza.org/dp1659.pdf) with
# pdftotext and typed the coefficients and standard errors straight from the
# table. School starting age is instrumented with expected school starting age
# from month of birth. Earnings are measured once, in 2000, so each ten-year
# birth cohort is observed at a different age: this is a single cross-section,
# not the same people followed over time. THIS IS PUBLISHED RESEARCH, NOT OURS.
# =============================================================================

suppressMessages({
  library(dplyr); library(ggplot2)
})
source("R/lib/theme_hometown.R")

# --- verified estimates, transcribed from the paper --------------------------
# Table 8, "All" column, Log(Earnings) coefficient and (robust SE) per cohort.
# age_mid is the approximate age of that ten-year cohort in the year 2000, used
# only to place the cohorts left to right; the label keeps the birth years so
# the age/cohort mixing stays visible.
fo <- tibble::tribble(
  ~cohort,     ~age_mid, ~coef,    ~se,      ~n,
  "1970-79",    25,      -0.0278,  0.0019,   954477,
  "1960-69",    35,       0.0029,  0.0021,   1021780,
  "1950-59",    45,       0.0009,  0.0022,   965514,
  "1940-49",    55,       0.0054,  0.0023,   1029797
) |>
  mutate(
    pct = coef * 100,                 # log coefficient read as percent
    lo  = (coef - 1.96 * se) * 100,   # approximate 95% interval, +/- 1.96 SE
    hi  = (coef + 1.96 * se) * 100,
    sig = lo > 0 | hi < 0             # interval excludes zero
  )

# Pooled all-cohort (1935-84) net life-cycle estimate, for the note only.
pooled_coef <- -0.0064; pooled_se <- 0.0048

cat("=== Fredriksson & Ockert, IZA DP1659, Table 8 (All, log earnings) ===\n")
print(as.data.frame(fo |> mutate(across(c(pct, lo, hi), \(x) round(x, 2)))))
cat(sprintf("\npooled 1935-84: %.2f%% (SE %.2f%%), z = %.2f -> %s\n",
            pooled_coef * 100, pooled_se * 100, pooled_coef / pooled_se,
            ifelse(abs(pooled_coef / pooled_se) > 1.96, "significant", "not significant")))

accent <- "#2166AC"                    # same RdBu blue as the Norway chart (paired)

sig_pts <- fo |> filter(sig)
ns_pts  <- fo |> filter(!sig)

p <- ggplot(fo, aes(age_mid, pct)) +
  geom_baseline(0) +
  # thin connecting line: a guide across cohorts, NOT the same people over time
  geom_line(colour = accent, linewidth = 0.6, alpha = 0.5) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 1.6,
                colour = accent, linewidth = 0.6) +
  # significant cohorts: filled; not-significant: open (echoing the Norway chart)
  geom_point(data = sig_pts, colour = accent, size = 2.9) +
  geom_point(data = ns_pts, colour = accent, size = 2.9, shape = 21,
             fill = "white", stroke = 0.9) +
  annotate("text", x = 26.4, y = -2.62, hjust = 0,
           label = "In their 20s, starting school a year older\nmeant about 3% lower earnings",
           size = 3.5, colour = ink_body, lineheight = 0.95, fontface = "bold") +
  annotate("text", x = 39.5, y = 1.28, hjust = 0,
           label = "By their 50s the sign has flipped: workers who\nstarted school older earn a bit more (about +0.5%),\nas the extra schooling finally pays off",
           size = 3.4, colour = ink_body, lineheight = 0.95) +
  annotate("text", x = 57.4, y = 0, hjust = 0, vjust = 0.5,
           label = "no earnings\ndifference", size = 3.1, colour = ink_baseline,
           lineheight = 0.95) +
  scale_x_continuous(
    breaks = c(25, 35, 45, 55),
    labels = c("Their 20s\n(born 1970-79)", "Their 30s\n(born 1960-69)",
               "Their 40s\n(born 1950-59)", "Their 50s\n(born 1940-49)"),
    expand = expansion(mult = c(0.06, 0.15))) +
  scale_y_continuous(
    breaks = seq(-3, 1, 1),
    labels = function(x) paste0(ifelse(x > 0, "+", ""), x, "%"),
    expand = expansion(mult = c(0.08, 0.10))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "In Sweden the earnings penalty from starting school older fades with age and even flips positive",
    subtitle = "Effect of starting school one year older on earnings, Sweden, by birth-cohort age group in a single year (2000), instrumental-variable estimate",
    x = NULL, y = NULL,
    caption = fig_caption(
      "Fredriksson and Ockert (2005), 'Is Early Learning Really More Productive?', IZA Discussion Paper No. 1659 (also IFAU Working Paper 2006:12), Table 8 (IV, all)",
      paste0("\nEffect of a one year higher school starting age on log earnings for the entire Swedish population, by ten-year birth cohort, all measured in the year 2000,",
             "\nso the horizontal axis mixes age with birth cohort: this is a single cross-section, not the same people followed over time. School starting age is instrumented",
             "\nwith expected school starting age from month of birth. Bars are the approximate 95% interval (estimate plus or minus 1.96 standard errors, both from the table)."),
      paste0("\nFrom published research, not this project's data. Coefficients are on log earnings, read as approximate percent; the log-earnings sample keeps only people",
             "\nearning over 100,000 SEK. Filled points are statistically different from zero, open points are not. Pooled across all cohorts the net life-cycle effect is about",
             "\n-0.6% and not statistically significant, which the authors read as a small net cost, since older starters also give up a year of earnings by entering work later."))) +
  theme_hometown(grid = "y") +
  theme(plot.margin = margin(10, 96, 8, 10))

save_fig("docs/figures/ba_startingage_nonnorway.png", p, w = 12, h = 6.9)

cat("\ndone\n")
