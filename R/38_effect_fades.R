# =============================================================================
# 38_effect_fades.R -- the relative age effect shrinking with age. REBUILT to be
# easier to read after Michael found the first version confusing. The fixes:
#   - one clean declining line, no "error bars" (the old ones were country
#     ranges that looked like confidence intervals)
#   - each point labeled in plain words with its own unit
#   - no curved annotation arrow, no dense mixed-unit axis
#   - an honest note that the stages use different measures, so read the shrink,
#     not the exact point-to-point difference
# Every number is quoted from published research (verified from the PDFs):
#   Bedard & Dhuey (2006) QJE 121(4) for the grade 4 and grade 8 test gaps;
#   Dhuey, Figlio, Karbownik & Roth (2017) NBER 23660 for kindergarten
#   readiness (+10 pts) and college attendance (+1.3 pts).
# =============================================================================

suppressMessages({
  library(dplyr); library(ggplot2); library(tibble)
})
source("R/lib/theme_hometown.R")

stages <- tribble(
  ~order, ~stage,                          ~value, ~vlab,
  1L,     "Kindergarten",                   10.0,  "+10 points\nmore ready for school",
  2L,     "Grade 4",                         8.0,  "+4 to 12\npercentile points on tests",
  3L,     "Grade 8",                         5.5,  "+2 to 9\npercentile points on tests",
  4L,     "College entry",                   1.3,  "+1.3 points\nmore likely to start college"
) |>
  mutate(stage = factor(stage, levels = stage))

p <- ggplot(stages, aes(order, value)) +
  geom_line(colour = pal_ratio[["low"]], linewidth = 1.1) +
  geom_point(colour = pal_ratio[["low"]], size = 4.4) +
  geom_text(aes(label = vlab), hjust = c(0, 0.5, 0.5, 1), vjust = -0.55,
            lineheight = 0.95, size = 3.4, fontface = "bold", colour = ink_body) +
  scale_x_continuous(breaks = stages$order, labels = stages$stage,
                     expand = expansion(mult = c(0.1, 0.1))) +
  scale_y_continuous(limits = c(0, 14), breaks = seq(0, 12, 4),
                     expand = expansion(mult = c(0, 0.06))) +
  labs(
    title = "The oldest kids in a grade start well ahead, and the head start shrinks as they grow up",
    subtitle = "How far ahead the relatively oldest students are, at four stages of school. It shrinks toward zero but never quite gets there.",
    x = NULL, y = "Oldest kids' advantage",
    caption = fig_caption(
      "Bedard & Dhuey (2006), Quarterly Journal of Economics 121(4); Dhuey, Figlio, Karbownik & Roth (2017), NBER Working Paper 23660",
      "\nFrom published research, not this project's data. The two middle points are oldest-vs-youngest test-score gaps in percentile points (across about 19",
      "\ncountries); the two ends are September-vs-August gaps on yes-or-no outcomes in percentage points, from the Florida study. Different measures, so read the\nshrink over time, not the exact point-to-point gaps. The edge fades but stays above zero, and on test scores it holds especially steady (see the test-score card).")) +
  theme_hometown(grid = "y") +
  theme(axis.text.x = element_text(size = rel(0.9)))
save_fig("docs/figures/ba_effect_fades.png", p, w = 12, h = 6.4)
cat("done\n")
