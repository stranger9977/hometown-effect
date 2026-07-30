# =============================================================================
# 53_w23660_testscores.R -- the test-score result from the NBER Florida paper
# Michael flagged (w23660, Dhuey, Figlio, Karbownik & Roth 2017). We already use
# this paper in the effect-fades chart; this pulls out its specific test-score
# finding, which is what he asked about.
#
# The effect of being oldest-in-grade (September vs August birth, under a Sept 1
# cutoff) on pooled math and reading test scores, grades 3 to 8: about 0.2 SD.
# The key strength is that it holds up when comparing SIBLINGS, so it is not
# just family background. Estimates grow slightly as the comparison gets cleaner.
# THIS IS PUBLISHED RESEARCH, NOT OUR DATA, and numbers are quoted from the PDF.
# =============================================================================

suppressMessages({
  library(dplyr); library(ggplot2); library(tibble)
})
source("R/lib/theme_hometown.R")

# Point estimates read from w23660 Table 3 / text (column 1 of each sample):
#   full singletons 0.197 SD; same-mother siblings 0.216; same-father siblings 0.223
est <- tribble(
  ~comparison,                              ~sd,     ~order,
  "All children\n(full sample)",            0.197,   1,
  "Between siblings\n(same mother)",        0.216,   2,
  "Between siblings\n(same father)",        0.223,   3
) |>
  mutate(comparison = factor(comparison, levels = comparison[order(order)]))

cat("w23660 test-score gap (Sept vs Aug birth), SD, by comparison:\n")
print(as.data.frame(est |> select(comparison, sd)))

p <- ggplot(est, aes(sd, comparison)) +
  geom_col(fill = pal_ratio[["low"]], width = 0.6) +
  geom_text(aes(label = sprintf("%.2f SD", sd)), hjust = -0.15, size = 3.8,
            fontface = "bold", colour = ink_body) +
  scale_x_continuous(limits = c(0, 0.28), expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = "The oldest kids in a grade score about 0.2 SD higher on tests, even comparing siblings",
    subtitle = "Effect of a September vs August birth on grade 3 to 8 test scores, from the Florida study Michael flagged (NBER w23660)",
    x = "Test-score gap, standard deviations", y = NULL,
    caption = fig_caption(
      "Dhuey, Figlio, Karbownik & Roth (2017), NBER Working Paper 23660, Table 3 (September vs August birth, pooled math and reading, grades 3 to 8)",
      "\nFrom published research, not this project's data. All three estimates are highly significant. The paper calls the gap remarkably stable, always around 0.2 SD",
      "\nacross grades 3 to 8. Because it holds between brothers and sisters in the same family, it is not just family background, it is the age line itself. This is the\ntest-score version of the same relative age effect in the fading-across-a-life chart above.")) +
  theme_hometown(grid = "none") +
  theme(axis.text.y = element_text(lineheight = 0.9))
save_fig("docs/figures/ba_testscore_gap.png", p, w = 11, h = 4.6)
cat("\ndone\n")
