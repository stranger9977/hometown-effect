# =============================================================================
# 38_effect_fades.R -- the relative age effect (oldest vs youngest in a school
# cohort) shrinks as people age, but never fully disappears.
#
# THIS CHART IS PUBLISHED RESEARCH, NOT THIS PROJECT'S DATA. Every number is
# quoted from one of two papers; nothing is simulated, interpolated, or fitted.
#
# Sources and exact figures (each point is annotated on the chart with its own
# unit, because the stages are measured on different scales):
#
#   Dhuey, Figlio, Karbownik & Roth (2017), "School Starting Age and Cognitive
#   Development", NBER Working Paper 23660. Florida administrative data,
#   regression discontinuity, September vs August births.
#     - Kindergarten readiness: older child advantage of 10 percentage points
#       (Section 3.1, Figure 1 Panel A; Table 1 raw means ~90% Sept vs ~79% Aug
#       deemed "kindergarten ready").
#     - Test scores, grades 3 to 8: about 0.20 SD (0.197 / 0.195 / 0.201 across
#       specifications, Table 3 Panel A). The paper stresses this is
#       "remarkably stable, always just around 0.2 SD" from age 6 to 15, so the
#       standardized test effect itself does NOT fade in this study. Used as a
#       caption note, not a plotted point (different unit from the rest).
#     - Young adult (Section 3.3, Table 5, all else equal): college attendance
#       +1.3 pp (2.1%), college graduation +1.1 pp (3.3%), selective college
#       +1.3 pp (7.2%). Juvenile incarceration by the 16th birthday -0.15 pp
#       (15.4%), but the paper states this is "not statistically distinct from
#       zero at conventional levels" -> caption only, never plotted.
#
#   Bedard & Dhuey (2006), "The Persistence of Early Childhood Maturity",
#   Quarterly Journal of Economics 121(4). TIMSS math and science percentile
#   gap between the oldest and youngest quartile of a school cohort, range
#   across about 19 OECD countries. Grade 4: 4 to 12 points. Grade 8: 2 to 9
#   points. These are the verified numbers already used in R/24_beyond_sports.R
#   and docs/figures/ba_classroom.png.
#
# The plotted spine mixes units on purpose (percentage points on yes/no
# outcomes at the ends, test percentile points in the middle). That is stated
# in the caption; no smooth curve is fitted, and the two grade points carry
# their full published country ranges as bars.
# =============================================================================

suppressMessages({
  library(dplyr); library(ggplot2); library(tibble)
})
source("R/lib/theme_hometown.R")

# Ordered life stages. lo/hi are the published range (equal when a point
# estimate); mid is what the dot sits at. vlab carries the value AND its unit.
stages <- tribble(
  ~order, ~stage,                        ~lo,  ~hi,  ~mid, ~vlab,                        ~src,
  1L,     "Kindergarten\nschool entry",  10.0, 10.0, 10.0, "+10 pts\nready for K",       "NBER",
  2L,     "Grade 4\nelementary",          4.0, 12.0,  8.0, "4-12 pts\ntest gap",         "B&D",
  3L,     "Grade 8\nmiddle school",       2.0,  9.0,  5.5, "2-9 pts\ntest gap",          "B&D",
  4L,     "College entry\nyoung adult",   1.3,  1.3,  1.3, "+1.3 pts\nattend college",   "NBER"
) |>
  mutate(stage = factor(stage, levels = stage))

cat("Plotted points (life stage -> gap, unit, source):\n")
for (i in seq_len(nrow(stages))) {
  cat(sprintf("  %-28s mid=%4.1f  range=%.1f-%.1f  [%s]\n",
              gsub("\n", " / ", stages$stage[i]), stages$mid[i],
              stages$lo[i], stages$hi[i], stages$src[i]))
}

p <- ggplot(stages, aes(order, mid)) +
  # "no advantage" reference at zero, so the fade toward it is legible
  geom_hline(yintercept = 0, colour = ink_baseline, linewidth = 0.4) +
  # faint straight connectors between real estimates: an eye guide, not a fit
  geom_line(colour = "grey75", linewidth = 0.55, linetype = "22") +
  # published country ranges for the two test-score points
  geom_errorbar(data = filter(stages, lo != hi),
                aes(ymin = lo, ymax = hi), width = 0.09,
                colour = ink_body, linewidth = 0.55) +
  geom_point(colour = pal_ratio[["low"]], size = 4.4) +
  geom_text(aes(y = hi, label = vlab), vjust = -0.32, lineheight = 0.9,
            size = 3.3, fontface = "bold", colour = ink_body) +
  # reinforce "fades but persists" using the empty upper-middle space
  annotate("text", x = 3.28, y = 12.4, hjust = 0.5, lineheight = 0.95,
           label = "By adulthood the head start is\nnearly gone, but still above zero",
           size = 3.1, fontface = "italic", colour = ink_subtitle) +
  annotate("curve", x = 3.5, y = 11.0, xend = 3.98, yend = 2.4,
           curvature = 0.2, colour = "grey60", linewidth = 0.4,
           arrow = arrow(length = unit(0.02, "npc"), type = "closed")) +
  scale_x_continuous(breaks = stages$order, labels = stages$stage,
                     expand = expansion(mult = c(0.09, 0.09))) +
  scale_y_continuous(limits = c(0, 14), breaks = seq(0, 12, 3),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(
    title = paste0(
      "The oldest kids in a class start with a big edge that fades as they grow up,\n",
      "but a small advantage still survives into adulthood"),
    subtitle = "Gap between the relatively oldest and youngest in the same school cohort, at four life stages",
    x = NULL, y = "Old versus young gap (percentage or percentile points)",
    caption = fig_caption(
      source = paste0("Bedard & Dhuey (2006), Quarterly Journal of Economics 121(4); ",
                      "Dhuey, Figlio, Karbownik & Roth (2017), NBER Working Paper 23660"),
      universe = paste0(
        "\nFrom published research, not this project's data: these are the summary figures stated in each paper, nothing simulated.",
        "\nKindergarten readiness (10 pts) and college attendance (1.3 pts) are September versus August gaps on yes or no outcomes",
        "\nfrom the NBER Florida study (Dhuey, Figlio, Karbownik & Roth 2017, Sections 3.1 and 3.3). The grade 4 (4-12 pts) and",
        "\ngrade 8 (2-9 pts) points are TIMSS math and science percentile gaps between the oldest and youngest quartile across",
        "\nabout 19 OECD countries, from Bedard & Dhuey (2006)."),
      note = paste0(
        "\nUnits differ by stage (percentage points on yes or no outcomes versus test percentile points), so read the trajectory,",
        "\nnot the exact point to point differences. The NBER study finds the standardized test score effect itself holds near 0.2",
        "\nSD from grade 3 to grade 8, and it still finds older children about 15 percent less likely to be jailed as juveniles by",
        "\nage 16 (though that estimate is not statistically significant), so the edge persists even where the raw gap looks small.")
    )
  ) +
  theme_hometown(grid = "y") +
  theme(axis.text.x = element_text(lineheight = 0.95, size = rel(0.85)))

save_fig("docs/figures/ba_effect_fades.png", p, w = 12, h = 7.0)
