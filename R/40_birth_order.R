# =============================================================================
# 40_birth_order.R -- the first-born advantage, from published research.
#
# The collaborator asked: "have you looked into birth order... are CEOs
# first-borns?" We could NOT find a rigorous, large-sample source for the
# "most CEOs are first-borns" claim, so we omit it. What IS well established
# is a small first-born edge in IQ, replicated in two large Norwegian
# population studies, which also shows up in adult earnings.
#
# THIS IS PUBLISHED RESEARCH, NOT THIS PROJECT'S DATA. Every number below is
# quoted from a named source and each is a first-born vs SECOND-born gap.
#
# Verified figures:
#   Kristensen & Bjerkedal 2007, Science ("Explaining the Relation Between
#     Birth Order and Intelligence"; companion in Intelligence): first-borns
#     average about 2.3 IQ points above second-borns, from 241,310 Norwegian
#     male conscripts (born 1967-1976). The gap tracks social rank, not
#     biology: second-borns whose older sibling died scored like first-borns.
#   Black, Devereux & Salvanes 2007, NBER Working Paper 13237 ("Older and
#     Wiser? Birth Order and IQ of Young Men"): first-born vs second-born gap
#     of "about one fifth of a standard deviation or approximately 3 IQ
#     points," from Norwegian population records. Quote: this "translates into
#     approximately a 2% difference in annual earnings as an adult."
# =============================================================================

source("R/lib/theme_hometown.R")

suppressMessages({
  library(dplyr)
})

# ---------------------------------------------------------------------------
# The data: three verified first-born-minus-second-born gaps. Two share a
# unit (IQ points) so they sit in one panel; earnings gets its own panel with
# its own axis so the different units are never plotted on one shared scale.
# ---------------------------------------------------------------------------
lev_iq  <- "IQ score, first-born minus second-born (points)"
lev_pay <- "Adult earnings, first-born minus second-born (percent)"

lab_kb  <- "Kristensen & Bjerkedal\n(2007, Science)"
lab_bds <- "Black, Devereux &\nSalvanes (2007)"

df <- tibble::tibble(
  measure = c(lev_iq, lev_iq, lev_pay),
  study   = c(lab_kb, lab_bds, lab_bds),
  value   = c(2.3, 3.0, 2.0),
  vlab    = c("+2.3 IQ points", "+3 IQ points", "+2% higher pay")
) |>
  mutate(
    measure = factor(measure, levels = c(lev_iq, lev_pay)),
    # last factor level plots on top; put the headline Science study on top.
    study   = factor(study, levels = c(lab_bds, lab_kb))
  )

accent      <- pal_ratio[["high"]]  # first-born edge = warm, above baseline
accent_dark <- "#A5382A"            # darker accent for the value labels

# ---------------------------------------------------------------------------
# Chart: horizontal bars, one accent, direct-labeled. Faceted by measure with
# free x AND free space so each unit keeps its own axis and bars stay equal
# thickness. Bars grow from a common zero baseline in every panel.
# ---------------------------------------------------------------------------
p <- ggplot(df, aes(value, study)) +
  geom_col(fill = accent, width = 0.62) +
  geom_text(aes(label = vlab), hjust = 0, nudge_x = 0.08,
            size = 4.1, fontface = "bold", colour = accent_dark) +
  facet_grid(rows = vars(measure), scales = "free", space = "free") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.34))) +
  labs(
    title = "The first-born advantage is real but small: 2 to 3 IQ points over the second-born",
    subtitle = paste0(
      "Two large Norwegian population studies agree, and the same edge shows ",
      "up as about 2 percent higher adult earnings."),
    x = NULL, y = NULL,
    caption = fig_caption(
      source = paste0("Kristensen & Bjerkedal 2007 (Science); Black, Devereux ",
                      "& Salvanes 2007 (NBER Working Paper 13237)"),
      universe = paste0(
        "\nFrom published research, not this project's data. Both studies use ",
        "Norwegian population and military conscript",
        "\nrecords, and every figure compares first-borns with second-borns."),
      note = paste0(
        "\nK&B report about 2.3 IQ points across roughly 241,000 conscripts. ",
        "BDS report about one fifth of a standard",
        "\ndeviation (about 3 IQ points) and about 2 percent higher adult earnings."))
  ) +
  theme_hometown(grid = "none") +
  theme(
    axis.text.x   = element_blank(),
    strip.text.y  = element_text(angle = 0, hjust = 0, face = "bold"),
    plot.margin   = margin(10, 70, 8, 10)
  ) +
  coord_cartesian(clip = "off")

save_fig("docs/figures/ba_birth_order.png", p, w = 11, h = 6.2)
