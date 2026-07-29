# =============================================================================
# 31_matthew_principle.R -- the Matthew effect explainer (Michael's outline
# item "Matthew Principal"). This is the NAME for the mechanism behind the
# whole piece: accumulated advantage. A tiny early edge (being oldest in the
# age group) compounds at every selection gate into a large late gap.
#
# HONESTY NOTE: this figure is an ILLUSTRATION of the mechanism, not measured
# data, and it says so on its face. The measured fingerprint of this compounding
# is the relative age effect itself (see the hockey month bars, the quarter
# chart, and the underdog reversal, all built from real data). This diagram just
# gives the video a clean way to SHOW the snowball while narrating it.
# =============================================================================

suppressMessages({
  library(dplyr); library(ggplot2); library(tibble)
})
source("R/lib/theme_hometown.R")

stages <- tibble(
  x     = 1:6,
  label = c("Youth\nleague", "Rep\nteam", "Elite\nteam", "Varsity",
            "College /\ndraft", "Pro"),
  early = c(8, 20, 35, 54, 76, 100),   # the oldest-in-group kid, selected again and again
  late  = c(5,  9, 12, 13, 14,  15))    # the youngest, passed over at each gate

gap <- stages |>
  transmute(x, ymin = late, ymax = early)

ends <- tibble(
  x = c(6.05, 6.05),
  y = c(100, 15),
  lab = c("Born early in the year", "Born late in the year"),
  col = c(pal_ratio[["high"]], ink_subtitle))

gates <- tibble(
  x = c(2, 4, 6),
  y = stages$early[c(2, 4, 6)],
  note = c("makes the team,\nmore ice time", "more coaching,\ntougher games",
           "the gap now looks\nlike talent"))

p <- ggplot() +
  geom_ribbon(data = gap, aes(x = x, ymin = ymin, ymax = ymax),
              fill = pal_ratio[["high"]], alpha = 0.10) +
  geom_line(data = stages, aes(x, late), colour = ink_subtitle, linewidth = 1) +
  geom_line(data = stages, aes(x, early), colour = pal_ratio[["high"]], linewidth = 1.3) +
  geom_point(data = stages, aes(x, early), colour = pal_ratio[["high"]], size = 2.6) +
  geom_point(data = stages, aes(x, late), colour = ink_subtitle, size = 2.2) +
  geom_text(data = ends, aes(x, y, label = lab, colour = col),
            hjust = 0, size = 3.7, fontface = "bold") +
  geom_text(data = gates, aes(x, y, label = note), vjust = 1.5, hjust = 0.5,
            size = 2.9, colour = ink_body, lineheight = 0.95) +
  scale_colour_identity() +
  scale_x_continuous(breaks = 1:6, labels = stages$label,
                     limits = c(0.8, 8.6)) +
  scale_y_continuous(limits = c(0, 108), expand = expansion(mult = c(0.02, 0.04))) +
  labs(
    title = "The Matthew effect: a one-month head start snowballs into a career",
    subtitle = "Every selection gate hands the older kid a little more, and the edge compounds. An illustration of the mechanism, not measured data.",
    x = NULL, y = "Accumulated advantage",
    caption = paste0(
      "Illustration, not data. The measured fingerprint of this compounding is the relative age effect itself: see the hockey and quarter charts\n",
      "above for the selection skew, and the underdog reversal for what it does to the few who beat it. Term: the Matthew effect (Merton, 1968),\n",
      "accumulated advantage, the rich get richer. The early ability gap fades as kids grow up, but the doors it opened or closed stay that way.")) +
  theme_hometown(grid = "none") +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_text(colour = ink_body),
        axis.text.x = element_text(size = rel(0.8), lineheight = 0.9),
        plot.margin = margin(10, 14, 8, 10))
save_fig("docs/figures/ba_matthew_principle.png", p, w = 11, h = 5.6)
cat("done\n")
