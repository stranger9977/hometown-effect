# =============================================================================
# 70_relative_mobility.R -- Michael's ask: get at RELATIVE mobility, and his
# framing is "your ability to leave the bottom, which in a utopia would be equal."
# (First version used the rank-rank line; Michael found it confusing -- "a kid
# born at 0 moves up to 33?" -- so this is the intuitive version: where kids
# actually LAND, by where they were born, against a flat 20% "fair world" line.)
#
# This answers the conservative pushback on the absolute-mobility chart ("1940s
# kids out-earned Depression-era parents easily"): relative mobility ignores
# whether everyone got richer and only asks whether your birth rank still decides
# your rank. In a perfect meritocracy a kid born in any fifth would have an equal
# 20% chance of landing in each adult fifth. They do not.
#
# SOURCE: quintile transition matrix computed from Chetty et al. (2017) "Fading
# American Dream" released copula (Harvard Dataverse doi:10.7910/DVN/B9TEWM,
# copula_base_adjusted.tab), aggregated to income fifths. It reproduces the
# published corners of Chetty, Hendren, Kline & Saez (2014), QJE 129(4) exactly
# (Q1->Q1 33.7, Q1->Q5 7.5, Q5->Q1 10.9, Q5->Q5 36.5). Recent trend from Chetty,
# Dobbie, Goldman, Porter & Yang (2024), NBER WP 32697: the class gap in mobility
# grew about 30% between the 1978 and 1992 cohorts. PUBLISHED RESEARCH, NOT OURS.
# No race/ethnicity on the chart.
# =============================================================================

suppressMessages({library(dplyr); library(ggplot2); library(tibble); library(tidyr)})
source("R/lib/theme_hometown.R")

# parent Q1 (poorest-born) and Q5 (richest-born) rows, child fifths Q1..Q5
d <- tribble(
  ~origin,             ~fifth,            ~pct,  ~fo,
  "Born poorest fifth","Poorest\nfifth",  33.7,  1,
  "Born poorest fifth","2nd",             28.0,  1,
  "Born poorest fifth","Middle",          18.4,  1,
  "Born poorest fifth","4th",             12.3,  1,
  "Born poorest fifth","Richest\nfifth",   7.5,  1,
  "Born richest fifth","Poorest\nfifth",  10.9,  2,
  "Born richest fifth","2nd",             11.9,  2,
  "Born richest fifth","Middle",          17.0,  2,
  "Born richest fifth","4th",             23.6,  2,
  "Born richest fifth","Richest\nfifth",  36.5,  2
) |>
  mutate(fifth  = factor(fifth, levels = c("Poorest\nfifth","2nd","Middle","4th","Richest\nfifth")),
         origin = factor(origin, levels = c("Born poorest fifth","Born richest fifth")))

pal_o <- c(`Born poorest fifth` = "#D55E00", `Born richest fifth` = "#0072B2")

p <- ggplot(d, aes(fifth, pct, fill = origin)) +
  geom_col(position = position_dodge(width = 0.74), width = 0.68) +
  geom_hline(yintercept = 20, linetype = "dashed", colour = ink_baseline, linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.0f%%", pct)), position = position_dodge(width = 0.74),
            vjust = -0.5, size = 3.0, fontface = "bold", colour = ink_body) +
  annotate("text", x = 5.42, y = 22.4, hjust = 1, size = 3.0, colour = ink_baseline,
           label = "A fair world: 20% each") +
  scale_fill_manual(values = pal_o, name = NULL) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 10),
                     labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0, 0.04))) +
  labs(
    title = "Born in the poorest fifth, you usually stay near the bottom; born in the richest, near the top",
    subtitle = "Where US children land in the adult income distribution, by the fifth they were born into. In a fair world every bar would be 20 percent.",
    x = "Where the child ends up as an adult", y = "Chance of landing there",
    caption = fig_caption(
      "Income-fifth transition matrix from Chetty et al. (2017) released copula (reproduces Chetty, Hendren, Kline & Saez 2014 exactly); trend from Chetty et al. (2024), NBER 32697",
      "\nFrom published research, not this project's data. Relative mobility compares a child's income rank to their parents', so unlike the out-earn-your-parents\nmeasure it ignores whether everyone got richer. A child born in the poorest fifth has only a 7.5 percent shot at the richest fifth and a 34 percent chance of\nstaying at the bottom; a child born in the richest fifth has a 36.5 percent chance of staying on top. A perfect meritocracy would be a flat 20 percent for both.",
      "\nAnd it is not improving: the class gap in mobility grew about 30 percent between the 1978 and 1992 birth cohorts (Chetty et al. 2024), so the ladder tilted\na little more toward the top, not less. This is the answer to 'the 1940s just had an easy baseline': relative mobility nets out growth, and by it the US has\nnot become fairer.")) +
  theme_hometown(grid = "y") +
  theme(legend.position = "top", legend.justification = "left",
        legend.key.size = unit(11, "pt"), legend.text = element_text(size = rel(0.85)))
save_fig("docs/figures/ba_relative_mobility.png", p, w = 11, h = 6.0)
cat("done\n")
