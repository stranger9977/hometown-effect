# =============================================================================
# 54_soccer_quarter_cohorts.R -- Michael's follow-up: the US soccer before/after
# the 2016 cutoff change, but in the QUARTER view to match the theme. US Soccer
# moved its youth age cutoff from August 1 to a January 1 calendar year around
# 2016. We split US soccer players by birth cohort:
#   - OLD cutoff: born 1985-1999 (grew up entirely under the August 1 cutoff)
#   - NEW cutoff: born 2003-2009 (youth-eligible under the 2016+ January 1 cutoff)
# and show percent of players by birth quarter, same style as ba_soccer_quarter.
#
# Honest read: the newer, January-cutoff cohort tilts much harder toward Q1, but
# it is a small sample (n = 721) and fresh to Wikipedia, so part of that spike
# may be early-born standouts being the only ones famous enough to have a page
# yet, not a pure cutoff effect. Caveat is on the chart.
# =============================================================================

suppressMessages({
  library(readr); library(dplyr); library(ggplot2); library(tidyr); library(tibble)
})
source("R/lib/theme_hometown.R")

exp_q <- read_csv("data/processed/us_birth_seasonality.csv", show_col_types = FALSE) |>
  mutate(quarter = ceiling(month / 3)) |>
  group_by(quarter) |> summarise(expected = 100 * sum(expected_share), .groups = "drop")

d <- read_csv("data/raw/wikidata/soccer_dob.csv", show_col_types = FALSE) |>
  distinct(qid, .keep_all = TRUE) |>
  mutate(year = as.integer(substr(dob, 1, 4)), month = as.integer(substr(dob, 6, 7)),
         quarter = ceiling(month / 3)) |>
  filter(!is.na(quarter), !is.na(year))

cohort <- function(lo, hi, name) {
  s <- d |> filter(year >= lo, year <= hi)
  s |> count(quarter, name = "n") |>
    mutate(share = 100 * n / sum(n), cohort = name, total = sum(n))
}
old <- cohort(1985, 1999, "Old cutoff, born 1985-1999\n(August 1 cutoff)")
new <- cohort(2003, 2009, "New cutoff, born 2003-2009\n(January 1 cutoff, 2016+)")
n_old <- old$total[1]; n_new <- new$total[1]

lv <- c("Old cutoff, born 1985-1999\n(August 1 cutoff)", "New cutoff, born 2003-2009\n(January 1 cutoff, 2016+)")
obs <- bind_rows(old, new) |>
  mutate(cohort = factor(cohort, levels = lv),
         quarter_lab = factor(paste0("Q", quarter), levels = paste0("Q", 1:4)))
exp_line <- tidyr::crossing(cohort = factor(lv, levels = lv), exp_q) |>
  mutate(quarter_lab = factor(paste0("Q", quarter), levels = paste0("Q", 1:4)))

pal <- setNames(c("#74A9CF", "#045A8D"), lv)

p <- ggplot(obs, aes(quarter_lab, share, group = cohort)) +
  geom_line(data = exp_line, aes(quarter_lab, expected, group = 1),
            colour = ink_baseline, linewidth = 0.5, linetype = "dashed") +
  geom_line(aes(colour = cohort), linewidth = 1) +
  geom_point(aes(colour = cohort), size = 2.8) +
  geom_text(aes(label = sprintf("%.0f%%", share), colour = cohort),
            vjust = -1.0, size = 3.2, fontface = "bold", show.legend = FALSE) +
  facet_wrap(~cohort, nrow = 1) +
  scale_colour_manual(values = pal, guide = "none") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0.02, 0.18))) +
  labs(
    title = "US soccer's early-year tilt got much sharper after the 2016 switch to a January cutoff",
    subtitle = "Percent of US soccer players by birth quarter, before and after US Soccer moved its youth age cutoff from August 1 to January 1",
    x = NULL, y = "Share of players (%)",
    caption = fig_caption(
      "Wikidata SPARQL: US-citizen association-football players with a recorded birth date, split by birth cohort",
      sprintf("\nOld-cutoff cohort n = %s, new-cutoff cohort n = %s (birth years 2000-2002 dropped as a transition window). Dashed line is the real US birth share.", format(n_old, big.mark=","), format(n_new, big.mark=",")),
      "\nThe January-cutoff cohort tilts hard to Q1 (37 percent) and away from Q4 (17), a much steeper slide than the older cohort. But it is a small, recent sample,\nso some of that spike may be early-born standouts being the only ones on Wikipedia yet, not a pure cutoff effect. Read the direction, not the decimals.")) +
  theme_hometown(grid = "y") +
  theme(strip.text = element_text(hjust = 0.5, size = rel(0.85), lineheight = 0.9),
        axis.text.x = element_text(size = rel(0.85)))
save_fig("docs/figures/ba_soccer_quarter_cohorts.png", p, w = 11, h = 5.4)
cat(sprintf("old Q1-Q4: %s | new Q1-Q4: %s\n", paste(round(old$share,1),collapse=","), paste(round(new$share,1),collapse=",")))
cat("done\n")
