# =============================================================================
# 50_soccer_cutoffs.R -- the sharpest proof that the relative age effect follows
# the CUTOFF, not the calendar. Two top leagues, two different youth cutoffs:
#   - Premier League (England): age cutoff September 1 -> players peak in autumn
#   - La Liga (Spain):          age cutoff January 1   -> players peak in winter
# Same effect, the birth-month peak just slides to wherever each country draws
# its age line. Data: Wikidata, footballers at clubs currently in each league,
# month-or-better birth dates (pulled in R/46 and here).
# =============================================================================

suppressMessages({
  library(readr); library(dplyr); library(ggplot2); library(tibble)
})
source("R/lib/theme_hometown.R")

load_league <- function(path, name, hi_months) {
  read_csv(path, show_col_types = FALSE) |>
    distinct(player, .keep_all = TRUE) |>
    mutate(month = as.integer(substr(dob, 6, 7))) |>
    filter(!is.na(month)) |>
    count(month, name = "n") |>
    mutate(share = 100 * n / sum(n), league = name,
           hi = ifelse(month %in% hi_months, "cutoff", "other"))
}

epl <- load_league("data/processed/epl_dob.csv", "Premier League (England): cutoff September 1", c(9, 10, 11))
lal <- load_league("data/processed/laliga_dob.csv", "La Liga (Spain): cutoff January 1", c(1, 2, 3))
n_epl <- sum(epl$n); n_lal <- sum(lal$n)

df <- bind_rows(epl, lal) |>
  mutate(month_lab = factor(month.abb[month], levels = month.abb),
         league = factor(league, levels = c("Premier League (England): cutoff September 1",
                                            "La Liga (Spain): cutoff January 1")))

cat(sprintf("EPL n=%s (Sep %.1f%%), La Liga n=%s (Jan %.1f%%)\n",
            format(n_epl, big.mark=","), epl$share[epl$month==9],
            format(n_lal, big.mark=","), lal$share[lal$month==1]))

p <- ggplot(df, aes(month_lab, share, fill = hi)) +
  geom_hline(yintercept = 100/12, linetype = "dashed", colour = ink_baseline, linewidth = 0.4) +
  geom_col(width = 0.74) +
  facet_wrap(~league, ncol = 1) +
  scale_fill_manual(values = c(cutoff = "#2B8CBE", other = "grey72"), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "The birthday edge follows the cutoff: England's league peaks in September, Spain's in January",
    subtitle = "Birth-month share of players. The highlighted months are the oldest in each country's youth football year, right after its age cutoff.",
    x = NULL, y = "Share of players (%)",
    caption = fig_caption(
      "Wikidata SPARQL: footballers at clubs currently in each league, month-or-better birth dates",
      sprintf("\nPremier League n = %s, La Liga n = %s (full club histories, not just current squads). Dashed line is a flat baseline.", format(n_epl, big.mark=","), format(n_lal, big.mark=",")),
      "\nEngland's youth cutoff is September 1, so its oldest kids are born in autumn; Spain's is January 1, so its oldest are born in winter. The effect is\nidentical, the peak just moves to match the cutoff. This is the cleanest proof that the relative age effect is about the age line, not the calendar itself.")) +
  theme_hometown(grid = "y") +
  theme(strip.text = element_text(hjust = 0, size = rel(0.92)))
save_fig("docs/figures/ba_soccer_cutoffs.png", p, w = 12, h = 7.0)
cat("\ndone\n")
