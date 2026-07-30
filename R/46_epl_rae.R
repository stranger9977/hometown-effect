# =============================================================================
# 46_epl_rae.R -- Michael's foreign-league soccer question. The Premier League
# shows the same relative age effect as US hockey, but SHIFTED: it peaks in
# September and October, not January, because English youth football's age
# cutoff is September 1 (the school year), not January 1. The birthday edge
# follows whatever cutoff the sport uses. This is the sharpest way to show the
# effect is about the cutoff, not the calendar.
#
# Data: Wikidata, footballers (P106 = Q937857) who are or were members (P54) of
# a club currently in the Premier League (P118 = Q9448), with month-or-better
# birth dates. That captures the full player history of today's top English
# clubs, not just current squads; noted in the caption.
# =============================================================================

suppressMessages({
  library(readr); library(dplyr); library(ggplot2); library(tibble)
})
source("R/lib/theme_hometown.R")

epl <- read_csv("data/processed/epl_dob.csv", show_col_types = FALSE) |>
  distinct(player, .keep_all = TRUE) |>
  mutate(month = as.integer(substr(dob, 6, 7))) |>
  filter(!is.na(month))
n_epl <- nrow(epl)

days <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
exp_share <- days / sum(days)

ms <- epl |>
  count(month, name = "n") |>
  mutate(share = 100 * n / sum(n),
         month_lab = factor(month.abb[month], levels = month.abb),
         # oldest in the English football year (Sept 1 cutoff) = Sep, Oct, Nov
         grp = ifelse(month %in% c(9, 10, 11), "autumn", "other"))

# football-year quarter check (Sep 1 cutoff)
fq <- function(m) ((m - 9) %% 12) %/% 3 + 1
qt <- epl |> mutate(q = fq(month)) |> count(q) |> mutate(share = round(100 * n / sum(n), 1))
cat("distinct EPL players:", n_epl, "\n")
cat("football-year quarter shares (Sep-Nov, Dec-Feb, Mar-May, Jun-Aug):",
    paste(qt$share, collapse = ", "), "\n")

sep <- round(ms$share[ms$month == 9], 1); jun <- round(ms$share[ms$month == 6], 1)

p <- ggplot(ms, aes(month_lab, share, fill = grp)) +
  geom_hline(yintercept = 100 * exp_share[1], linetype = "dashed",
             colour = ink_baseline, linewidth = 0.4) +
  geom_col(width = 0.72) +
  geom_text(data = ~subset(.x, month %in% c(6, 9)),
            aes(label = sprintf("%.1f%%", share)), vjust = -0.5, size = 3.3,
            fontface = "bold", colour = ink_body) +
  scale_fill_manual(values = c(autumn = "#2B8CBE", other = "grey72"), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  annotate("text", x = 10, y = 11.4, label = "born just after the\nSeptember 1 cutoff",
           size = 3.0, colour = "#1c6fa8", fontface = "bold", lineheight = 0.95) +
  labs(
    title = "The Premier League has the same birthday edge as US hockey, shifted to its September cutoff",
    subtitle = "Birth-month share of Premier League footballers. English youth football's age cutoff is September 1, so the oldest kids in a group are born in autumn.",
    x = NULL, y = "Share of players (%)",
    caption = fig_caption(
      "Wikidata SPARQL: footballers (P106) at clubs currently in the Premier League (P118), month-or-better birth dates",
      sprintf("\nn = %s distinct players (the full history of today's top English clubs, not just current squads). Dashed line is a flat days-adjusted baseline.", format(n_epl, big.mark = ",")),
      sprintf("\nSeptember births (%.1f%%) run well above June (%.1f%%), and by the English football year the oldest quarter (Sep-Nov) is 30%% of players vs 20%% for\nthe youngest (Jun-Aug). Natural birth seasonality is a percent or two at most, far too small to explain this. US hockey peaks in January because its\ncutoff is January 1; the Premier League peaks in September because its cutoff is September 1. Same effect, different cutoff.", sep, jun))) +
  theme_hometown(grid = "y")
save_fig("docs/figures/ba_epl_rae.png", p, w = 12, h = 6.4)
cat("\ndone\n")
