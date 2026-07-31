# =============================================================================
# 57_top1_wealth.R -- Michael's "do the top 1% study": where the widening gap
# actually lives. The place-level county chart (R/56) held roughly steady, but
# that is because a county median hides the rich tail. At the HOUSEHOLD level,
# the gap has widened hugely. Share of total US household net worth held by the
# top 1 percent vs the bottom 50 percent, from the Federal Reserve's
# Distributional Financial Accounts, back to 1989 (which also answers "what
# about the 90s"). Data via FRED: WFRBST01134 (top 1%), WFRBSB50215 (bottom 50%).
# =============================================================================

suppressMessages({
  library(readr); library(dplyr); library(ggplot2); library(lubridate); library(tibble)
})
source("R/lib/theme_hometown.R")

load_fred <- function(id, label) {
  read_csv(sprintf("data/raw/fed/%s.csv", id), show_col_types = FALSE) |>
    rename(date = 1, share = 2) |>
    mutate(year = decimal_date(as.Date(date)), group = label)
}
top1 <- load_fred("WFRBST01134", "Top 1%")
bot50 <- load_fred("WFRBSB50215", "Bottom 50%")
d <- bind_rows(top1, bot50) |> mutate(group = factor(group, levels = c("Top 1%", "Bottom 50%")))

t0 <- top1$share[1]; t1 <- tail(top1$share, 1)
b0 <- bot50$share[1]; b1 <- tail(bot50$share, 1)
yr1 <- floor(tail(top1$year, 1))
cat(sprintf("Top 1%%: %.1f%% (1989) -> %.1f%% (%d)\n", t0, t1, yr1))
cat(sprintf("Bottom 50%%: %.1f%% (1989) -> %.1f%% (%d)\n", b0, b1, yr1))
cat(sprintf("top1/bottom50 ratio: %.1fx (1989) -> %.1fx (now)\n", t0/b0, t1/b1))

ends <- d |> group_by(group) |> slice_max(year, n = 1) |> ungroup()
pal <- c("Top 1%" = pal_sport[["NFL"]], "Bottom 50%" = "grey60")

p <- ggplot(d, aes(year, share, colour = group)) +
  geom_line(linewidth = 1.1) +
  geom_text(data = ends, aes(label = sprintf("%s  %.1f%%", group, share)),
            hjust = 0, nudge_x = 0.4, size = 3.6, fontface = "bold", show.legend = FALSE) +
  scale_colour_manual(values = pal, guide = "none") +
  scale_x_continuous(breaks = seq(1990, 2025, 5), limits = c(1989, 2032)) +
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, NA),
                     expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "This is where the gap actually widened: the top 1 percent now hold nearly a third of all US wealth",
    subtitle = "Share of total US household net worth held by the top 1 percent and the bottom 50 percent, since 1989.",
    x = NULL, y = NULL,
    caption = fig_caption(
      "Federal Reserve Distributional Financial Accounts via FRED (series WFRBST01134 and WFRBSB50215), quarterly",
      sprintf("\nThe top 1 percent went from %.1f to %.1f percent of all household wealth since 1989, while the bottom half fell from %.1f to %.1f percent.", t0, t1, b0, b1),
      "\nThis is household net worth, the widening the county-level chart could not see because a county's median hides the rich tail. This is the real Matthew principle,\nand where Michael's instinct is right. General population, no sports, no race.")) +
  theme_hometown(grid = "y")
save_fig("docs/figures/ba_top1_wealth.png", p, w = 11.5, h = 5.6)
cat("\ndone\n")
