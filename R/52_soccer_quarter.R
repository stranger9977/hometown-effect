# =============================================================================
# 52_soccer_quarter.R -- Michael's ask: the same quarter-share view (like the
# 1982 NHL figure and our 4-league quarter chart) for US soccer. Percent of
# players born in each birth quarter Q1-Q4, against the real US birth share,
# for US soccer players and swimmers from Wikidata. Same style as
# ba_quarter_share.png so it drops right in next to the leagues.
# =============================================================================

suppressMessages({
  library(readr); library(dplyr); library(ggplot2); library(tidyr); library(tibble)
})
source("R/lib/theme_hometown.R")

exp_q <- read_csv("data/processed/us_birth_seasonality.csv", show_col_types = FALSE) |>
  mutate(quarter = ceiling(month / 3)) |>
  group_by(quarter) |> summarise(expected = 100 * sum(expected_share), .groups = "drop")

load_q <- function(path, name) {
  read_csv(path, show_col_types = FALSE) |>
    distinct(qid, .keep_all = TRUE) |>
    mutate(month = as.integer(substr(dob, 6, 7)), quarter = ceiling(month / 3)) |>
    filter(!is.na(quarter)) |>
    count(quarter, name = "n") |>
    mutate(share = 100 * n / sum(n), sport = name, total = sum(n))
}

soc <- load_q("data/raw/wikidata/soccer_dob.csv", "US soccer")
swi <- load_q("data/raw/wikidata/swim_dob.csv", "US swimming")
n_soc <- soc$total[1]; n_swi <- swi$total[1]

obs <- bind_rows(soc, swi) |>
  mutate(sport = factor(sport, levels = c("US soccer", "US swimming")),
         quarter_lab = factor(paste0("Q", quarter), levels = paste0("Q", 1:4)))
exp_line <- tidyr::crossing(sport = factor(c("US soccer", "US swimming"), levels = c("US soccer", "US swimming")),
                            exp_q) |>
  mutate(quarter_lab = factor(paste0("Q", quarter), levels = paste0("Q", 1:4)))

cat("=== US soccer quarter share (%):", paste(round(soc$share,1), collapse=", "), "\n")
cat("=== US swimming quarter share (%):", paste(round(swi$share,1), collapse=", "), "\n")

pal <- c("US soccer" = "#3B6BA5", "US swimming" = "#5AAE61")

p <- ggplot(obs, aes(quarter_lab, share, group = sport)) +
  geom_line(data = exp_line, aes(quarter_lab, expected, group = 1),
            colour = ink_baseline, linewidth = 0.5, linetype = "dashed") +
  geom_line(aes(colour = sport), linewidth = 1) +
  geom_point(aes(colour = sport), size = 2.8) +
  geom_text(aes(label = sprintf("%.0f%%", share), colour = sport),
            vjust = -1.0, size = 3.2, fontface = "bold", show.legend = FALSE) +
  facet_wrap(~sport, nrow = 1) +
  scale_colour_manual(values = pal, guide = "none") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0.02, 0.16))) +
  labs(
    title = "US soccer players slide from Q1 to Q4 like hockey, and swimmers do too",
    subtitle = "Percent of players born in each birth quarter (Q1 = Jan-Mar ... Q4 = Oct-Dec), against the real US birth share (dashed)",
    x = NULL, y = "Share of players (%)",
    caption = fig_caption(
      "Wikidata SPARQL: US-citizen association-football players and swimmers with a recorded birth date",
      sprintf("\nUS soccer n = %s, US swimming n = %s. Same quarter view as the four leagues and the original 1982 NHL figure. Dashed line is the real US birth share.", format(n_soc, big.mark=","), format(n_swi, big.mark=",")),
      "\nBoth tilt toward early-year births like hockey, the mark of a hard age-group cutoff. Wikidata coverage is broad (not just pro rosters), so read the shape, not the decimals.")) +
  theme_hometown(grid = "y") +
  theme(strip.text = element_text(hjust = 0.5),
        axis.text.x = element_text(size = rel(0.85)))
save_fig("docs/figures/ba_soccer_quarter.png", p, w = 10, h = 5.2)
cat("\ndone\n")
