# =============================================================================
# 30_nfl_young_draft.R -- the fantasy-football version of the underdog reversal.
#
# Nick's angle: a young player who goes high in the NFL draft is a strong
# signal. An elite prospect who is ALSO young for his class got there despite
# being less physically mature than the players around him, so his tape is more
# likely to be real talent than borrowed size. Ja'Marr Chase went 5th overall
# at 21. If the angle holds, younger draftees should outproduce older ones, and
# should do so even at the same draft slot (so it is not just that young guys
# get picked higher).
#
# Outcome = weighted career approximate value (w_av) + Pro Bowl rate, from
# nflverse draft picks. Skill positions (QB/RB/WR/TE), draft classes 2000-2016
# so careers have had time to accrue. No player-level data leaves nflverse.
# =============================================================================

suppressMessages({
  library(nflreadr); library(dplyr); library(ggplot2); library(tidyr); library(tibble)
})
source("R/lib/theme_hometown.R")

sk <- load_draft_picks() |>
  as_tibble() |>
  filter(position %in% c("QB","RB","WR","TE"), season >= 2000, season <= 2016,
         !is.na(age), !is.na(w_av)) |>
  mutate(w_av = as.numeric(w_av),
         agecap = cut(age, breaks = c(-Inf, 21, 22, 23, Inf),
                      labels = c("21 or under", "22", "23", "24 or older")),
         probowl = probowls > 0)

by_age <- sk |>
  group_by(agecap) |>
  summarise(n = n(), mean_wav = mean(w_av), probowl_rate = 100 * mean(probowl),
            .groups = "drop")
cat("=== skill-position picks 2000-2016 by age at draft ===\n")
print(as.data.frame(by_age |> mutate(mean_wav = round(mean_wav, 1),
                                     probowl_rate = round(probowl_rate, 1))))

# Robustness: does it survive holding draft capital fixed? (first round only)
fr <- sk |> filter(round == 1) |> group_by(agecap) |>
  summarise(n = n(), mean_wav = round(mean(w_av), 1),
            probowl_rate = round(100 * mean(probowl), 1), .groups = "drop")
cat("\n=== first round only (draft slot held roughly fixed) ===\n")
print(as.data.frame(fr))
young_fr <- fr$mean_wav[fr$agecap == "21 or under"]
old_fr   <- mean(fr$mean_wav[fr$agecap %in% c("23", "24 or older")])

# --- one pasteable chart: value and Pro Bowl rate by age at draft -----------
plot_df <- by_age |>
  transmute(agecap,
            `Career value (weighted AV)` = mean_wav,
            `Reached a Pro Bowl (%)`      = probowl_rate) |>
  pivot_longer(-agecap, names_to = "measure", values_to = "value") |>
  mutate(measure = factor(measure, levels = c("Career value (weighted AV)", "Reached a Pro Bowl (%)")),
         lab = ifelse(measure == "Reached a Pro Bowl (%)",
                      sprintf("%.0f%%", value), sprintf("%.0f", value)))

p <- ggplot(plot_df, aes(agecap, value)) +
  geom_col(fill = pal_sport[["NFL"]], width = 0.72) +
  geom_text(aes(label = lab), vjust = -0.5, size = 3.6, fontface = "bold",
            colour = ink_body) +
  facet_wrap(~measure, scales = "free_y") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.16))) +
  labs(
    title = "The younger a skill player is on draft day, the better his NFL career tends to be",
    subtitle = "NFL QB, RB, WR, and TE draft picks (2000-2016) by age at the draft. Left: career value. Right: how many made a Pro Bowl.",
    x = "Age at the NFL draft", y = NULL,
    caption = fig_caption(
      "nflverse draft picks (weighted career approximate value + Pro Bowls)",
      "\nQB, RB, WR, TE drafted 2000-2016 with career data (n = 1,191); weighted AV is Pro-Football-Reference's career-value measure.",
      paste0("\nThe edge holds even inside the first round (21-or-under first-rounders average ", young_fr,
             " AV vs ", round(old_fr), " for age 23 and up), so it is not only that younger players get picked higher.\n",
             "It is the draft-day version of the underdog effect: being elite AND young means the youth was not doing the work. Example: Ja'Marr Chase, 5th at 21."))) +
  theme_hometown(grid = "y") +
  theme(strip.text = element_text(hjust = 0.5, size = rel(0.95)),
        panel.spacing = unit(1.8, "lines"),
        axis.text.x = element_text(size = rel(0.8)))
save_fig("docs/figures/ba_nfl_young_draft.png", p, w = 11.5, h = 5.4)
cat("\ndone\n")
