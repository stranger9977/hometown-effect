# =============================================================================
# 45_qb_income.R -- Michael's QB question, on the income and high-school-location
# axis only. Two honest halves:
#   (1) QBs increasingly come from the richest quarter of hometowns (a real,
#       rising trend across the eras).
#   (2) Part of that is WHERE they went to high school, not where they grew up:
#       we place a player by high school when we know it, so a recruit who
#       transfers to a well-off powerhouse gets tagged to that richer place.
#       QBs placed by high school come from meaningfully richer hometowns than
#       QBs placed by birthplace. Bryce Young (Mater Dei) is the type case.
# Data: hometown.parquet (NFL). No race cut; that angle is out of scope here.
# =============================================================================

suppressMessages({
  library(arrow); library(dplyr); library(ggplot2); library(tidyr); library(tibble)
})
source("R/lib/theme_hometown.R")

h <- read_parquet("data/processed/hometown.parquet")
q75 <- quantile(h$matched_income_now, 0.75, na.rm = TRUE)   # national richest quartile

qb <- h |> filter(position == "QB", !is.na(matched_income_now)) |>
  mutate(rich = matched_income_now >= q75)

era_levels <- c("1990s", "2000s", "2010s", "2020s")
by_era <- qb |> filter(era %in% era_levels) |>
  group_by(era) |> summarise(pct_rich = 100 * mean(rich), .groups = "drop") |>
  mutate(era = factor(era, levels = era_levels))

by_src <- qb |> filter(hometown_source %in% c("birthplace", "high_school")) |>
  group_by(hometown_source) |>
  summarise(med = median(matched_income_now) / 1000, .groups = "drop") |>
  mutate(src = recode(hometown_source, birthplace = "Placed by\nbirthplace",
                      high_school = "Placed by\nhigh school"))

cat("QB richest-quartile share by era:\n"); print(as.data.frame(by_era |> mutate(pct_rich = round(pct_rich,1))))
cat("\nQB median hometown income by placement (thousands):\n"); print(as.data.frame(by_src))

# --- panel A: trend over eras ---
pA <- ggplot(by_era, aes(era, pct_rich, group = 1)) +
  geom_line(colour = pal_sport[["NFL"]], linewidth = 0.9) +
  geom_point(colour = pal_sport[["NFL"]], size = 2.8) +
  geom_text(aes(label = sprintf("%.0f%%", pct_rich)), vjust = -1.0, size = 3.6,
            fontface = "bold", colour = ink_body) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0.02, 0.18))) +
  labs(subtitle = "Share of QBs from the richest quarter of hometowns, by era", x = NULL, y = NULL) +
  theme_hometown(grid = "y")

# --- panel B: placement effect ---
pB <- ggplot(by_src, aes(src, med)) +
  geom_col(fill = pal_sport[["NFL"]], width = 0.6) +
  geom_text(aes(label = sprintf("$%.0fk", med)), vjust = -0.5, size = 3.8,
            fontface = "bold", colour = ink_body) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.16))) +
  labs(subtitle = "QB median hometown income, by how we placed the player", x = NULL, y = NULL) +
  theme_hometown(grid = "y")

# combine via a shared long frame instead of a patchwork dependency
combo <- bind_rows(
  by_era |> transmute(x = as.character(era), value = pct_rich, panel = "Share of QBs from the richest quarter of hometowns (%)", lab = sprintf("%.0f%%", pct_rich), kind = "line"),
  by_src |> transmute(x = src, value = med, panel = "QB median hometown income, birthplace vs high school ($k)", lab = sprintf("$%.0fk", med), kind = "bar")) |>
  mutate(panel = factor(panel, levels = c("Share of QBs from the richest quarter of hometowns (%)",
                                          "QB median hometown income, birthplace vs high school ($k)")),
         x = factor(x, levels = c(era_levels, "Placed by\nbirthplace", "Placed by\nhigh school")))

p <- ggplot(combo, aes(x, value)) +
  geom_col(data = ~filter(.x, kind == "bar"), fill = pal_sport[["NFL"]], width = 0.6) +
  geom_line(data = ~filter(.x, kind == "line"), aes(group = 1), colour = pal_sport[["NFL"]], linewidth = 0.9) +
  geom_point(data = ~filter(.x, kind == "line"), colour = pal_sport[["NFL"]], size = 2.8) +
  geom_text(aes(label = lab), vjust = -0.8, size = 3.5, fontface = "bold", colour = ink_body) +
  facet_wrap(~panel, scales = "free") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "Quarterbacks keep coming from richer hometowns, and part of it is where they went to high school",
    subtitle = "NFL quarterbacks. Left: the rise over time. Right: QBs placed by high school come from richer hometowns than those placed by birthplace.",
    x = NULL, y = NULL,
    caption = fig_caption(
      "nflverse + ESPN + Sleeper hometowns matched to US Census place income",
      "\nNFL quarterbacks with a matched hometown. Richest quarter is the national 75th percentile of hometown median income, measured today.",
      "\nWe place a player by his high school when we know it, else birthplace. High-school placement pulls the income up because recruits who transfer\nto a well-off powerhouse (Bryce Young at Mater Dei is the type case) get tagged to that richer place. Even by birthplace alone, QBs still skew rich.")) +
  theme_hometown(grid = "y") +
  theme(strip.text = element_text(hjust = 0.5, size = rel(0.9)),
        panel.spacing = unit(1.8, "lines"),
        axis.text.x = element_text(size = rel(0.78), lineheight = 0.9))
save_fig("docs/figures/ba_qb_income.png", p, w = 12, h = 5.4)
cat("\ndone\n")
