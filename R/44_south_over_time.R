# =============================================================================
# 44_south_over_time.R -- Michael's question: has the rich-getting-richer income
# shift priced poorer productive states (Mississippi, Alabama, the Deep South)
# OUT of producing NFL players over time?
#
# Answer in the data: no, not at the regional level. The South's share of NFL
# players has RISEN over the eras, not fallen. But the income shift is real
# WITHIN the South: recent Southern players come from hometowns that are richer
# today than earlier Southern players' hometowns. So "rich getting richer" here
# is a shift toward richer neighborhoods inside the same productive states, not
# the South being squeezed out.
#
# NFL only: hometown.parquet is the one league with matched hometown income
# (high school location where known, else birthplace). Baseball and basketball
# have no hometown-income match here, noted in the caption.
# =============================================================================

suppressMessages({
  library(arrow); library(dplyr); library(ggplot2); library(tidyr); library(tibble)
})
source("R/lib/theme_hometown.R")

h <- read_parquet("data/processed/hometown.parquet") |>
  filter(!is.na(era), !is.na(hometown_state))
reg <- setNames(as.character(state.region), state.abb)
h <- h |> mutate(region = recode(reg[hometown_state], "North Central" = "Midwest"))

era_levels <- c("1990s", "2000s", "2010s", "2020s")

share_tbl <- h |>
  group_by(era) |>
  summarise(south_share = 100 * mean(region == "South", na.rm = TRUE), .groups = "drop")

income_tbl <- h |>
  filter(region == "South", !is.na(matched_income_now)) |>
  group_by(era) |>
  summarise(med_income = median(matched_income_now) / 1000, .groups = "drop")

cat("=== South share of NFL players by era ===\n")
print(as.data.frame(share_tbl |> mutate(south_share = round(south_share, 1))))
cat("\n=== median hometown income (thousands), Southern players, by era ===\n")
print(as.data.frame(income_tbl |> mutate(med_income = round(med_income, 1))))

plot_df <- bind_rows(
  share_tbl  |> transmute(era, value = south_share,
                          panel = "Share of NFL players from the South (%)",
                          lab = sprintf("%.0f%%", south_share)),
  income_tbl |> transmute(era, value = med_income,
                          panel = "Median hometown income, Southern players",
                          lab = sprintf("$%.0fk", med_income))) |>
  mutate(era = factor(era, levels = era_levels),
         panel = factor(panel, levels = c("Share of NFL players from the South (%)",
                                          "Median hometown income, Southern players")))

p <- ggplot(plot_df, aes(era, value, group = 1)) +
  geom_line(colour = pal_sport[["NFL"]], linewidth = 0.9) +
  geom_point(colour = pal_sport[["NFL"]], size = 2.8) +
  geom_text(aes(label = lab), vjust = -1.0, size = 3.5, fontface = "bold", colour = ink_body) +
  facet_wrap(~panel, scales = "free_y") +
  scale_y_continuous(expand = expansion(mult = c(0.12, 0.16))) +
  labs(
    title = "The Deep South is not being priced out. It makes more NFL players than ever, from richer parts of the South",
    subtitle = "NFL players with a Southern hometown, by rookie era. Left: share of all players. Right: the current income of their hometowns.",
    x = NULL, y = NULL,
    caption = fig_caption(
      "nflverse + ESPN + Sleeper hometowns matched to US Census place income",
      "\nNFL only (the one league with matched hometown income here); hometown is high school location where known, else birthplace.",
      "\nThe South's share of players rose across the eras rather than falling, so the region is not being squeezed out. Income is each era's players'\nhometown median household income measured today, so a rising line means recent Southern players come from places that are richer now.")) +
  theme_hometown(grid = "y") +
  theme(strip.text = element_text(hjust = 0.5, size = rel(0.92)),
        panel.spacing = unit(1.8, "lines"))
save_fig("docs/figures/ba_south_over_time.png", p, w = 12, h = 5.2)
cat("\ndone\n")
