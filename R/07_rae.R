suppressMessages({ library(dplyr); library(arrow); library(ggplot2); library(lubridate) })

spine <- read_parquet("data/processed/spine.parquet") |>
  filter(!is.na(birth_date))

days_in_month <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
expected <- tibble(month = 1:12, expected_share = days_in_month / sum(days_in_month))

rae <- spine |>
  mutate(era = coalesce(era, "pre-1990"),
         month = month(birth_date)) |>
  count(era, month) |>
  group_by(era) |>
  mutate(share = n / sum(n)) |>
  ungroup() |>
  left_join(expected, by = "month") |>
  mutate(ratio = share / expected_share)

dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)
write.csv(rae, "data/processed/rae_table.csv", row.names = FALSE)

p <- rae |>
  mutate(month_lab = factor(month.abb[month], levels = month.abb)) |>
  ggplot(aes(month_lab, ratio, group = era)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  geom_col(fill = "#2C7FB8") +
  facet_wrap(~era, nrow = 1) +
  labs(title = "NFL births by month vs. expected",
       subtitle = "Ratio of player birth-month share to days-adjusted uniform baseline",
       x = NULL, y = "Observed / expected",
       caption = "Data: nflverse. Baseline: days-in-month adjusted uniform.") +
  theme_minimal(base_size = 16) +
  theme(panel.grid.minor = element_blank())

ggsave("docs/figures/rae_nfl.png", p, width = 12, height = 6.75, dpi = 320)
cat("wrote docs/figures/rae_nfl.png\n")
print(rae |> group_by(era) |> summarise(n = sum(n)))
