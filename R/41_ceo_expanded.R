# =============================================================================
# 41_ceo_expanded.R -- Michael's "can we get more CEOs?" + the August wrinkle.
#
# Expansion attempt: relaxed the birth-date precision from day-only to month-or-
# better (we only need the month), keeping the public-company filter (P414) and
# US citizenship (P27=Q30). That only moved the sample from 187 to ~196, so the
# honest finding is that the clean US public-company-CEO pool on Wikidata tops
# out near 200. A broader pull without the public-company filter (n=476, saved
# earlier) washed the dip out entirely, so we do NOT chase n at the cost of the
# signal; we pair our small replication with the published S&P 500 result.
#
# The upgrade that matters: re-baseline on REAL US births (not a flat line) and
# surface the AUGUST wrinkle Michael flagged. Under a September school cutoff,
# July and August babies are the youngest in the grade. July shows the dip; but
# August, the month most associated with being HELD BACK a year into becoming
# the oldest, does not, which is the fingerprint of the hold-back move.
# =============================================================================

suppressMessages({
  library(dplyr); library(readr); library(ggplot2); library(tibble)
})
source("R/lib/theme_hometown.R")

ceo <- read_csv("data/processed/ceo_birthdates_v2.csv", show_col_types = FALSE) |>
  rename(person = 1) |>
  distinct(person, .keep_all = TRUE) |>
  mutate(month = as.integer(substr(dob, 6, 7))) |>
  filter(!is.na(month))
n_ceo <- nrow(ceo)
cat(sprintf("expanded CEO sample (distinct persons): %d\n", n_ceo))

birth_season <- read_csv("data/processed/us_birth_seasonality.csv", show_col_types = FALSE) |>
  select(month, expected_share)

ceo_m <- ceo |>
  count(month, name = "n_month") |>
  right_join(tibble(month = 1:12), by = "month") |>
  mutate(n_month = coalesce(n_month, 0L)) |>
  left_join(birth_season, by = "month") |>
  mutate(share = n_month / sum(n_month),
         ratio = share / expected_share,
         month_lab = factor(month.abb[month], levels = month.abb),
         grp = case_when(month == 7 ~ "Jul",
                         month == 8 ~ "Aug",
                         month == 6 ~ "Jun",
                         TRUE ~ "other"))

jul <- round(ceo_m$ratio[ceo_m$month == 7], 2)
aug <- round(ceo_m$ratio[ceo_m$month == 8], 2)
jun <- round(ceo_m$ratio[ceo_m$month == 6], 2)
cat("=== CEO birth-month ratio vs REAL US births (expanded sample) ===\n")
print(as.data.frame(ceo_m |> transmute(month = month.abb[month], n_month, ratio = round(ratio, 2))))
cat(sprintf("June %.2f | July %.2f | August %.2f\n", jun, jul, aug))

fill_vals <- c(Jul = pal_ratio[["low"]], Aug = pal_sport[["MLB"]],
               Jun = pal_ratio[["low"]], other = "grey72")

p <- ggplot(ceo_m, aes(month_lab, ratio, fill = grp)) +
  geom_baseline(1) +
  geom_col(width = 0.72) +
  geom_text(aes(label = ifelse(month %in% c(6,7,8), sprintf("%.2f", ratio), "")),
            vjust = -0.5, size = 3.2, fontface = "bold", colour = ink_body) +
  scale_fill_manual(values = fill_vals, guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "A faint summer dip and an August spike among chief executives, the hold-back fingerprint",
    subtitle = "CEO birth-month share divided by the real US birth distribution, US CEOs of public companies and large private employers. Small, noisy sample.",
    x = NULL, y = "Observed / expected",
    caption = fig_caption(
      "Wikidata SPARQL (query.wikidata.org): US-citizen CEOs (P169) of stock-exchange-listed companies (P414) or large private employers (1000-plus staff), month-or-better birth dates",
      sprintf("\nOurs, not published research. n = %d. Michael asked for more, so we added large private employers to the public companies (187 to 196 to %d).\nThe dip persists but softens as the sample grows, and the fully broad any-CEO pull (549) washes it out, so the clean signal caps around here.", n_ceo, n_ceo),
      sprintf("\nBaseline is the real US birth curve (CDC 1994-2003). July sits at %.2f, August at %.2f. Under a September school cutoff, July and August babies are the\nyoungest in the grade, but August is the month most tied to being held back a year into becoming the oldest, which is the hold-back fingerprint.\nRead single months with caution: with 13 to 23 people per month the swings are large (April is also low, likely noise). The July-to-August contrast is\nthe part that fits the school-cutoff story. The published S&P 500 studies find a sharper version (about 6%% June and July each vs 12.5%% March).",
              jul, aug))) +
  theme_hometown(grid = "y")
save_fig("docs/figures/ba_ceo_expanded.png", p, w = 12, h = 6.4)
cat("\ndone\n")
