# =============================================================================
# 24_rae_soccer_swim.R -- RAE extension to soccer and swimming.
# Data is NOT in the existing spine/players parquet pipeline: soccer and
# swimming birthdates were pulled fresh from Wikidata (SPARQL) since neither
# sport has a birthdate source already wired into this repo. Queries are
# saved at data/raw/wikidata/{soccer,swim}_query.rq for reproducibility;
# results (person id, date of birth) are data/raw/wikidata/{soccer,swim}_dob.csv.
#
# Soccer query: humans with occupation "association football player"
# (wd:Q937857) AND citizenship USA (wd:Q30) AND a recorded date of birth,
# born 1985-2009. This is a broad Wikidata universe (MLS, USMNT/USWNT,
# college, lower-division, and some dual/territorial citizens are all
# in there) -- NOT limited to MLS or national-team rosters. Deduped to one
# row per person (a handful had multiple sourced DOBs; first kept).
#
# Swim query: same shape, occupation "swimmer" (wd:Q10843402), born 1970-2009.
#
# Soccer is split into two birth-year cohorts to probe US Soccer's youth
# age-cutoff move from Aug 1 to a Jan 1 calendar year, adopted for the
# 2016-17 season:
#   born 1985-1999: grew up entirely under the old Aug 1 cutoff
#   born 2003-2009: youth-national-team-eligible ages (roughly 13-18) fall
#                   entirely after the 2016 switch, i.e. grew up under Jan 1
#   born 2000-2002 (transition years) are dropped from the comparison
#
# Baseline identical to R/07, R/17, R/23: days-in-month adjusted uniform
# (Feb = 28.25). Reads data/raw/wikidata/*.csv only; writes one figure.
# =============================================================================

suppressMessages({
  library(dplyr); library(readr); library(ggplot2); library(lubridate); library(tibble)
})
source("R/lib/theme_hometown.R")

days_in_month <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
expected_month <- tibble(month = 1:12, expected_share = days_in_month / sum(days_in_month))
quarter_of_month <- function(m) ceiling(m / 3)
q_days <- tapply(days_in_month, quarter_of_month(1:12), sum)
expected_q <- tibble(quarter = as.integer(names(q_days)),
                     expected_share = as.numeric(q_days) / sum(days_in_month))

soccer_raw <- read_csv("data/raw/wikidata/soccer_dob.csv", show_col_types = FALSE) |>
  mutate(dob = as.Date(dob), byear = year(dob), month = month(dob),
         quarter = quarter_of_month(month))

swim_raw <- read_csv("data/raw/wikidata/swim_dob.csv", show_col_types = FALSE) |>
  mutate(dob = as.Date(dob), byear = year(dob), month = month(dob),
         quarter = quarter_of_month(month))

soc_old <- soccer_raw |> filter(byear <= 1999) |> mutate(panel = "Soccer, born 1985-1999\n(grew up under the old Aug 1 cutoff)")
soc_new <- soccer_raw |> filter(byear >= 2003) |> mutate(panel = "Soccer, born 2003-2009\n(youth-eligible under the 2016+ Jan 1 cutoff)")
swim    <- swim_raw   |> mutate(panel = "Swimming, born 1970-2009")

panel_levels <- c(
  "Soccer, born 1985-1999\n(grew up under the old Aug 1 cutoff)",
  "Soccer, born 2003-2009\n(youth-eligible under the 2016+ Jan 1 cutoff)",
  "Swimming, born 1970-2009"
)

pooled <- bind_rows(soc_old, soc_new, swim) |>
  mutate(panel = factor(panel, levels = panel_levels),
         sport_group = ifelse(grepl("^Soccer", panel), "Soccer", "Swimming"))

# --- exact n's for the caption --------------------------------------------
n_soc_old <- nrow(soc_old)
n_soc_new <- nrow(soc_new)
n_swim    <- nrow(swim)
n_soc_all <- nrow(soccer_raw)

# --- month-level ratio table -------------------------------------------------
month_tbl <- pooled |>
  count(panel, sport_group, month) |>
  group_by(panel) |>
  mutate(share = n / sum(n)) |>
  ungroup() |>
  left_join(expected_month, by = "month") |>
  mutate(ratio = share / expected_share,
         month_lab = factor(month.abb[month], levels = month.abb))

cat("=== birth-month ratio, soccer (by cohort) and swimming ===\n")
print(as.data.frame(month_tbl |> select(panel, month_lab, n, ratio)), row.names = FALSE)

# --- quarter-level ratio, cited in the caption ------------------------------
quarter_tbl <- pooled |>
  count(panel, quarter) |>
  group_by(panel) |>
  mutate(share = n / sum(n)) |>
  ungroup() |>
  left_join(expected_q, by = "quarter") |>
  mutate(ratio = share / expected_share)

cat("\n=== birth-quarter ratio (Q1 = Jan-Mar ... Q4 = Oct-Dec) ===\n")
print(as.data.frame(quarter_tbl |> select(panel, quarter, n, ratio)), row.names = FALSE)

q1_old  <- quarter_tbl$ratio[quarter_tbl$panel == panel_levels[1] & quarter_tbl$quarter == 1]
q1_new  <- quarter_tbl$ratio[quarter_tbl$panel == panel_levels[2] & quarter_tbl$quarter == 1]
q1_swim <- quarter_tbl$ratio[quarter_tbl$panel == panel_levels[3] & quarter_tbl$quarter == 1]
q4_old  <- quarter_tbl$ratio[quarter_tbl$panel == panel_levels[1] & quarter_tbl$quarter == 4]
q4_new  <- quarter_tbl$ratio[quarter_tbl$panel == panel_levels[2] & quarter_tbl$quarter == 4]
q4_swim <- quarter_tbl$ratio[quarter_tbl$panel == panel_levels[3] & quarter_tbl$quarter == 4]

pal_soc_swim <- c(Soccer = "#0072B2", Swimming = "#E69F00")

p <- ggplot(month_tbl, aes(month_lab, ratio, fill = sport_group)) +
  geom_baseline(1) +
  geom_col(width = 0.7) +
  geom_text(aes(label = sprintf("%.2f", ratio)), vjust = -0.5, size = 2.5,
           colour = ink_body) +
  facet_wrap(~panel, nrow = 1) +
  scale_fill_manual(values = pal_soc_swim, guide = "none") +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "January-born athletes are overrepresented in soccer and swimming, just like hockey",
    subtitle = "Birth-month share vs a days-adjusted uniform baseline; soccer split by birth cohort around the 2016 US Soccer age-cutoff change",
    x = NULL, y = "Representation ratio (1 = expected)",
    caption = paste0(
      "Data: Wikidata SPARQL query for people tagged as association football player (soccer) or swimmer, ",
      "US citizenship, with a recorded birth date; retrieved July 2026.\n",
      sprintf("Soccer n = %s total; born 1985-1999 n = %s, born 2003-2009 n = %s (birth years 2000-2002 dropped as a transition window). Swimming n = %s, born 1970-2009.\n",
              format(n_soc_all, big.mark = ","), format(n_soc_old, big.mark = ","),
              format(n_soc_new, big.mark = ","), format(n_swim, big.mark = ",")),
      sprintf("Jan-Mar (Q1) is overrepresented in every panel: ratio %.2f (old soccer cohort), %.2f (new soccer cohort), %.2f (swimming).\n",
              q1_old, q1_new, q1_swim),
      sprintf("Oct-Dec (Q4) is underrepresented in every panel: ratio %.2f (old soccer cohort), %.2f (new soccer cohort), %.2f (swimming).\n",
              q4_old, q4_new, q4_swim),
      "The skew predates the 2016 cutoff move, since it is already there in the 1985-1999 cohort, and it got larger, not smaller, after the change.\n",
      sprintf("The newer cohort is much smaller (n = %s) and newer to Wikipedia, so some of that jump may be notability bias toward early standouts, not a pure cutoff effect.\n",
              format(n_soc_new, big.mark = ",")),
      "Universe is broad (Wikidata occupation and citizenship tags), not limited to MLS or Olympic rosters; some dual or territorial citizens may be included.\n",
      "Baseline: days-in-month adjusted uniform (Feb = 28.25). Our-computed data."
    )
  ) +
  theme_hometown() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = rel(0.75)),
        strip.text = element_text(hjust = 0.5, size = rel(0.78)))

save_fig("docs/figures/ba_soccer_swim.png", p, w = 14, h = 7.6)

cat("\ndone\n")
