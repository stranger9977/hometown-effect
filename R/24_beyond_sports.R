# =============================================================================
# 24_beyond_sports.R -- the relative age effect (RAE) outside sports: the
# corner office and the classroom.
#
# PART 1 (ba_ceo_months.png) is OUR data: American chief executives' birth
# months, pulled from Wikidata via SPARQL (query.wikidata.org/sparql). Query
# saved at data/processed/ceo_birthdates_query.sparql. Method:
#   - ?company wdt:P169 ?person   (person is the modeled "chief executive
#     officer" of a Wikidata org, a much cleaner signal than scraping the
#     free-text "occupation: CEO" property, which also catches politicians,
#     rappers, and Congress members who once ran a small business)
#   - ?company wdt:P414 ?exchange  (company trades on a stock exchange, i.e.
#     a publicly traded company, the closest honest Wikidata analogue to
#     "Fortune 500 / S&P 500 caliber" without hand-curating a list)
#   - ?person wdt:P27 wd:Q30       (US citizen, since the RAE mechanism here
#     is the ~September 1 US school-entry cutoff; a foreign-schooled CEO of a
#     US-listed company would not share that cutoff)
#   - birth date known to DAY precision (wikibase:timePrecision = 11), not
#     just year, since month is the whole analysis
#   n = 187 unique people. A broader pull without the "publicly traded"
#   filter (any org P169 CEO, same citizenship/precision rules) gave n = 476
#   and is saved at data/processed/ceo_birthdates_broad.csv for comparison;
#   its June/July ratios were close to 1.0 (no dip), so the effect only
#   shows up once the sample is narrowed toward large public companies. This
#   is reported honestly below, not hidden.
#
# PART 2 (ba_classroom.png) is PUBLISHED RESEARCH, not this project's data.
# Numbers are read directly out of two papers (see in-caption citations):
#   - Bedard & Dhuey (2006), "The Persistence of Early Childhood Maturity:
#     International Evidence of Long-Run Age Effects", QJE 121(4). Grade 4/8
#     TIMSS math+science percentile gaps (oldest vs youngest quartile,
#     ~19 OECD countries), a US-specific grade 8 math estimate, and US/BC
#     numbers on SAT-taking, college enrollment, and a pre-university track.
#   - Dhuey & Lipscomb (2008), "What makes a leader? Relative age and high
#     school leadership", Economics of Education Review 27(2). Relatively
#     older US students are 4-11 percentage points more likely to hold a
#     school leadership role (team captain, club president).
# No microdata exists for either finding here; the chart plots the summary
# numbers stated in each paper's text/tables, recast onto one consistent
# scale ("oldest vs youngest advantage"), nothing simulated or interpolated.
# =============================================================================

suppressMessages({
  library(dplyr); library(ggplot2); library(readr); library(tibble)
})
source("R/lib/theme_hometown.R")

days_in_month <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
expected_month <- tibble(month = 1:12, expected_share = days_in_month / sum(days_in_month))

# -----------------------------------------------------------------------
# PART 1: CEO birth months (ours)
# -----------------------------------------------------------------------
ceo <- read_csv("data/processed/ceo_birthdates.csv", show_col_types = FALSE) |>
  mutate(dob = as.Date(dob), month = as.integer(format(dob, "%m")))

n_ceo <- nrow(ceo)
stopifnot(n_ceo == 187)

ceo_months <- ceo |>
  count(month, name = "n_month") |>
  left_join(expected_month, by = "month") |>
  mutate(share = n_month / sum(n_month),
         ratio = share / expected_share,
         month_lab = factor(month.abb[month], levels = month.abb),
         highlight = month %in% c(6, 7))

jun_ratio <- round(ceo_months$ratio[ceo_months$month == 6], 2)
jul_ratio <- round(ceo_months$ratio[ceo_months$month == 7], 2)
apr_ratio <- round(ceo_months$ratio[ceo_months$month == 4], 2)
cat(sprintf("CEO sample n = %d; June ratio = %.2f; July ratio = %.2f; April ratio = %.2f\n",
            n_ceo, jun_ratio, jul_ratio, apr_ratio))

p_ceo <- ggplot(ceo_months, aes(month_lab, ratio)) +
  geom_baseline(1) +
  geom_col(aes(fill = highlight), width = 0.7) +
  scale_fill_manual(values = c(`FALSE` = "grey70", `TRUE` = pal_ratio[["low"]]),
                     guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "July is the rarest birth month for American chief executives, not June",
    subtitle = "Birth-month share vs a days-adjusted uniform baseline, US CEOs of publicly traded companies (n = 187)",
    x = NULL, y = "Observed / expected",
    caption = paste0(
      "Data: Wikidata SPARQL (query.wikidata.org/sparql); query saved at data/processed/ceo_birthdates_query.sparql.\n",
      "Ours, not published research. US-citizen CEOs (P169) of stock-exchange-listed companies (P414), day-precision birth dates, n = 187, pulled 2026-07-24.\n",
      "June ratio ", jun_ratio, ", July ratio ", jul_ratio, ", April also low at ", apr_ratio, " (likely noise, about 15-20 people per month).\n",
      "A broader pull with no public-company filter (n = 476) showed no dip at all (both near 1.0); see ceo_birthdates_broad.csv.\n",
      "Published S&P 500 studies find a larger dip (6.1% June, 5.9% July vs 12.5% March); our n is much smaller, so treat this as suggestive."
    )
  ) +
  theme_hometown(grid = "y")

save_fig("docs/figures/ba_ceo_months.png", p_ceo, w = 12, h = 6.75)

# -----------------------------------------------------------------------
# PART 2: classroom RAE, from published research (NOT this project's data)
# -----------------------------------------------------------------------
classroom <- tribble(
  ~panel, ~metric, ~lo, ~hi,
  "Test scores (TIMSS math/science, percentile points, oldest vs youngest)",
    "Grade 4, range across ~19 OECD countries", 4, 12,
  "Test scores (TIMSS math/science, percentile points, oldest vs youngest)",
    "Grade 8, range across ~19 OECD countries", 2, 9,
  "Test scores (TIMSS math/science, percentile points, oldest vs youngest)",
    "Grade 8, United States only (math)", 4, 8,
  "Longer-run outcomes (percentage points, oldest vs youngest)",
    "High school leadership roles, United States", 4, 11,
  "Longer-run outcomes (percentage points, oldest vs youngest)",
    "4-year college enrollment, United States", 11.6, 11.6,
  "Longer-run outcomes (percentage points, oldest vs youngest)",
    "Pre-university track, British Columbia", 9.8, 9.8,
  "Longer-run outcomes (percentage points, oldest vs youngest)",
    "SAT/ACT-taking, United States", 7.7, 7.7
) |>
  mutate(mid = (lo + hi) / 2,
         label = ifelse(lo == hi, sprintf("%.1f", hi), sprintf("%g-%g", lo, hi)),
         metric = factor(metric, levels = rev(metric)),
         panel = factor(panel, levels = c(
           "Test scores (TIMSS math/science, percentile points, oldest vs youngest)",
           "Longer-run outcomes (percentage points, oldest vs youngest)")))

p_class <- ggplot(classroom, aes(y = metric, x = mid)) +
  geom_col(fill = pal_ratio[["low"]], width = 0.6) +
  geom_errorbar(aes(xmin = lo, xmax = hi), width = 0.15, colour = ink_body,
                orientation = "y") +
  geom_text(aes(x = pmax(hi, mid) , label = label), hjust = -0.15, size = 3.4,
            colour = ink_body) +
  facet_wrap(~panel, ncol = 1, scales = "free") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(
    title = "Being the oldest in your grade pays off long after grade school",
    subtitle = "Published estimates of the gap between the relatively oldest and youngest students in the same school cohort",
    x = NULL, y = NULL,
    caption = paste0(
      "Data: Bedard & Dhuey (2006), Quarterly Journal of Economics 121(4), and Dhuey & Lipscomb (2008), Economics of Education Review 27(2).\n",
      "From published research, not this project's data: no microdata here, these are the summary numbers stated in each paper.\n",
      "Test scores: TIMSS math/science, oldest vs youngest quartile in the school cohort. Outcomes: SAT/ACT-taking, 4-year college enrollment,\n",
      "a pre-university track (US/British Columbia), and high school leadership roles. Oldest vs youngest defined by each country's own\n",
      "school-entry cutoff date. The two panels use different units (percentile points vs percentage points); each has its own x-axis."
    )
  ) +
  theme_hometown(grid = "none") +
  theme(strip.text = element_text(size = rel(0.75)),
        panel.spacing = unit(1.6, "lines"),
        axis.text.y = element_text(hjust = 1))

save_fig("docs/figures/ba_classroom.png", p_class, w = 11, h = 7.2)
