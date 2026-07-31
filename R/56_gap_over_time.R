# =============================================================================
# 56_gap_over_time.R -- Michael's "widening Matthew Principle" question, tested.
#
# General population, NOT sports. We bin every US county into equal-population
# income fifths IN EACH YEAR, then track the richest fifth of places vs the
# poorest fifth of places on three outcomes over time: median household income
# (constant 2023 dollars), the adult bachelor's-degree share, and the home
# ownership rate. If the top and bottom lines fan apart, richer places are
# pulling away; if they hold their spacing, the gap is steady.
#
# Years: 2000 decennial SF3 (income is 1999, our "1990s-ish" anchor; 1990 is not
# on the API), plus ACS 5-year 2010, 2015, 2019, 2023. Incomes are deflated to
# 2023 dollars with CPI-U annual averages, which Michael asked for.
#
# HONESTY: this supersedes R/55, which found the income RATIO roughly flat near
# 2.1x for 2010-2023. We add the 2000 anchor and two more outcomes and report
# what the data actually shows, ratio AND absolute gap, without forcing a story.
#
# NOTE on one source correction: for 2000 home ownership we use tenure table
# H007 (H007002 owner / H007001 total occupied). The H004 table named in the
# brief is "percent of occupied/vacant units in sample" (a sampling-coverage
# percentage), not tenure counts, so it is the wrong table.
# =============================================================================

suppressMessages({
  library(jsonlite); library(curl); library(dplyr); library(ggplot2)
  library(tidyr); library(tibble)
})
source("R/lib/theme_hometown.R")

KEY <- Sys.getenv("CENSUS_API_KEY"); stopifnot(nzchar(KEY))

# CPI-U annual averages (BLS). deflator = 2023 CPI / year CPI -> constant 2023 $.
# 2000 SF3 income is 1999 dollars; we use the 2000 CPI as a close proxy (noted).
cpi <- c(`2000` = 172.200, `2010` = 218.056, `2015` = 237.017,
         `2019` = 255.657, `2023` = 304.702)

fetch <- function(url) {
  res <- curl_fetch_memory(url)
  if (res$status_code != 200)
    stop(sprintf("census status %d for %s", res$status_code, url))
  m <- fromJSON(rawToChar(res$content))
  d <- as.data.frame(m[-1, , drop = FALSE], stringsAsFactors = FALSE)
  names(d) <- m[1, ]
  d
}

num <- function(x) suppressWarnings(as.numeric(x))

# --- 2000 decennial SF3 --------------------------------------------------------
pull_2000 <- function() {
  vars <- c("P053001", "P001001", "P037001",
            "P037015", "P037016", "P037017", "P037018",
            "P037032", "P037033", "P037034", "P037035",
            "H007001", "H007002")
  url <- sprintf("https://api.census.gov/data/2000/dec/sf3?get=%s&for=county:*&in=state:*&key=%s",
                 paste(vars, collapse = ","), KEY)
  fetch(url) |>
    transmute(
      year      = 2000L,
      income    = num(P053001),
      pop       = num(P001001),
      edu_total = num(P037001),
      ba_plus   = num(P037015) + num(P037016) + num(P037017) + num(P037018) +
                  num(P037032) + num(P037033) + num(P037034) + num(P037035),
      occ_total = num(H007001),
      owner     = num(H007002))
}

# --- ACS 5-year ---------------------------------------------------------------
# Education uses B15002 (Sex by Educational Attainment, 25+) for every ACS year.
# B15003 (the un-sexed version named in the brief) does not exist in ACS 2010
# (it starts in 2012); B15002 exists in all years, sums to the identical BA+
# count as B15003, and mirrors the 2000 P037 cells exactly, so the whole series
# uses one consistent definition. BA+ = Male + Female (015-018, 032-035).
pull_acs <- function(yr) {
  vars <- c("B19013_001E", "B01003_001E",
            "B15002_001E", "B15002_015E", "B15002_016E", "B15002_017E", "B15002_018E",
            "B15002_032E", "B15002_033E", "B15002_034E", "B15002_035E",
            "B25003_001E", "B25003_002E")
  url <- sprintf("https://api.census.gov/data/%d/acs/acs5?get=%s&for=county:*&in=state:*&key=%s",
                 yr, paste(vars, collapse = ","), KEY)
  fetch(url) |>
    transmute(
      year      = yr,
      income    = num(B19013_001E),
      pop       = num(B01003_001E),
      edu_total = num(B15002_001E),
      ba_plus   = num(B15002_015E) + num(B15002_016E) + num(B15002_017E) + num(B15002_018E) +
                  num(B15002_032E) + num(B15002_033E) + num(B15002_034E) + num(B15002_035E),
      occ_total = num(B25003_001E),
      owner     = num(B25003_002E))
}

raw <- bind_rows(
  pull_2000(),
  pull_acs(2010), pull_acs(2015), pull_acs(2019), pull_acs(2023))

# Keep counties with a usable income and population (income defines the bins),
# then deflate income to constant 2023 dollars.
county <- raw |>
  filter(!is.na(income), income > 0, !is.na(pop), pop > 0) |>
  mutate(income = income * cpi["2023"] / cpi[as.character(year)])

# --- bin into equal-population income fifths within each year -----------------
county <- county |>
  group_by(year) |>
  arrange(income, .by_group = TRUE) |>
  mutate(fifth = cut(cumsum(pop),
                     breaks = c(0, seq(0.2, 1, 0.2) * sum(pop)),
                     labels = 1:5, include.lowest = TRUE)) |>
  ungroup()

# --- summarise the top (5) and bottom (1) fifth on each outcome ---------------
# income:    population-weighted mean of county median incomes
# ba / own:  true aggregate share = sum(numerator) / sum(denominator)
fifth_tbl <- county |>
  filter(fifth %in% c(1, 5)) |>
  group_by(year, fifth) |>
  summarise(
    income   = weighted.mean(income, pop),
    ba_share = 100 * sum(ba_plus, na.rm = TRUE) / sum(edu_total, na.rm = TRUE),
    own_share = 100 * sum(owner, na.rm = TRUE) / sum(occ_total, na.rm = TRUE),
    .groups = "drop") |>
  mutate(band = ifelse(fifth == 5, "top", "bottom"))

# --- overall population-weighted median income per year (for the income panel) -
pwm <- function(x, w) {
  o <- order(x); x <- x[o]; w <- w[o]
  x[which(cumsum(w) >= 0.5 * sum(w))[1]]
}
median_tbl <- county |>
  group_by(year) |>
  summarise(income = pwm(income, pop), .groups = "drop")

# =============================================================================
# PRINTED NUMBERS: per outcome and year, top fifth, bottom fifth, ratio + gap
# =============================================================================
wide <- fifth_tbl |>
  pivot_longer(c(income, ba_share, own_share), names_to = "outcome") |>
  pivot_wider(names_from = band, values_from = value, id_cols = c(year, outcome)) |>
  mutate(ratio = top / bottom, gap = top - bottom)

fmt_outcome <- function(o, top, bottom, ratio, gap) {
  if (o == "income")
    sprintf("  top $%6.0f   bottom $%6.0f   ratio %.2fx   gap $%.0f",
            top, bottom, ratio, gap)
  else
    sprintf("  top %5.1f%%   bottom %5.1f%%   ratio %.2fx   gap %.1f pts",
            top, bottom, ratio, gap)
}
labels_o <- c(income = "MEDIAN HOUSEHOLD INCOME (2023 $)",
              ba_share = "BACHELOR'S DEGREE OR HIGHER, ADULTS 25+ (%)",
              own_share = "HOME OWNERSHIP RATE (%)")
for (o in c("income", "ba_share", "own_share")) {
  cat("\n===", labels_o[[o]], "-- richest vs poorest fifth of places ===\n")
  sub <- wide |> filter(outcome == o) |> arrange(year)
  for (i in seq_len(nrow(sub)))
    cat(sprintf("%d %s\n", sub$year[i],
                fmt_outcome(o, sub$top[i], sub$bottom[i], sub$ratio[i], sub$gap[i])))
}
cat("\n=== overall population-weighted median household income (2023 $) ===\n")
for (i in seq_len(nrow(median_tbl)))
  cat(sprintf("%d  $%6.0f\n", median_tbl$year[i], median_tbl$income[i]))

# =============================================================================
# FIGURE: one 3-panel figure, facet by outcome, richest vs poorest fifth line
# =============================================================================
out_levels <- c("Median household income (2023 dollars)",
                "Bachelor's degree or higher, adults 25+ (%)",
                "Home ownership rate (%)")

lines_df <- fifth_tbl |>
  transmute(year, band,
            group = ifelse(band == "top", "Richest fifth of places",
                           "Poorest fifth of places"),
            `Median household income (2023 dollars)`       = income,
            `Bachelor's degree or higher, adults 25+ (%)`  = ba_share,
            `Home ownership rate (%)`                      = own_share) |>
  pivot_longer(all_of(out_levels), names_to = "outcome", values_to = "value") |>
  mutate(outcome = factor(outcome, levels = out_levels))

# median reference line: income panel only
med_df <- median_tbl |>
  transmute(year, value = income, group = "Typical place (median)",
            outcome = factor(out_levels[1], levels = out_levels))

yrs <- sort(unique(county$year))

lab_line <- function(o, v) {
  if (grepl("income", o)) sprintf("$%.0fk", v / 1000) else sprintf("%.0f%%", v)
}
ends <- lines_df |> filter(year == max(yrs)) |>
  mutate(lab = mapply(lab_line, as.character(outcome), value),
         # richest and poorest end on the SAME 66.0% ownership point; split the
         # two labels vertically there so they do not print on top of each other
         ny = case_when(
           grepl("ownership", outcome) & band == "top"    ~ value + 0.55,
           grepl("ownership", outcome) & band == "bottom" ~ value - 0.55,
           TRUE ~ value))
ends_med <- med_df |> filter(year == max(yrs)) |>
  mutate(lab = sprintf("$%.0fk", value / 1000))

col_vals <- c("Richest fifth of places" = pal_sport[["NFL"]],
              "Poorest fifth of places" = "grey60",
              "Typical place (median)"  = "grey40")

p <- ggplot(lines_df, aes(year, value, colour = group)) +
  # income-panel median line, dashed, drawn first so it sits behind
  geom_line(data = med_df, linewidth = 0.7, linetype = "22") +
  geom_point(data = med_df, size = 1.8) +
  geom_line(linewidth = 1.05) +
  geom_point(size = 2.3) +
  geom_text(data = ends, aes(y = ny, label = lab), hjust = 0, nudge_x = 0.9,
            size = 3.3, fontface = "bold", show.legend = FALSE) +
  geom_text(data = ends_med, aes(label = lab), hjust = 0, nudge_x = 0.9,
            size = 3.0, fontface = "bold", show.legend = FALSE) +
  facet_wrap(~outcome, scales = "free_y", nrow = 1) +
  scale_colour_manual(values = col_vals, name = NULL,
                      breaks = c("Richest fifth of places",
                                 "Typical place (median)",
                                 "Poorest fifth of places")) +
  scale_x_continuous(breaks = yrs,
                     limits = c(min(yrs), max(yrs) + 4.5),
                     expand = expansion(mult = c(0.02, 0))) +
  scale_y_continuous(expand = expansion(mult = c(0.06, 0.12))) +
  coord_cartesian(clip = "off") +
  labs(
    title = "The gap between rich and poor places is wide but mostly holding steady, not widening",
    subtitle = "US counties binned into equal-population income fifths each year. Lines track the richest and poorest fifth of places, with the typical (median) place on the income panel.",
    x = NULL, y = NULL,
    caption = fig_caption(
      "US Census 2000 SF3 (1999 income) and ACS 5-year 2010, 2015, 2019 and 2023, all US counties binned into equal-population income fifths each year",
      "\nIncome is deflated to constant 2023 dollars with CPI-U annual averages; the 2000 value uses the 2000 CPI as a close proxy for 1999 income.\nEach fifth holds about a fifth of the US population that year. Bachelor share is adults 25 and older; home ownership is the owner-occupied\nshare of occupied homes. General population, no sports and no race or ethnicity in this chart.",
      "\nVerdict: the income ratio held near 2.1 times for 23 years and the real-dollar gap grew only about 11 percent.\nThe college-degree gap widened by roughly 5 points, but its ratio narrowed as poorer places gained faster.\nHome ownership shows no gap and closed to zero by 2023, so the widening Matthew Principle mostly does not hold.")) +
  theme_hometown(grid = "y") +
  theme(legend.position = "top", legend.justification = "left",
        strip.text = element_text(size = rel(0.9)),
        panel.spacing = unit(1.7, "lines"),
        plot.margin = margin(10, 40, 8, 10))

save_fig("docs/figures/ba_gap_over_time.png", p, w = 13, h = 5.6)
cat("\ndone\n")
