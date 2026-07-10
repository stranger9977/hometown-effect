# =============================================================================
# 22_kickers.R -- the kicker segment: K/P vs other position groups on
# hometown income.
#
# Reproduces output/exploration/kp_wealth_0{1,2,3}.R with the repo's
# era-matched vintage income method (R/10_income.R): 1990s/2000s rookies are
# scored against 1999 place incomes (2000 Census), 2010s/2020s against
# current ACS incomes, with population-weighted quartile cutpoints per
# vintage so "Q4" always means the richest quarter of place-dwelling America
# at the right point in time.
#
# Outputs:
#   data/processed/kicker_money.csv   Q4 share by position group x era, plus
#                                     the suburban-ring 2020s comparison and
#                                     the drafted-vs-undrafted null test
#   docs/figures/kicker_money.png     Q4 share by era, K/P emphasized
# =============================================================================

suppressMessages({ library(dplyr); library(arrow); library(ggplot2); library(tidyr) })
source("R/lib/theme_hometown.R")
source("R/lib/bins.R")

matched <- read_parquet("data/processed/birthplace_matched.parquet") |>
  filter(match_tier != "unmatched", !is.na(era))
places  <- read_parquet("data/processed/census_places.parquet")
draft   <- read_parquet("data/raw/players.parquet") |>
  select(gsis_id, draft_round)
metro   <- read_parquet("data/processed/metro_distance.parquet") |>
  filter(sport == "NFL") |>
  select(gsis_id = player_id, band)

# Position groups. KR/PK are legacy specialist codes; LS is long snapper and
# stays with OL as in the exploration scripts.
pos_group <- function(p) case_when(
  p %in% c("QB") ~ "QB",
  p %in% c("RB", "FB", "HB", "WR", "TE") ~ "RB/WR/TE",
  p %in% c("T", "G", "C", "OL", "OT", "OG", "LS") ~ "OL",
  p %in% c("DE", "DT", "NT", "DL", "LB", "ILB", "OLB", "MLB", "EDGE") ~ "DL/LB",
  p %in% c("CB", "S", "SS", "FS", "DB", "SAF") ~ "DB",
  p %in% c("K", "P", "PK", "KR") ~ "K/P",
  TRUE ~ "other/unknown"
)

# ---- Era-matched vintage income quartiles (identical to R/10_income.R) ------
wq <- function(income, pop) {
  ok <- !is.na(income) & !is.na(pop) & pop > 0
  o <- order(income[ok])
  cw <- cumsum(as.numeric(pop[ok][o])) / sum(as.numeric(pop[ok][o]))
  list(income = income[ok][o], cw = cw)
}
cuts <- function(w) sapply(c(.25, .5, .75), function(q) w$income[which.max(w$cw >= q)])
cut99  <- cuts(wq(places$income1999, places$pop2000))
cutnow <- cuts(wq(places$income_now, places$pop_now))

df <- matched |>
  left_join(draft, by = "gsis_id") |>
  mutate(grp = pos_group(position),
         vintage = if_else(era %in% c("1990s", "2000s"), "1999", "now"),
         income = if_else(vintage == "1999", matched_income1999, matched_income_now),
         q = case_when(
           is.na(income) ~ NA_integer_,
           vintage == "1999" ~ findInterval(income, cut99) + 1L,
           TRUE              ~ findInterval(income, cutnow) + 1L),
         undrafted = is.na(draft_round)) |>
  filter(!is.na(q), grp != "other/unknown")

eras <- c("1990s", "2000s", "2010s", "2020s")
grp_levels <- c("K/P", "QB", "RB/WR/TE", "OL", "DL/LB", "DB")

# =============================================================================
# (1) Q4 share by position group x era
# =============================================================================
tab_grp <- df |>
  group_by(grp, era) |>
  summarise(n = n(), q4_share = mean(q == 4), .groups = "drop") |>
  mutate(metric = "q4_by_group_era", group = grp) |>
  select(metric, era, group, n, q4_share)

cat("== Q4 share by position group x era ==\n")
print(tab_grp |> mutate(q4_share = round(q4_share, 3)) |>
        pivot_wider(names_from = era, values_from = c(n, q4_share)) |>
        as.data.frame())

# =============================================================================
# (2) Within the suburban ring (15-45 mi from a 1M+ metro), 2020s:
#     K/P vs everyone else. Place type does not explain the K/P money.
# =============================================================================
ring <- df |>
  inner_join(metro, by = "gsis_id") |>
  filter(era == "2020s", band == "15-45mi") |>
  mutate(group = if_else(grp == "K/P", "K/P", "non-K/P")) |>
  group_by(group) |>
  summarise(n = n(), q4_share = mean(q == 4), .groups = "drop") |>
  mutate(metric = "suburban_ring_2020s", era = "2020s") |>
  select(metric, era, group, n, q4_share)

cat("\n== 2020s, suburban ring only (15-45 mi band): K/P vs non-K/P Q4 ==\n")
print(ring |> mutate(q4_share = round(q4_share, 3)) |> as.data.frame())

# Context print: the premium holds within EVERY place type, not just the ring.
cat("\n== 2020s Q4 share by metro-distance band, K/P vs non-K/P (context) ==\n")
print(df |>
        inner_join(metro, by = "gsis_id") |>
        filter(era == "2020s", !is.na(band)) |>
        mutate(g2 = if_else(grp == "K/P", "K/P", "non-K/P")) |>
        group_by(band, g2) |>
        summarise(n = n(), q4 = round(mean(q == 4), 3), .groups = "drop") |>
        pivot_wider(names_from = g2, values_from = c(n, q4)) |>
        as.data.frame())

# =============================================================================
# (3) The dead walk-on hypothesis: if specialist wealth came from walk-on
#     economics (undrafted players' families financing the long shot), the
#     undrafted should out-earn the drafted on hometown income everywhere.
#     Pooled across NON-K/P position groups, per era.
# =============================================================================
und <- df |>
  filter(grp != "K/P") |>
  mutate(group = if_else(undrafted, "undrafted", "drafted")) |>
  group_by(era, group) |>
  summarise(n = n(), q4_share = mean(q == 4), .groups = "drop") |>
  mutate(metric = "undrafted_vs_drafted_nonKP") |>
  select(metric, era, group, n, q4_share)

cat("\n== Drafted vs undrafted Q4 share, non-K/P pooled, by era ==\n")
print(und |> mutate(q4_share = round(q4_share, 3)) |>
        pivot_wider(names_from = group, values_from = c(n, q4_share)) |>
        mutate(gap_pts = round(100 * (q4_share_undrafted - q4_share_drafted), 1)) |>
        as.data.frame())

out <- bind_rows(tab_grp, ring, und) |>
  mutate(q4_share = round(q4_share, 4), small_cell = n < 30)
stopifnot(!any(is.na(out$q4_share)))
write.csv(out, "data/processed/kicker_money.csv", row.names = FALSE)
cat("\nwrote data/processed/kicker_money.csv (", nrow(out), "rows )\n")

# =============================================================================
# QA prints for the page: K vs P split, significance, town roll call
# =============================================================================
cat("\n== K vs P separately, 2020s (small n, flag in prose) ==\n")
print(df |> filter(grp == "K/P", era == "2020s") |>
        group_by(position) |>
        summarise(n = n(), q4 = round(mean(q == 4), 3), .groups = "drop") |>
        as.data.frame())

t20 <- df |> filter(era == "2020s")
x <- c(sum(t20$q[t20$grp == "K/P"] == 4), sum(t20$q[t20$grp != "K/P"] == 4))
n <- c(sum(t20$grp == "K/P"), sum(t20$grp != "K/P"))
pt <- suppressWarnings(prop.test(x, n))
cat(sprintf("\n2020s prop test: K/P %d/%d = %.3f vs non-K/P %d/%d = %.3f, p = %.4f\n",
            x[1], n[1], x[1] / n[1], x[2], n[2], x[2] / n[2], pt$p.value))

cat("\n== 2020s K/P Q4 hometowns (roll call) ==\n")
print(df |> filter(era == "2020s", grp == "K/P", q == 4) |>
        left_join(places |> select(geoid, place_name = name_raw, pstate = state),
                  by = "geoid") |>
        select(display_name, position, birth_city, pstate, income) |>
        arrange(desc(income)) |> as.data.frame())

# =============================================================================
# Figure: Q4 share by era, K/P emphasized against the five other groups
# =============================================================================
fig <- tab_grp |>
  mutate(era_n = as.integer(factor(era, levels = eras)),
         grp = factor(group, levels = grp_levels))

kp_col   <- "#0072B2"   # pal_sport NFL blue: the one accent
grey_ln  <- "grey78"
grey_lab <- "grey45"

ends <- fig |> filter(era == "2020s") |> arrange(q4_share)
# Deterministic label stacking: push labels apart bottom-up, min gap 2.2 pts.
lab_y <- ends$q4_share
for (i in seq_along(lab_y)[-1]) lab_y[i] <- max(lab_y[i], lab_y[i - 1] + 0.022)
ends$lab_y <- lab_y
ends <- ends |>
  mutate(lab = sprintf("%s %.0f%%", grp, 100 * q4_share),
         col = if_else(grp == "K/P", kp_col, grey_lab))

p <- ggplot(fig, aes(era_n, q4_share, group = grp)) +
  # Baseline as a segment (not geom_baseline) so it stops short of the
  # direct-label gutter and cannot strike through the end labels.
  annotate("segment", x = 1, xend = 4.06, y = 0.25, yend = 0.25,
           linetype = "dashed", colour = ink_baseline, linewidth = 0.35) +
  annotate("text", x = 1.02, y = 0.25 - 0.013, label = "25% = proportional share",
           colour = ink_baseline, size = 3.1, hjust = 0) +
  geom_line(data = fig |> filter(grp != "K/P"), colour = grey_ln, linewidth = 0.9) +
  geom_point(data = fig |> filter(grp != "K/P"), colour = grey_ln, size = 2.2) +
  geom_line(data = fig |> filter(grp == "K/P"), colour = kp_col, linewidth = 2) +
  geom_point(data = fig |> filter(grp == "K/P"), colour = kp_col, size = 3.4) +
  geom_text(data = ends, aes(x = era_n + 0.1, y = lab_y, label = lab),
            colour = ends$col, hjust = 0, size = 3.9,
            fontface = if_else(ends$grp == "K/P", "bold", "plain")) +
  scale_x_continuous(breaks = 1:4, labels = eras, limits = c(1, 4.85)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = c(0, 0.47)) +
  labs(title = "Kickers and punters come from the richest hometowns in football",
       subtitle = paste0("Share of NFL players whose hometown sits in the richest quartile of American places",
                         " (Q4 by median household income,\npopulation weighted), by position group and career-start era"),
       x = NULL, y = "Share from richest-quartile hometowns",
       caption = paste0(
         "Data: nflverse + ESPN birthplaces, US Census place incomes (2000 Census for 1990s-2000s rookies, ACS 2023 for 2010s-2020s).\n",
         "US-born players matched to a Census place; quartile cutpoints are population weighted within each income vintage.\n",
         "K/P n = 67-97 per era (small samples); other groups n = 105-1,342 per era. 2020s K/P vs all others: 43% vs 25%, p < 0.001.")) +
  theme_hometown() +
  theme(plot.margin = margin(10, 30, 8, 10)) +
  coord_cartesian(clip = "off")

save_fig("docs/figures/kicker_money.png", p)
