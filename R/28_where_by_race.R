# =============================================================================
# 28_where_by_race.R -- Michael's "where, controlled" asks, handled carefully.
#
# WHAT WE CAN AND CANNOT SAY. The player data (nflverse + ESPN + Sleeper) has
# NO race field. So this script makes NO claim about any individual's odds by
# race, and none about any group being better or worse at football. That would
# be both unsupported by the data and not the point. Everything here is
# ECOLOGICAL: it describes COUNTIES and REGIONS (how many NFL players they
# produce per resident, and who lives there), never people. Ecological patterns
# cannot be read down to individuals (the ecological fallacy); the captions say
# so out loud.
#
# The honest questions we CAN answer with county data:
#   1. What share of NFL players comes from each region, and is a region over-
#      or under-represented once you divide by its population? (Michael's "four
#      quadrants, made fair.")
#   2. The South produces the most players per capita. Michael's read: a lot of
#      that may be because most Black Americans live in the South. We test the
#      geography of that: the South's share of the US Black population, and
#      whether counties with larger Black populations produce more NFL players
#      per resident, shown WITHIN region so it is not just "the South."
#
# Race/population by county: Census Population Estimates county characteristics
# (cc-est2024, keyless). Black = "Black or African American alone." Player
# counts + county populations: data/processed/county_rates.csv (from R/09).
# =============================================================================

suppressMessages({
  library(dplyr); library(readr); library(ggplot2); library(stringr); library(tidyr)
})
source("R/lib/theme_hometown.R")

# --- county race/population (keyless popest characteristics) ----------------
cc_path <- "data/raw/census/cc-est2024-alldata.csv"
if (!file.exists(cc_path)) {
  dir.create(dirname(cc_path), showWarnings = FALSE, recursive = TRUE)
  download.file(
    "https://www2.census.gov/programs-surveys/popest/datasets/2020-2024/counties/asrh/cc-est2024-alldata.csv",
    cc_path, mode = "wb", quiet = TRUE)
}

cc_raw <- read_csv(cc_path, col_types = cols(.default = col_character()),
                   locale = locale(encoding = "latin1"))
latest_year <- max(as.integer(cc_raw$YEAR))   # 6 = 7/1/2024 vintage

race <- cc_raw |>
  filter(SUMLEV == "050", AGEGRP == "0", YEAR == as.character(latest_year)) |>
  transmute(
    county_fips = paste0(STATE, COUNTY),
    stname = STNAME,
    tot_pop = as.numeric(TOT_POP),
    black   = as.numeric(BA_MALE) + as.numeric(BA_FEMALE),   # Black alone
    white   = as.numeric(WA_MALE) + as.numeric(WA_FEMALE)) |> # White alone
  mutate(black_share = black / tot_pop)

# --- Census region from state name ------------------------------------------
region_of <- setNames(as.character(state.region), state.name)
region_of["District of Columbia"] <- "South"   # Census places DC in the South
race <- race |>
  mutate(region = recode(region_of[stname], "North Central" = "Midwest")) |>
  filter(!is.na(region))

# --- join player counts (per-county) ----------------------------------------
rates <- read_csv("data/processed/county_rates.csv", show_col_types = FALSE) |>
  mutate(county_fips = str_pad(as.character(county_fips), 5, pad = "0")) |>
  select(county_fips, players)

co <- race |>
  left_join(rates, by = "county_fips") |>
  mutate(players = coalesce(players, 0))

# ============================================================================
# 1. Region per-capita: players, population, share vs share, players per million
# ============================================================================
region_levels <- c("South", "West", "Midwest", "Northeast")
region_tbl <- co |>
  group_by(region) |>
  summarise(players = sum(players), pop = sum(tot_pop), .groups = "drop") |>
  mutate(player_share = players / sum(players),
         pop_share    = pop / sum(pop),
         per_million  = 1e6 * players / pop,
         index        = player_share / pop_share,   # 1 = its fair population share
         region = factor(region, levels = region_levels)) |>
  arrange(region)

us_rate <- 1e6 * sum(co$players) / sum(co$tot_pop)

cat("=== (1) NFL production by region ===\n")
print(region_tbl |>
        mutate(player_pct = round(100 * player_share, 1),
               pop_pct = round(100 * pop_share, 1),
               per_million = round(per_million, 1),
               index = round(index, 2)) |>
        select(region, players, player_pct, pop_pct, per_million, index) |>
        as.data.frame())
cat(sprintf("US overall: %.1f players per million\n\n", us_rate))

p_region <- ggplot(region_tbl, aes(reorder(region, per_million), per_million)) +
  geom_col(fill = pal_sport[["NFL"]], width = 0.68) +
  geom_hline(yintercept = us_rate, linetype = "dashed", colour = ink_baseline,
             linewidth = 0.4) +
  annotate("text", x = 0.7, y = us_rate, vjust = -0.6, hjust = 0,
           label = sprintf("US average %.0f", us_rate), size = 3.1, colour = ink_body) +
  geom_text(aes(label = sprintf("%.0f", per_million)), hjust = -0.2,
            size = 3.6, fontface = "bold", colour = ink_body) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = "The South produces NFL players at more than double the national rate",
    subtitle = "NFL players per million residents by region, already divided by each region's population",
    x = NULL, y = "NFL players per million residents",
    caption = fig_caption(
      "nflverse + ESPN + Sleeper (players, rookie seasons 1990-2025); US Census population estimates (cc-est2024)",
      "\nPlayers placed by hometown county (high school where known, else birthplace); regions are the four US Census regions.",
      "\nThis is production per resident, so it already controls for the fact that the South and West hold more people.")) +
  theme_hometown(grid = "none")
save_fig("docs/figures/ba_region_percapita.png", p_region, w = 11, h = 4.6)

# ============================================================================
# 2. The race geography, ecological and within-region.
# ============================================================================
# 2a. Where the US Black population lives.
black_by_region <- co |>
  group_by(region) |>
  summarise(black = sum(black), .groups = "drop") |>
  mutate(share = black / sum(black)) |>
  arrange(desc(share))
south_black_share <- black_by_region$share[black_by_region$region == "South"]
cat("=== (2a) share of the US Black population by region ===\n")
print(black_by_region |> mutate(share = round(100 * share, 1)) |> as.data.frame())
cat(sprintf("South holds %.0f%% of the US Black population\n\n", 100 * south_black_share))

# 2b. County Black-population share bins -> NFL players per million, by region.
# Weighted by population (aggregate players and pop per bin), so big and small
# counties are pooled honestly rather than averaging noisy small-county rates.
bin_lab <- c("Under 5%", "5-15%", "15-30%", "30-50%", "50% or more")
co_bin <- co |>
  mutate(bin = cut(black_share, breaks = c(-Inf, .05, .15, .30, .50, Inf),
                   labels = bin_lab),
         south = if_else(region == "South", "South", "Rest of the country"))

bin_tbl <- co_bin |>
  group_by(bin) |>
  summarise(players = sum(players), pop = sum(tot_pop), n_counties = n(), .groups = "drop") |>
  mutate(per_million = 1e6 * players / pop)
cat("=== (2b) NFL players per million by county Black-population share (national) ===\n")
print(bin_tbl |> mutate(per_million = round(per_million, 1)) |> as.data.frame())

bin_region <- co_bin |>
  group_by(south, bin) |>
  summarise(players = sum(players), pop = sum(tot_pop), n_counties = n(), .groups = "drop") |>
  mutate(per_million = 1e6 * players / pop) |>
  filter(pop > 0)
cat("\n=== (2b) same, split South vs rest of the country ===\n")
print(bin_region |> mutate(per_million = round(per_million, 1)) |> as.data.frame())

p_bins <- ggplot(bin_region, aes(bin, per_million, fill = south)) +
  geom_col(width = 0.7, position = position_dodge(width = 0.75)) +
  geom_text(aes(label = sprintf("%.0f", per_million)),
            position = position_dodge(width = 0.75), vjust = -0.5, size = 3.0,
            fontface = "bold", colour = ink_body) +
  scale_fill_manual(values = c("South" = pal_sport[["NFL"]], "Rest of the country" = "grey65"),
                    name = NULL) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(
    title = "Counties with larger Black populations produce more NFL players per resident, in the South and outside it",
    subtitle = "NFL players per million residents, counties grouped by their Black share of population, split South vs rest",
    x = "County's Black share of population", y = "NFL players per million residents",
    caption = fig_caption(
      "nflverse + ESPN + Sleeper (players); US Census population estimates (cc-est2024)",
      "Counts are pooled across all counties in each group (population-weighted), not an average of county rates.",
      paste0("\nThis describes COUNTIES, not individuals. The player data has no race, so this says nothing about any one person's odds or any\n",
             "group's ability. It shows where NFL production concentrates, which tracks both Southern football culture and where people live.\n",
             sprintf("For context, the South is home to about %.0f%% of the US Black population.", 100 * south_black_share)))) +
  theme_hometown(grid = "y") +
  theme(legend.position = "top", legend.justification = "left",
        axis.text.x = element_text(size = rel(0.8)))
save_fig("docs/figures/ba_where_by_black_share.png", p_bins, w = 12, h = 6.4)

cat("\ndone\n")
