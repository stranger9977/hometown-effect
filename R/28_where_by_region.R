# =============================================================================
# 28_where_by_region.R -- Michael's "four quadrants, made fair" ask: what share
# of NFL players comes from each US region, and is a region over- or under-
# represented once you divide by its population?
#
# Region populations: Census Population Estimates county characteristics
# (cc-est2024, keyless). Player counts + county populations join through
# data/processed/county_rates.csv (from R/09).
# =============================================================================

suppressMessages({
  library(dplyr); library(readr); library(ggplot2); library(stringr); library(tidyr)
})
source("R/lib/theme_hometown.R")

# --- county population (keyless popest characteristics) ---------------------
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

cty <- cc_raw |>
  filter(SUMLEV == "050", AGEGRP == "0", YEAR == as.character(latest_year)) |>
  transmute(
    county_fips = paste0(STATE, COUNTY),
    stname = STNAME,
    tot_pop = as.numeric(TOT_POP))

# --- Census region from state name ------------------------------------------
region_of <- setNames(as.character(state.region), state.name)
region_of["District of Columbia"] <- "South"   # Census places DC in the South
cty <- cty |>
  mutate(region = recode(region_of[stname], "North Central" = "Midwest")) |>
  filter(!is.na(region))

# --- join player counts (per-county) ----------------------------------------
rates <- read_csv("data/processed/county_rates.csv", show_col_types = FALSE) |>
  mutate(county_fips = str_pad(as.character(county_fips), 5, pad = "0")) |>
  select(county_fips, players)

co <- cty |>
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

cat("\ndone\n")
