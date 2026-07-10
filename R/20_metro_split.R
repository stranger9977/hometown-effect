# =============================================================================
# 20_metro_split.R -- regional decomposition of the NFL suburban-ring surplus.
#
# Question: is the 2020s surplus of NFL players from the 15-45mi suburban ring
# a Sun Belt phenomenon, or do Northeast/Midwest/West suburbs share in it?
#
# Method: take NFL players in the 15-45mi band of the nearest 1M+ metro
# (data/processed/metro_distance.parquet) and split them by the REGION of that
# nearest metro. Baseline is every census place in the 15-45mi band of the
# same 1M+ metro anchors, weighted by current population (same anchors and
# distance logic as output/exploration/metro_cbsa_02_analysis.R).
#
# Representation ratio for a (region, era) cell:
#   (ring players near region-R metros / all NFL players in era) /
#   (ring population near region-R metros / total US place population)
# so cells decompose the overall 15-45mi band ratio and read as
# over/underproduction vs proportional.
#
# Inputs:  data/processed/metro_distance.parquet
#          data/processed/census_places.parquet
#          output/exploration/cbsa_build.rds  (1M+ CBSA anchors)
# Outputs: data/processed/suburb_regions.csv  (level = "region" rows +
#          level = "metro_2020s" rows for the per-metro leaderboard)
#          docs/figures/suburb_regions.png    (region slope chart)
# =============================================================================

suppressMessages({
  library(arrow)
  library(dplyr)
  library(tidyr)
  library(stringr)
})
setwd("/Users/nick/hometown-effect")
source("R/lib/theme_hometown.R")

# ---- 1. Metro anchors and region assignment ---------------------------------
bld <- readRDS("output/exploration/cbsa_build.rds")
anch1m <- bld$big1m_anchors

# Region of a metro = census-style region of its primary state (the first
# state in the CBSA title suffix, which belongs to the largest principal
# city). Sun Belt per task spec; VA/KY metros do not fit any of the four
# named regions and are kept as "Other" so the cells still sum to the band.
st_sunbelt   <- c("TX","OK","AR","LA","MS","AL","GA","FL","SC","NC","TN","AZ","NV","NM")
st_northeast <- c("ME","NH","VT","MA","RI","CT","NY","NJ","PA","MD","DE","DC")
st_midwest   <- c("OH","IN","IL","MI","WI","MN","IA","MO","ND","SD","NE","KS")
st_west      <- c("WA","OR","CA","AK","HI","CO","UT","ID","MT","WY")

metro_region <- function(cbsa_title) {
  primary_state <- str_extract(cbsa_title, "(?<=, )[A-Z]{2}")
  case_when(
    primary_state %in% st_sunbelt   ~ "Sun Belt",
    primary_state %in% st_northeast ~ "Northeast",
    primary_state %in% st_midwest   ~ "Midwest",
    primary_state %in% st_west      ~ "West/Pacific",
    TRUE                            ~ "Other"
  )
}
anch1m <- anch1m |> mutate(region = metro_region(cbsa_title))
cat("== 1M+ metro anchors by region ==\n")
print(anch1m |> count(region) |> as.data.frame())
cat("Other-region metros:",
    paste(anch1m$cbsa_title[anch1m$region == "Other"], collapse = "; "), "\n\n")

# ---- 2. Baseline: census places annotated with nearest 1M+ anchor -----------
R_mi <- 3958.8
nearest_anchor <- function(lat, lon, anchors) {
  la1 <- lat * pi / 180; lo1 <- lon * pi / 180
  dmin <- rep(Inf, length(lat)); imin <- rep(NA_integer_, length(lat))
  for (j in seq_len(nrow(anchors))) {
    la2 <- anchors$a_lat[j] * pi / 180; lo2 <- anchors$a_lon[j] * pi / 180
    a <- sin((la2 - la1) / 2)^2 + cos(la1) * cos(la2) * sin((lo2 - lo1) / 2)^2
    d <- 2 * R_mi * asin(pmin(1, sqrt(a)))
    upd <- d < dmin
    imin[upd] <- j; dmin[upd] <- d[upd]
  }
  list(dist = dmin, idx = imin)
}

cp <- read_parquet("data/processed/census_places.parquet") |>
  filter(state != "PR", !is.na(lat), !is.na(lon), !is.na(pop_now), pop_now > 0)
nn <- nearest_anchor(cp$lat, cp$lon, anch1m)
cp <- cp |>
  mutate(dist_mi = nn$dist,
         nearest_metro = anch1m$cbsa_title[nn$idx],
         region = anch1m$region[nn$idx],
         in_ring = dist_mi >= 15 & dist_mi < 45)

pop_total <- sum(cp$pop_now)
base_region <- cp |>
  filter(in_ring) |>
  group_by(region) |>
  summarise(base_pop = sum(pop_now), n_places = n(), .groups = "drop") |>
  mutate(base_share = base_pop / pop_total)
base_metro <- cp |>
  filter(in_ring) |>
  group_by(nearest_metro, region) |>
  summarise(base_pop = sum(pop_now), .groups = "drop")

cat(sprintf("baseline: %s places, %s people total; %s in the 15-45mi ring (%.1f%%)\n\n",
            format(nrow(cp), big.mark = ","), format(pop_total, big.mark = ","),
            format(sum(cp$pop_now[cp$in_ring]), big.mark = ","),
            100 * sum(cp$pop_now[cp$in_ring]) / pop_total))

# ---- 3. NFL players: 15-45mi band by region of nearest metro -----------------
eras <- c("1990s", "2000s", "2010s", "2020s")
md_all <- read_parquet("data/processed/metro_distance.parquet") |> filter(sport == "NFL")
md <- md_all |> filter(era %in% eras)
cat(sprintf("NFL players: %d total, %d with a decade era (dropped %d with era NA)\n",
            nrow(md_all), nrow(md), nrow(md_all) - nrow(md)))
era_totals <- md |> count(era, name = "n_era_total")

ring <- md |>
  filter(band == "15-45mi") |>
  mutate(region = metro_region(nearest_metro))

region_tab <- ring |>
  count(era, region, name = "n_players") |>
  left_join(era_totals, by = "era") |>
  left_join(base_region |> select(region, base_pop, base_share), by = "region") |>
  mutate(player_share = n_players / n_era_total,
         rep_ratio = player_share / base_share,
         small_cell = n_players < 30) |>
  group_by(era) |>
  mutate(share_of_ring_players = n_players / sum(n_players)) |>
  ungroup() |>
  mutate(share_of_ring_pop = base_pop / sum(base_region$base_pop)) |>
  arrange(era, desc(rep_ratio))

cat("== NFL 15-45mi ring: representation ratio by metro region by era ==\n")
cat("(rep_ratio = share of all NFL players in era from region-R ring, over\n")
cat(" share of US place population living in region-R ring; 1 = proportional)\n\n")
print(region_tab |>
        mutate(across(c(player_share, base_share, share_of_ring_players,
                        share_of_ring_pop), ~round(.x, 4)),
               rep_ratio = round(rep_ratio, 2)) |>
        as.data.frame())

# consistency check: region cells should sum to the overall band ratio
chk <- region_tab |>
  group_by(era) |>
  summarise(band_ratio = sum(player_share) / sum(base_share), .groups = "drop")
cat("\ncheck (overall 15-45mi band ratio rebuilt from region cells):\n")
print(chk |> mutate(band_ratio = round(band_ratio, 2)) |> as.data.frame())

# ---- 4. Top metros by 2020s ring per-capita production (min 8 players) ------
metro_2020 <- ring |>
  filter(era == "2020s") |>
  count(nearest_metro, region, name = "n_players") |>
  inner_join(base_metro |> select(nearest_metro, base_pop), by = "nearest_metro") |>
  mutate(players_per_m = n_players / (base_pop / 1e6)) |>
  filter(n_players >= 8) |>
  arrange(desc(players_per_m))

cat("\n== Top 10 metros, 2020s 15-45mi ring players per 1M ring residents",
    "(min 8 players) ==\n")
print(metro_2020 |> head(10) |>
        mutate(players_per_m = round(players_per_m, 1)) |> as.data.frame())

# ---- 5. Write table ----------------------------------------------------------
out <- bind_rows(
  region_tab |>
    transmute(level = "region", era, group = region, n_players, n_era_total,
              player_share, base_pop, base_share, rep_ratio,
              share_of_ring_players, share_of_ring_pop, small_cell),
  metro_2020 |>
    transmute(level = "metro_2020s", era = "2020s", group = nearest_metro,
              n_players, n_era_total = NA_integer_, player_share = NA_real_,
              base_pop, base_share = NA_real_, rep_ratio = players_per_m,
              share_of_ring_players = NA_real_, share_of_ring_pop = NA_real_,
              small_cell = n_players < 30)
)
write.csv(out, "data/processed/suburb_regions.csv", row.names = FALSE)
cat("\nwrote data/processed/suburb_regions.csv (", nrow(out), "rows )\n")
cat("note: for level = 'metro_2020s' rows, rep_ratio holds players per 1M",
    "ring residents,\nnot a share ratio.\n")

# ---- 6. Figure: region slope chart, 1990s -> 2020s ---------------------------
slope <- region_tab |>
  filter(era %in% c("1990s", "2020s"), region != "Other") |>
  mutate(era_n = ifelse(era == "1990s", 1, 2))
ends <- slope |> filter(era == "2020s")
# West/Pacific and Midwest both start at ~0.50; dodge their labels apart
starts <- slope |>
  filter(era == "1990s") |>
  mutate(y_lab = rep_ratio + case_when(region == "West/Pacific" ~ 0.035,
                                       region == "Midwest"      ~ -0.035,
                                       TRUE                     ~ 0))

pal_region <- c("Sun Belt" = "#D55E00", "Northeast" = "#0072B2",
                "Midwest" = "#009E73", "West/Pacific" = "#CC79A7")

p <- ggplot(slope, aes(era_n, rep_ratio, colour = region, group = region)) +
  geom_baseline(1) +
  geom_line(linewidth = 1.6) +
  geom_point(size = 3.2) +
  direct_label(ends, aes(label = sprintf("%s %.2fx", region, rep_ratio)),
               nudge_x = 0.05, size = 4.2) +
  geom_text(data = starts, aes(y = y_lab, label = sprintf("%.2f", rep_ratio)),
            hjust = 1, nudge_x = -0.05, size = 3.6, fontface = "bold",
            show.legend = FALSE) +
  scale_colour_manual(values = pal_region) +
  scale_x_continuous(breaks = 1:2, labels = c("1990s", "2020s"),
                     limits = c(0.9, 2.55)) +
  coord_cartesian(clip = "off") +
  labs(title = "The suburban NFL boom is a Sun Belt story; Northeast suburbs still underproduce",
       subtitle = "NFL representation ratio of the 15-45mi suburban ring, split by the region of the nearest 1M+ metro, 1990s vs 2020s rookie cohorts",
       x = NULL, y = "Representation ratio (1 = proportional)",
       caption = paste0(
         "Data: nflverse+ESPN, Census CBSA delineation + places. US-born players; hometown = high school where known, else birthplace.\n",
         "Cell ratio = share of all NFL players in the era who grew up 15-45mi from a 1M+ metro anchor in the region, divided by the share\n",
         "of US place population living in that ring. Sun Belt = metros in TX OK AR LA MS AL GA FL SC NC TN AZ NV NM; Richmond,\n",
         "Virginia Beach and Louisville (not in any of the four regions) are excluded from the chart but kept in the table.")) +
  theme_hometown()
save_fig("docs/figures/suburb_regions.png", p)
