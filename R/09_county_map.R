suppressMessages({ library(dplyr); library(arrow); library(sf); library(usmap); library(ggplot2) })
source("R/lib/theme_hometown.R")

matched <- read_parquet("data/processed/hometown.parquet") |>
  filter(!is.na(hometown_source), !is.na(hometown_lat), !is.na(hometown_lon)) |>
  rename(lat = hometown_lat, lon = hometown_lon)

counties_sf <- st_read(list.files("data/raw/census/cb_2023_us_county_500k",
                                  pattern = "[.]shp$", full.names = TRUE),
                       quiet = TRUE)

pts <- st_as_sf(matched, coords = c("lon", "lat"), crs = st_crs(counties_sf))
hit <- st_join(pts, counties_sf["GEOID"], join = st_within)

county_counts <- hit |>
  st_drop_geometry() |>
  filter(!is.na(GEOID)) |>
  count(county_fips = GEOID, name = "players")

co_pop <- read_parquet("data/processed/census_counties.parquet")

rates <- co_pop |>
  left_join(county_counts, by = "county_fips") |>
  mutate(players = coalesce(players, 0L),
         per_million = 1e6 * players / pop2024)
write.csv(rates, "data/processed/county_rates.csv", row.names = FALSE)

p <- plot_usmap(regions = "counties",
                data = rates |> select(fips = county_fips, per_million),
                values = "per_million", linewidth = 0) +
  scale_fill_viridis_c(option = "magma", direction = -1, transform = "sqrt",
                       na.value = "grey92",
                       name = "NFL players\nper 1M residents",
                       guide = guide_colourbar(barwidth = unit(0.45, "lines"),
                                               barheight = unit(7, "lines"))) +
  labs(title = "The Deep South produces NFL players at the highest per-capita rates",
       subtitle = "NFL players per 1 million residents by hometown county (high school where known, else birthplace), rookie seasons 1990-2025",
       caption = fig_caption("nflverse + ESPN + Sleeper + NCES + US Census",
                             "US counties, players with rookie seasons 1990-2025.",
                             "Grey: no matched players.")) +
  theme_hometown_legend(grid = "none", position = "inside") +
  theme(legend.position.inside = c(0.98, 0.06),
        legend.justification.inside = c(1, 0),
        panel.grid = element_blank(),
        axis.text  = element_blank(),
        axis.title = element_blank())
save_fig("docs/figures/county_map.png", p, w = 12, h = 8)
cat("top 10 counties by per-capita production (min 5 players):\n")
rates |> filter(players >= 5) |> arrange(desc(per_million)) |> head(10) |> print()
