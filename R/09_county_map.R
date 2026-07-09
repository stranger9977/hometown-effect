suppressMessages({ library(dplyr); library(arrow); library(sf); library(usmap); library(ggplot2) })

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
  scale_fill_viridis_c(option = "magma", direction = -1, trans = "sqrt",
                       na.value = "grey92",
                       name = "NFL players born\nper 1M residents") +
  labs(title = "Where NFL players are from, per capita",
       subtitle = "Players with rookie seasons 1990–2025, by hometown county (high school where known, else birthplace)",
       caption = "Data: nflverse + ESPN + Sleeper + NCES + US Census. Grey: no matched players.") +
  theme(plot.title = element_text(size = 20, face = "bold"),
        plot.subtitle = element_text(size = 14),
        legend.position = "right",
        plot.background = element_rect(fill = "white", colour = NA))
ggsave("docs/figures/county_map.png", p, width = 12, height = 8, dpi = 320, bg = "white")
cat("top 10 counties by per-capita production (min 5 players):\n")
rates |> filter(players >= 5) |> arrange(desc(per_million)) |> head(10) |> print()
