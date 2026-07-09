suppressMessages({ library(dplyr); library(arrow) })
source("R/lib/places.R")

birthplace <- read_parquet("data/processed/birthplace.parquet") |>
  filter(is.na(birth_country) | birth_country == "USA",
         birth_state %in% c(state.abb, "DC"))
places <- read_parquet("data/processed/census_places.parquet")

matched <- match_places(birthplace, places)
write_parquet(matched, "data/processed/birthplace_matched.parquet")

report <- matched |>
  filter(!is.na(era)) |>
  group_by(era) |>
  summarise(n = n(),
            matched = sum(match_tier != "unmatched"),
            match_rate = matched / n,
            no_pop_share = mean(match_tier != "unmatched" &
                                is.na(matched_pop_now)))
write.csv(report, "data/processed/match_report.csv", row.names = FALSE)
print(report)

cat("\ntop unmatched cities (data quality review):\n")
matched |>
  filter(match_tier == "unmatched") |>
  count(birth_city, birth_state, sort = TRUE) |>
  head(20) |>
  as.data.frame() |>
  print()
