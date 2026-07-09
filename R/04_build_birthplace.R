suppressMessages({ library(dplyr); library(arrow); library(purrr) })
source("R/lib/espn.R")

files <- list.files("data/raw/espn", full.names = TRUE)
stopifnot(length(files) > 10000)
cat("parsing", length(files), "cached responses...\n")
espn <- map_dfr(files, parse_espn_athlete)

spine <- read_parquet("data/processed/spine.parquet")

joined <- spine |>
  filter(!is.na(espn_id)) |>
  left_join(espn, by = "espn_id") |>
  mutate(dob_match = if_else(
    is.na(birth_date) | is.na(espn_dob), NA,
    as.character(birth_date) == espn_dob))

cat("DOB mismatches (crosswalk errors, dropped):",
    sum(joined$dob_match == FALSE, na.rm = TRUE), "\n")

birthplace <- joined |>
  filter(parse_ok, !is.na(birth_city), !is.na(birth_state),
         is.na(dob_match) | dob_match)

write_parquet(birthplace, "data/processed/birthplace.parquet")

stats <- joined |>
  filter(!is.na(era)) |>
  group_by(era) |>
  summarise(with_espn_id = n(),
            with_birthplace = sum(parse_ok & !is.na(birth_city) &
                                  !is.na(birth_state), na.rm = TRUE),
            rate = with_birthplace / with_espn_id)
print(stats)
cat("total birthplace rows:", nrow(birthplace), "\n")
