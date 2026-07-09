suppressMessages({ library(dplyr); library(arrow); library(purrr) })
source("R/lib/espn.R")

spine <- read_parquet("data/processed/spine.parquet") |>
  filter(!is.na(espn_id), !is.na(era))

set.seed(42)
sample_ids <- spine |> group_by(era) |> slice_sample(n = 25) |> ungroup()

dir.create("data/raw/espn_sample", showWarnings = FALSE, recursive = TRUE)

fetch_one <- function(espn_id) {
  dest <- file.path("data/raw/espn_sample", paste0(espn_id, ".json"))
  if (!file.exists(dest)) {
    res <- tryCatch(curl::curl_fetch_memory(espn_athlete_url(espn_id)),
                    error = function(e) NULL)
    if (!is.null(res) && res$status_code == 200) writeBin(res$content, dest)
    Sys.sleep(0.2)
  }
  dest
}

paths <- map_chr(sample_ids$espn_id, fetch_one)
parsed <- map_dfr(paths[file.exists(paths)], parse_espn_athlete)

report <- sample_ids |>
  left_join(parsed, by = "espn_id") |>
  mutate(hit = !is.na(birth_city) & !is.na(birth_state)) |>
  group_by(era) |>
  summarise(n = n(), hits = sum(hit, na.rm = TRUE), hit_rate = hits / n)

write.csv(report, "data/processed/espn_sample_report.csv", row.names = FALSE)
print(report)
overall <- sum(report$hits) / sum(report$n)
cat(sprintf("OVERALL HIT RATE: %.1f%% — %s\n", 100 * overall,
            ifelse(overall >= 0.90, "PASS: proceed to full pull",
                   "FAIL: do NOT run Task 4; revisit source choice")))
