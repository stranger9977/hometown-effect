suppressMessages({ library(dplyr); library(arrow) })
source("R/lib/espn.R")

spine <- read_parquet("data/processed/spine.parquet") |> filter(!is.na(espn_id))
dir.create("data/raw/espn", showWarnings = FALSE, recursive = TRUE)

ids <- spine$espn_id
done <- sub("[.]json$", "", list.files("data/raw/espn"))
todo <- setdiff(ids, done)
cat(sprintf("total %d | cached %d | to fetch %d\n",
            length(ids), length(done), length(todo)))

fail_log <- file("data/raw/espn_failures.log", open = "a")
t0 <- Sys.time()
for (i in seq_along(todo)) {
  id <- todo[i]
  res <- tryCatch(curl::curl_fetch_memory(espn_athlete_url(id)),
                  error = function(e) list(status_code = -1L))
  if (res$status_code == 200) {
    writeBin(res$content, file.path("data/raw/espn", paste0(id, ".json")))
  } else {
    writeLines(sprintf("%s\t%s", id, res$status_code), fail_log)
  }
  if (i %% 500 == 0) {
    rate <- i / as.numeric(difftime(Sys.time(), t0, units = "secs"))
    cat(sprintf("%d/%d (%.1f/s, ~%.0f min left)\n",
                i, length(todo), rate, (length(todo) - i) / rate / 60))
  }
  Sys.sleep(0.2)
}
close(fail_log)
cat("done. cached:", length(list.files("data/raw/espn")), "\n")
