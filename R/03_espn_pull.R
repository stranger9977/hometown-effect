suppressMessages({ library(dplyr); library(arrow); library(curl) })
source("R/lib/espn.R")

spine <- read_parquet("data/processed/spine.parquet") |> filter(!is.na(espn_id))
out_dir <- "data/raw/espn"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Guard against truncated/corrupt files left behind by a killed run: any
# cached *.json that is zero bytes or doesn't start with "{" gets removed so
# its id is re-fetched below.
cached_files <- list.files(out_dir, pattern = "[.]json$", full.names = TRUE)
for (f in cached_files) {
  sz <- file.size(f)
  bad <- is.na(sz) || sz == 0
  if (!bad) {
    first_byte <- readBin(f, "raw", n = 1)
    bad <- length(first_byte) == 0 || first_byte != charToRaw("{")
  }
  if (bad) file.remove(f)
}

ids <- spine$espn_id
done <- sub("[.]json$", "", list.files(out_dir))
todo <- setdiff(ids, done)
cat(sprintf("total %d | cached %d | to fetch %d\n",
            length(ids), length(done), length(todo)))

fail_log_path <- "data/raw/espn_failures.log"

# Immediate open-append-close per failure: failures are rare, so this is
# cheap, and it means a killed run never loses a failure record.
log_failure <- function(id, status) {
  con <- file(fail_log_path, open = "a")
  on.exit(close(con))
  writeLines(sprintf("%s\t%s", id, status), con)
}

# Atomic success write: write to a temp file in the same dir, then rename
# (rename is atomic on the same filesystem), so a kill mid-write never leaves
# a truncated *.json behind.
save_success <- function(id, content) {
  final_path <- file.path(out_dir, paste0(id, ".json"))
  tmp_path <- file.path(out_dir, sprintf(".%s.json.tmp", id))
  writeBin(content, tmp_path)
  file.rename(tmp_path, final_path)
}

MAX_CONC   <- 6   # politeness cap -- do not raise
CHUNK_SIZE <- 500 # queue this many requests per multi_run() batch

n_total <- length(todo)
n_done  <- 0
t0      <- Sys.time()

progress <- function() {
  n_done <<- n_done + 1
  if (n_done %% 500 == 0) {
    rate <- n_done / as.numeric(difftime(Sys.time(), t0, units = "secs"))
    cat(sprintf("%d/%d (%.1f/s, ~%.0f min left)\n",
                n_done, n_total, rate, (n_total - n_done) / rate / 60))
  }
}

make_handlers <- function(id) {
  force(id)
  list(
    done = function(res) {
      if (res$status_code == 200) {
        save_success(id, res$content)
      } else {
        log_failure(id, res$status_code)
      }
      progress()
    },
    fail = function(err) {
      log_failure(id, -1L)
      progress()
    }
  )
}

i <- 1L
while (i <= n_total) {
  chunk_ids <- todo[i:min(i + CHUNK_SIZE - 1L, n_total)]
  # multiplex = FALSE: ESPN's API negotiates HTTP/2, and libcurl's default
  # (multiplex = TRUE) would let each of the host_con connections carry
  # multiple concurrent streams -- silently blowing past the 6-request
  # politeness cap. Disabling it keeps concurrency pinned at MAX_CONC.
  pool <- curl::new_pool(total_con = MAX_CONC, host_con = MAX_CONC, multiplex = FALSE)
  for (id in chunk_ids) {
    h <- curl::new_handle(url = espn_athlete_url(id))
    handlers <- make_handlers(id)
    curl::multi_add(h, done = handlers$done, fail = handlers$fail, pool = pool)
  }
  curl::multi_run(pool = pool)
  i <- i + CHUNK_SIZE
}

cat("done. cached:", length(list.files(out_dir)), "\n")
