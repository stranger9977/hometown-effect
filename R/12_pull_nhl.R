suppressMessages({ library(dplyr); library(arrow); library(jsonlite); library(curl) })
source("R/lib/bins.R")

raw <- "data/raw/nhl_all.json"
if (!file.exists(raw)) {
  cat("data/raw/nhl_all.json missing -- refetching bulk player search...\n")
  dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)
  ok <- tryCatch({
    curl::curl_download(
      "https://search.d3.nhle.com/api/v1/search/player?culture=en-us&limit=100000&q=%2A",
      raw)
    TRUE
  }, error = function(e) FALSE)
  if (!ok || !file.exists(raw)) {
    stop("Could not download the NHL bulk player search. Place the response ",
         "at ", raw, " and re-run.")
  }
}

bulk <- fromJSON(raw, simplifyVector = TRUE)
stopifnot(nrow(bulk) > 20000)

# Players who actually played (bulk search includes prospects/never-played
# entries with a null lastSeasonId).
played <- bulk |> filter(!is.na(lastSeasonId))

# Only fetch individual landing pages for players active in/after the
# 1989-90 season -- the oldest cohort era_cohort() can bin. Older players
# get era = NA and birth_date = NA without a network round trip.
RECENT_CUTOFF <- 19891990L
played <- played |> mutate(last_season_int = as.integer(lastSeasonId))
recent_ids <- played |> filter(last_season_int >= RECENT_CUTOFF) |> pull(playerId)

out_dir <- "data/raw/nhl_landing"
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

done <- sub("[.]json$", "", list.files(out_dir))
todo <- setdiff(recent_ids, done)
cat(sprintf("NHL landing: recent %d | cached %d | to fetch %d\n",
            length(recent_ids), length(done), length(todo)))

fail_log_path <- "data/raw/nhl_landing_failures.log"

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

landing_url <- function(id) sprintf("https://api-web.nhle.com/v1/player/%s/landing", id)

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

if (n_total > 0) {
  i <- 1L
  while (i <= n_total) {
    chunk_ids <- todo[i:min(i + CHUNK_SIZE - 1L, n_total)]
    # multiplex = FALSE keeps concurrency pinned at MAX_CONC (see R/03_espn_pull.R).
    pool <- curl::new_pool(total_con = MAX_CONC, host_con = MAX_CONC, multiplex = FALSE)
    for (id in chunk_ids) {
      h <- curl::new_handle(url = landing_url(id))
      handlers <- make_handlers(id)
      curl::multi_add(h, done = handlers$done, fail = handlers$fail, pool = pool)
    }
    curl::multi_run(pool = pool)
    i <- i + CHUNK_SIZE
  }
}

n_cached <- length(list.files(out_dir, pattern = "[.]json$"))
cat("NHL landing fetch done. cached:", n_cached, "\n")

# --- Parse cached landing JSON for birth_date + first NHL regular-season year ---
#
# Verified on player 8478402 (Connor McDavid) before writing this: his
# seasonTotals include youth/junior leagues (GTHL, OHL, WJC, etc.) that are
# ALSO tagged gameTypeId == 2 ("regular season" within their own league), so
# gameTypeId == 2 alone is not sufficient to isolate NHL seasons. Filtering
# additionally on leagueAbbrev == "NHL" gives the correct first NHL season
# (2015 for McDavid, matching his actual rookie year).
parse_landing <- function(id) {
  path <- file.path(out_dir, paste0(id, ".json"))
  if (!file.exists(path)) return(list(birth_date = NA_character_, first_nhl_year = NA_integer_))
  parsed <- tryCatch(fromJSON(path, simplifyVector = TRUE), error = function(e) NULL)
  if (is.null(parsed)) return(list(birth_date = NA_character_, first_nhl_year = NA_integer_))

  bdate <- parsed$birthDate
  if (is.null(bdate) || length(bdate) == 0) bdate <- NA_character_

  st <- parsed$seasonTotals
  first_year <- NA_integer_
  if (is.data.frame(st) && all(c("gameTypeId", "leagueAbbrev", "season") %in% names(st))) {
    nhl_seasons <- st$season[st$gameTypeId == 2 & st$leagueAbbrev == "NHL"]
    if (length(nhl_seasons) > 0) {
      first_year <- as.integer(substr(as.character(min(nhl_seasons, na.rm = TRUE)), 1, 4))
    }
  }
  list(birth_date = bdate, first_nhl_year = first_year)
}

landing <- lapply(recent_ids, parse_landing)
landing_df <- tibble::tibble(
  playerId       = recent_ids,
  birth_date     = vapply(landing, function(x) x$birth_date, character(1)),
  first_nhl_year = vapply(landing, function(x) x$first_nhl_year, integer(1))
)

nhl <- played |>
  left_join(landing_df, by = "playerId") |>
  transmute(
    sport         = "NHL",
    player_name   = name,
    birth_city    = birthCity,
    birth_state   = birthStateProvince,
    birth_country = birthCountry,
    birth_date    = birth_date,
    era           = era_cohort(first_nhl_year)
  )

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
write_parquet(nhl, "data/processed/players_nhl.parquet")

cat("=== NHL ===\n")
cat("total rows:", nrow(nhl), "\n")
cat("US-born rows:", sum(nhl$birth_country == "USA", na.rm = TRUE), "\n")
cat("era distribution:\n")
print(count(nhl, era))
cat("birth_date coverage:", sprintf("%.1f%%", 100 * mean(!is.na(nhl$birth_date))), "\n")
n_failures <- if (file.exists(fail_log_path)) length(readLines(fail_log_path)) else 0
cat("landing fetch failures logged:", n_failures, "\n")
