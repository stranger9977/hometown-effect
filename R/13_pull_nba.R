suppressMessages({ library(dplyr); library(arrow); library(rvest); library(xml2); library(curl) })
source("R/lib/bins.R")

out_dir <- "data/raw/bbref"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

states <- c(datasets::state.abb, "DC")
stopifnot(length(states) == 51)

UA <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
SLEEP_SECS <- 3.2  # <= 20 req/min politeness cap -- do not lower

bbref_url <- function(state) {
  sprintf("https://www.basketball-reference.com/friv/birthplaces.fcgi?country=US&state=%s", state)
}
wayback_url <- function(state) sprintf("http://web.archive.org/web/2026/%s", bbref_url(state))

# A response is only accepted if it's a genuine basketball-reference content
# page -- guards against Cloudflare block pages (which can come back as HTTP
# 200) and against Wayback serving back an archived 403/error capture. Some
# low-population states (verified: Vermont has 0 NBA/ABA-born players) render
# a real page with no results table at all, so "has a stats table" alone is
# too strict -- the locations-list summary block is present on every genuine
# page load regardless of whether that state's table is empty.
looks_valid <- function(status, body) {
  status == 200L &&
    (grepl('id="stats"', body, fixed = TRUE) ||
       grepl("Show/Hide Locations List", body, fixed = TRUE))
}

fetch_one <- function(url) {
  h <- curl::new_handle(useragent = UA, followlocation = TRUE, timeout = 30)
  res <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) NULL)
  if (is.null(res)) return(list(status = -1L, body = ""))
  list(status = res$status_code, body = rawToChar(res$content))
}

cache_path <- function(state) file.path(out_dir, paste0(state, ".html"))

is_cached_valid <- function(state) {
  p <- cache_path(state)
  if (!file.exists(p)) return(FALSE)
  sz <- file.size(p)
  if (is.na(sz) || sz == 0) return(FALSE)
  body <- readChar(p, sz, useBytes = TRUE)
  looks_valid(200L, body)
}

save_cache <- function(state, body) {
  final_path <- cache_path(state)
  tmp_path <- file.path(out_dir, sprintf(".%s.html.tmp", state))
  writeLines(body, tmp_path, useBytes = TRUE)
  file.rename(tmp_path, final_path)
}

method_used <- setNames(rep(NA_character_, length(states)), states)  # "cache" | "direct" | "wayback" | "failed"

for (state in states) {
  if (is_cached_valid(state)) {
    method_used[state] <- "cache"
    next
  }

  direct <- fetch_one(bbref_url(state))
  Sys.sleep(SLEEP_SECS)
  if (looks_valid(direct$status, direct$body)) {
    save_cache(state, direct$body)
    method_used[state] <- "direct"
    next
  }

  cat(sprintf("basketball-reference blocked state=%s (status %s) -- trying Wayback\n",
              state, direct$status))
  wb <- fetch_one(wayback_url(state))
  Sys.sleep(SLEEP_SECS)
  if (looks_valid(wb$status, wb$body)) {
    save_cache(state, wb$body)
    method_used[state] <- "wayback"
    next
  }

  cat(sprintf("Wayback also failed for state=%s (status %s)\n", state, wb$status))
  method_used[state] <- "failed"
}

cat("=== NBA fetch summary ===\n")
print(table(method_used, useNA = "ifany"))

ok_states <- states[method_used %in% c("cache", "direct", "wayback")]

if (length(ok_states) == 0) {
  nba <- tibble::tibble(
    sport = character(), player_name = character(), birth_city = character(),
    birth_state = character(), birth_country = character(),
    birth_date = character(), era = character()
  )
  dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
  write_parquet(nba, "data/processed/players_nba.parquet")
  cat("NBA BLOCKED\n")
  cat("basketball-reference.com was unreachable both directly and via Wayback ",
      "for all 51 state pages. Wrote players_nba.parquet with 0 rows.\n", sep = "")
  quit(save = "no", status = 0)
}

# Parse one cached state page: player name (strip trailing "*" HOF marker),
# From (rookie season -> era), birth city. birth_state is the state we
# queried (bbref filters the page to it); birth_country is always USA
# (the friv/birthplaces.fcgi?country=US endpoint). No birth_date on this
# page format's exposed fields we're using -- NBA RAE is skipped downstream.
parse_state <- function(state) {
  page <- read_html(cache_path(state))
  rows <- html_elements(page, "table#stats tbody tr:not(.thead)")
  if (length(rows) == 0) {
    return(tibble::tibble(player_name = character(), birth_city = character(), from_year = integer()))
  }
  player <- html_element(rows, "td[data-stat='player']") |> html_text2()
  from   <- html_element(rows, "td[data-stat='season_min']") |> html_text2()
  city   <- html_element(rows, "td[data-stat='birth_city']") |> html_text2()
  tibble::tibble(
    player_name = sub("[*]$", "", player),
    birth_city  = city,
    from_year   = suppressWarnings(as.integer(from))
  )
}

state_tables <- list()
n_printed <- 0
for (state in ok_states) {
  st_df <- parse_state(state)
  st_df$birth_state <- state
  state_tables[[state]] <- st_df

  if (n_printed < 2 && nrow(st_df) > 0) {
    cat(sprintf("--- sample parsed row, state=%s ---\n", state))
    print(head(st_df, 1))
    n_printed <- n_printed + 1
  }
}

nba <- bind_rows(state_tables) |>
  transmute(
    sport         = "NBA",
    player_name,
    birth_city,
    birth_state,
    birth_country = "USA",
    birth_date    = NA_character_,
    era           = era_cohort(from_year)
  )

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
write_parquet(nba, "data/processed/players_nba.parquet")

cat("=== NBA ===\n")
cat("total rows:", nrow(nba), "\n")
cat("US-born rows:", sum(nba$birth_country == "USA", na.rm = TRUE), "\n")
cat("era distribution:\n")
print(count(nba, era))
cat("birth_date coverage:", sprintf("%.1f%%", 100 * mean(!is.na(nba$birth_date))), "\n")
n_failed <- sum(method_used == "failed", na.rm = TRUE)
if (n_failed > 0) {
  cat("states with no usable page (excluded):", paste(states[method_used == "failed"], collapse = ", "), "\n")
}
