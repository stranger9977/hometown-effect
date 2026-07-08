# hometown-effect POC Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** NFL proof-of-concept of the hometown-effect study: nflverse spine + ESPN birthplace pull + keyless Census join → RAE chart, small-town-effect replication by era, county per-capita US heatmap, published as a GitHub Pages report.

**Architecture:** Numbered R pipeline scripts (`R/01_*.R` …) that read/write files under `data/`; pure logic lives in `R/lib/*.R` with testthat tests; figures export to `docs/figures/`; a hand-built `docs/index.html` is served by GitHub Pages from `/docs` on `main`.

**Tech Stack:** R 4.5.2 — tidyverse, arrow, jsonlite, curl, sf, usmap (all verified installed except testthat, installed in Task 1). No Python needed for the POC.

## Global Constraints

- All scripts are run from the repo root: `Rscript R/NN_name.R`. Relative paths only.
- Spec: `docs/specs/2026-07-08-hometown-effect-design.md`. Re-read the relevant spec section before implementing a task.
- Census API key is in `~/.Renviron` as `CENSUS_API_KEY` (R auto-loads it; access with `Sys.getenv("CENSUS_API_KEY")`). **Never print it, never commit it, never write it into any file in the repo.** Fail fast with a clear error if it's empty. Cache every API response to `data/raw/census/api/` (gitignored) so reruns are free.
- ESPN API: max 5 requests/second (`Sys.sleep(0.2)` per request), on-disk cache in `data/raw/espn/`, every fetch loop must be resumable (skip files that already exist and parse as JSON).
- `data/raw/` and `data/processed/` are gitignored — never commit data files; commit only code, tests, figures, and HTML.
- Figures: ggplot2, PNG via `ggsave(..., width = 12, height = 6.75, dpi = 320)` (≈4K, video-friendly), colorblind-safe colors, no default gray theme — use `theme_minimal(base_size = 16)` plus explicit styling.
- Era cohorts everywhere: `rookie_season` 1990–1999 = "1990s", 2000–2009 = "2000s", 2010–2019 = "2010s", 2020+ = "2020s". Players with `rookie_season < 1990` are excluded from birthplace analyses (ESPN join coverage collapses pre-1990) but included in RAE.
- Every analysis output that depends on a join must print/save its match rate. No silent drops.
- Commit after each task with a conventional message; never push data.

## File Structure

```
R/lib/espn.R            ESPN URL builder + athlete JSON parser (pure)
R/lib/places.R          city-name normalizer, LSAD stripper, matcher (pure)
R/lib/bins.R            Côté-style bins + era cohort assignment (pure)
R/01_build_spine.R      players.parquet → data/processed/spine.parquet
R/02_espn_sample.R      100-id hit-rate gate → data/processed/espn_sample_report.csv
R/03_espn_pull.R        full cached pull → data/raw/espn/{id}.json
R/04_build_birthplace.R parse cache + DOB-validated join → data/processed/birthplace.parquet
R/05_pull_census.R      keyless census files → data/raw/census/, data/processed/census_*.parquet
R/06_match_places.R     birthplace → place GEOID/pop/density → data/processed/birthplace_matched.parquet
R/07_rae.R              RAE figure → docs/figures/rae_nfl.png
R/08_birthplace_effect.R  bins + density figures → docs/figures/{cote_bins,density_gradient}.png
R/09_county_map.R       county per-capita map → docs/figures/county_map.png
docs/index.html         the shareable report page
tests/testthat/         testthat tests for R/lib/*
tests/fixtures/         small JSON fixtures (committed)
```

---

### Task 1: Test harness + player spine

**Files:**
- Create: `R/lib/bins.R`, `R/01_build_spine.R`, `tests/testthat/test-bins.R`, `tests/run_tests.R`

**Interfaces:**
- Produces: `data/processed/spine.parquet` with columns `gsis_id, display_name, birth_date (Date), rookie_season (int), draft_year, position, college_name, espn_id (chr), pfr_id, sleeper_id (chr), era (chr or NA)`.
- Produces: `era_cohort(rookie_season)` → chr ("1990s"/"2000s"/"2010s"/"2020s"/NA) in `R/lib/bins.R`.

- [ ] **Step 1: Install testthat if missing**

Run: `Rscript -e 'if (!requireNamespace("testthat", quietly=TRUE)) install.packages("testthat", repos="https://cloud.r-project.org"); cat(as.character(packageVersion("testthat")), "\n")`
Expected: prints a version like `3.2.x`

- [ ] **Step 2: Write the failing test**

`tests/testthat/test-bins.R`:
```r
source(file.path(testthat::test_path(), "..", "..", "R", "lib", "bins.R"))

test_that("era_cohort maps rookie seasons to cohorts", {
  expect_equal(era_cohort(c(1990, 1999, 2000, 2013, 2020, 2025)),
               c("1990s", "1990s", "2000s", "2010s", "2020s", "2020s"))
  expect_true(is.na(era_cohort(1989)))
  expect_true(is.na(era_cohort(NA)))
})
```

`tests/run_tests.R`:
```r
testthat::test_dir("tests/testthat", stop_on_failure = TRUE)
```

- [ ] **Step 3: Run test to verify it fails**

Run: `Rscript tests/run_tests.R`
Expected: FAIL — `era_cohort` not found

- [ ] **Step 4: Implement `R/lib/bins.R`**

```r
era_cohort <- function(rookie_season) {
  dplyr::case_when(
    is.na(rookie_season)      ~ NA_character_,
    rookie_season < 1990      ~ NA_character_,
    rookie_season < 2000      ~ "1990s",
    rookie_season < 2010      ~ "2000s",
    rookie_season < 2020      ~ "2010s",
    TRUE                      ~ "2020s"
  )
}
```

- [ ] **Step 5: Run tests to verify pass**

Run: `Rscript tests/run_tests.R`
Expected: PASS

- [ ] **Step 6: Write `R/01_build_spine.R`**

```r
suppressMessages({ library(dplyr); library(arrow) })
source("R/lib/bins.R")

raw <- "data/raw/players.parquet"
if (!file.exists(raw)) {
  download.file(
    "https://github.com/nflverse/nflverse-data/releases/download/players/players.parquet",
    raw, mode = "wb")
}

players <- read_parquet(raw)
stopifnot(nrow(players) > 24000)

spine <- players |>
  transmute(
    gsis_id, display_name,
    birth_date = as.Date(birth_date),
    rookie_season = as.integer(rookie_season),
    draft_year, position, college_name,
    espn_id = as.character(espn_id),
    pfr_id,
    sleeper_id = if ("sleeper_id" %in% names(players)) as.character(sleeper_id) else NA_character_,
    era = era_cohort(as.integer(rookie_season))
  )

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
write_parquet(spine, "data/processed/spine.parquet")

cat("rows:", nrow(spine), "\n")
cat("with espn_id:", sum(!is.na(spine$espn_id)), "\n")
print(count(spine, era))
```

Note: the nflverse players table may not carry `sleeper_id` (scouting saw it in
`load_players()`/`load_rosters()`); the guard above keeps the column present either
way. If it lands as all-NA, Phase 2 (high school) will pull the crosswalk from
`nflreadr::load_ff_playerids()` instead — out of scope here.

- [ ] **Step 7: Run and sanity-check**

Run: `Rscript R/01_build_spine.R`
Expected: `rows: 25033`, `with espn_id: 16765` (±small drift if nflverse re-released), era counts printed with NA for pre-1990.

- [ ] **Step 8: Commit**

```bash
git add R/lib/bins.R R/01_build_spine.R tests/
git commit -m "feat: test harness, era cohorts, player spine"
```

---

### Task 2: ESPN athlete parser (TDD)

**Files:**
- Create: `R/lib/espn.R`, `tests/testthat/test-espn.R`, `tests/fixtures/espn_mahomes.json`, `tests/fixtures/espn_no_birthplace.json`

**Interfaces:**
- Produces: `espn_athlete_url(espn_id)` → chr URL.
- Produces: `parse_espn_athlete(path)` → one-row tibble `espn_id (chr), full_name, birth_city, birth_state, birth_country, espn_dob (chr "YYYY-MM-DD" or NA), parse_ok (lgl)`. Missing birthPlace fields → NA, `parse_ok = TRUE` when the file is valid JSON with an id; unreadable/invalid file → `parse_ok = FALSE` row with `espn_id` from the filename.

- [ ] **Step 1: Create fixtures**

`tests/fixtures/espn_mahomes.json` — copy from scratchpad if present, else fetch once:
```bash
S=/private/tmp/claude-501/-Users-nick/894d474a-0a68-42e6-877b-804c54b296b4/scratchpad
cp "$S/mahomes_core.json" tests/fixtures/espn_mahomes.json 2>/dev/null || \
  curl -s "https://sports.core.api.espn.com/v2/sports/football/leagues/nfl/athletes/3139477" \
    -o tests/fixtures/espn_mahomes.json
```

`tests/fixtures/espn_no_birthplace.json` (hand-written — models the junk/partial records ESPN serves):
```json
{"id": "9999999", "fullName": "No Place Guy", "dateOfBirth": "1980-01-02T08:00Z"}
```

- [ ] **Step 2: Write the failing tests**

`tests/testthat/test-espn.R`:
```r
source(file.path(testthat::test_path(), "..", "..", "R", "lib", "espn.R"))

fx <- function(f) file.path(testthat::test_path(), "..", "fixtures", f)

test_that("espn_athlete_url builds the core v2 URL", {
  expect_equal(
    espn_athlete_url("3139477"),
    "https://sports.core.api.espn.com/v2/sports/football/leagues/nfl/athletes/3139477")
})

test_that("parse_espn_athlete extracts birthplace and DOB date-only", {
  row <- parse_espn_athlete(fx("espn_mahomes.json"))
  expect_equal(row$espn_id, "3139477")
  expect_equal(row$birth_city, "Whitehouse")
  expect_equal(row$birth_state, "TX")
  expect_equal(row$espn_dob, "1995-09-17")   # NOT tz-shifted from 1995-09-17T07:00Z
  expect_true(row$parse_ok)
})

test_that("missing birthPlace yields NAs, still parse_ok", {
  row <- parse_espn_athlete(fx("espn_no_birthplace.json"))
  expect_true(is.na(row$birth_city))
  expect_true(is.na(row$birth_country))
  expect_equal(row$espn_dob, "1980-01-02")
  expect_true(row$parse_ok)
})

test_that("garbage file is parse_ok FALSE with id from filename", {
  tmp <- file.path(tempdir(), "1234567.json")
  writeLines("<html>Not Found</html>", tmp)
  row <- parse_espn_athlete(tmp)
  expect_false(row$parse_ok)
  expect_equal(row$espn_id, "1234567")
})
```

- [ ] **Step 3: Run tests to verify they fail**

Run: `Rscript tests/run_tests.R`
Expected: FAIL — `espn_athlete_url` not found

- [ ] **Step 4: Implement `R/lib/espn.R`**

```r
`%||%` <- function(a, b) if (is.null(a)) b else a

espn_athlete_url <- function(espn_id) {
  sprintf("https://sports.core.api.espn.com/v2/sports/football/leagues/nfl/athletes/%s",
          espn_id)
}

parse_espn_athlete <- function(path) {
  id_from_file <- sub("[.]json$", "", basename(path))
  j <- tryCatch(jsonlite::read_json(path), error = function(e) NULL)
  if (is.null(j) || is.null(j$id)) {
    return(tibble::tibble(
      espn_id = id_from_file, full_name = NA_character_,
      birth_city = NA_character_, birth_state = NA_character_,
      birth_country = NA_character_, espn_dob = NA_character_,
      parse_ok = FALSE))
  }
  bp <- j$birthPlace %||% list()
  dob <- j$dateOfBirth %||% NA_character_
  tibble::tibble(
    espn_id       = as.character(j$id),
    full_name     = j$fullName %||% NA_character_,
    birth_city    = bp$city %||% NA_character_,
    birth_state   = bp$state %||% NA_character_,
    birth_country = bp$country %||% NA_character_,
    espn_dob      = if (is.na(dob)) NA_character_ else substr(dob, 1, 10),
    parse_ok      = TRUE)
}
```

- [ ] **Step 5: Run tests to verify pass**

Run: `Rscript tests/run_tests.R`
Expected: PASS (all files)

- [ ] **Step 6: Commit**

```bash
git add R/lib/espn.R tests/
git commit -m "feat: ESPN athlete parser with fixtures"
```

---

### Task 3: ESPN hit-rate gate (100-id sample)

**Files:**
- Create: `R/02_espn_sample.R`

**Interfaces:**
- Consumes: `data/processed/spine.parquet`, `R/lib/espn.R`.
- Produces: `data/processed/espn_sample_report.csv` (columns `era, n, hits, hit_rate`) and a clear PASS/FAIL line on stdout. **GATE: overall hit rate ≥ 0.90 required before Task 4.**

- [ ] **Step 1: Write `R/02_espn_sample.R`**

```r
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
```

- [ ] **Step 2: Run it**

Run: `Rscript R/02_espn_sample.R` (~30s: 100 requests at 5/s)
Expected: per-era table + `OVERALL HIT RATE: ...% — PASS`. If FAIL: **stop, report to the user** — the spec's fallback is PFR index pages.

- [ ] **Step 3: Commit**

```bash
git add R/02_espn_sample.R
git commit -m "feat: ESPN birthplace hit-rate gate"
```

---

### Task 4: Full ESPN pull (background, resumable)

**Files:**
- Create: `R/03_espn_pull.R`

**Interfaces:**
- Consumes: `data/processed/spine.parquet`, `R/lib/espn.R`, PASS from Task 3.
- Produces: `data/raw/espn/{espn_id}.json` for ≥95% of the ~16.8k ids (404s logged, not retried more than once). Safe to re-run: skips existing valid files.

- [ ] **Step 1: Write `R/03_espn_pull.R`**

```r
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
```

- [ ] **Step 2: Launch in the background**

Run (background): `Rscript R/03_espn_pull.R` — expect ~60–75 min for ~16.8k ids at 5/s.
**Do not block on it** — Tasks 5 (census) and 7 (RAE) have no dependency on it; proceed and check back.

- [ ] **Step 3: On completion, verify coverage**

Run: `ls data/raw/espn | wc -l` and `wc -l < data/raw/espn_failures.log`
Expected: cached ≥ 95% of ids; failures file small. Re-run the script once to retry transient failures (it resumes automatically).

- [ ] **Step 4: Commit**

```bash
git add R/03_espn_pull.R
git commit -m "feat: resumable ESPN birthplace pull"
```

---

### Task 5: Census pulls (keyed API: all places incl. CDPs, income; keyless files: gazetteer, counties, shapes)

**Files:**
- Create: `R/05_pull_census.R`

**Interfaces:**
- Produces: `data/processed/census_places.parquet` — one row per Census place: `state (USPS), geoid (chr 7), name_raw, lsad (chr), aland_sqmi (dbl), lat (dbl), lon (dbl), pop2000 (int), pop2010 (int), pop_now (int, ACS5 2023), income1999 (int, 2000 SF3 median HH income), income_now (int, ACS5 2023 median HH income)`. Population/income cover CDPs (decennial + ACS universes).
- Produces: `data/processed/census_zcta.parquet` — `zcta (chr 5), pop_now (int), income_now (int)` (banked for the later high-school/ZIP phase).
- Produces: `data/processed/census_counties.parquet` — `county_fips (chr 5), county_name, state, pop2024 (int)`.
- Produces: `data/raw/census/cb_2023_us_county_500k/` unzipped shapefile dir.

- [ ] **Step 1: Write `R/05_pull_census.R`**

```r
suppressMessages({ library(dplyr); library(readr); library(arrow); library(stringr);
                   library(jsonlite); library(purrr) })

KEY <- Sys.getenv("CENSUS_API_KEY")
stopifnot("CENSUS_API_KEY missing from ~/.Renviron" = nzchar(KEY))

dir.create("data/raw/census/api", showWarnings = FALSE, recursive = TRUE)
dl <- function(url, dest) {
  if (!file.exists(dest)) download.file(url, dest, mode = "wb", quiet = TRUE)
  dest
}

# Cached Census API GET → data.frame. NEVER cat() the URL (contains the key).
census_get <- function(base, vars, geo, in_clause, cache_name) {
  dest <- file.path("data/raw/census/api", paste0(cache_name, ".json"))
  if (!file.exists(dest)) {
    enc <- function(x) gsub(" ", "%20", x)   # Census API wants ':' and '*' literal
    url <- sprintf("%s?get=%s&for=%s%s&key=%s", base, vars, enc(geo),
                   ifelse(nzchar(in_clause), paste0("&in=", enc(in_clause)), ""),
                   KEY)
    res <- tryCatch(curl::curl_fetch_memory(url), error = function(e) NULL)
    if (is.null(res) || res$status_code != 200) {
      warning(sprintf("census_get failed for %s (status %s)", cache_name,
                      ifelse(is.null(res), "conn", res$status_code)))
      return(NULL)
    }
    writeBin(res$content, dest)
    Sys.sleep(0.5)
  }
  m <- fromJSON(dest)
  df <- as.data.frame(m[-1, , drop = FALSE], stringsAsFactors = FALSE)
  names(df) <- m[1, ]
  df
}

# --- Place Gazetteer (names, LSAD, land area, centroid) ---
gz_zip <- dl("https://www2.census.gov/geo/docs/maps-data/data/gazetteer/2023_Gazetteer/2023_Gaz_place_national.zip",
             "data/raw/census/gaz_place_2023.zip")
unzip(gz_zip, exdir = "data/raw/census")
gaz <- read_tsv(list.files("data/raw/census", pattern = "Gaz_place.*[.]txt$",
                           full.names = TRUE),
                col_types = cols(.default = col_character()))
names(gaz) <- trimws(names(gaz))         # last col name is whitespace-padded
gaz <- gaz |>
  transmute(state = USPS, geoid = GEOID, name_raw = NAME, lsad = LSAD,
            aland_sqmi = as.numeric(ALAND_SQMI),
            lat = as.numeric(INTPTLAT), lon = as.numeric(INTPTLONG))
stopifnot(nrow(gaz) > 30000)
state_fips <- sort(unique(substr(gaz$geoid, 1, 2)))

# --- Place populations + income via API (covers CDPs) ---
pull_places <- function(base, vars, tag) {
  map_dfr(state_fips, function(st) {
    df <- census_get(base, vars, "place:*", paste0("state:", st),
                     paste0(tag, "_", st))
    if (is.null(df)) return(NULL)
    df$geoid <- paste0(df$state, df$place)
    df
  })
}

dec2000 <- pull_places("https://api.census.gov/data/2000/dec/sf1", "P001001", "dec2000")
dec2010 <- pull_places("https://api.census.gov/data/2010/dec/sf1", "P0010001", "dec2010")
acs2023 <- pull_places("https://api.census.gov/data/2023/acs/acs5",
                       "B01003_001E,B19013_001E", "acs2023")
sf3_2000 <- pull_places("https://api.census.gov/data/2000/dec/sf3", "P053001", "sf3_2000")

as_int <- function(x) suppressWarnings(as.integer(x))
places <- gaz |>
  left_join(dec2000 |> transmute(geoid, pop2000 = as_int(P001001)), by = "geoid") |>
  left_join(dec2010 |> transmute(geoid, pop2010 = as_int(P0010001)), by = "geoid") |>
  left_join(acs2023 |> transmute(geoid, pop_now = as_int(B01003_001E),
                                 income_now = as_int(B19013_001E)), by = "geoid") |>
  left_join(sf3_2000 |> transmute(geoid, income1999 = as_int(P053001)), by = "geoid") |>
  # ACS uses negative sentinels (-666666666) for suppressed medians → NA
  mutate(income_now = if_else(!is.na(income_now) & income_now < 0, NA_integer_, income_now),
         income1999 = if_else(!is.na(income1999) & income1999 < 0, NA_integer_, income1999))

dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
write_parquet(places, "data/processed/census_places.parquet")
cat(sprintf("places: %d | pop2000: %d | pop2010: %d | pop_now: %d | income_now: %d | CDPs w/ pop_now: %d\n",
            nrow(places), sum(!is.na(places$pop2000)), sum(!is.na(places$pop2010)),
            sum(!is.na(places$pop_now)), sum(!is.na(places$income_now)),
            sum(places$lsad == "57" & !is.na(places$pop_now))))

# --- ZCTA income/population (banked for the high-school phase) ---
z <- census_get("https://api.census.gov/data/2023/acs/acs5",
                "B01003_001E,B19013_001E", "zip code tabulation area:*", "", "zcta2023")
if (!is.null(z)) {
  zcta <- z |> transmute(zcta = `zip code tabulation area`,
                         pop_now = as_int(B01003_001E),
                         income_now = as_int(B19013_001E)) |>
    mutate(income_now = if_else(!is.na(income_now) & income_now < 0, NA_integer_, income_now))
  write_parquet(zcta, "data/processed/census_zcta.parquet")
  cat("zctas:", nrow(zcta), "\n")
}

# --- County populations (keyless popest) ---
co <- read_csv(dl("https://www2.census.gov/programs-surveys/popest/datasets/2020-2024/counties/totals/co-est2024-alldata.csv",
                  "data/raw/census/co-est2024-alldata.csv"),
               col_types = cols(.default = col_character())) |>
  filter(SUMLEV == "050") |>
  transmute(county_fips = paste0(STATE, COUNTY), county_name = CTYNAME,
            state = STNAME, pop2024 = as.integer(POPESTIMATE2024))
stopifnot(nrow(co) > 3000)
write_parquet(co, "data/processed/census_counties.parquet")

# --- County cartographic boundaries (for point-in-polygon + maps) ---
shp_zip <- dl("https://www2.census.gov/geo/tiger/GENZ2023/shp/cb_2023_us_county_500k.zip",
              "data/raw/census/cb_2023_us_county_500k.zip")
unzip(shp_zip, exdir = "data/raw/census/cb_2023_us_county_500k")
cat("county shapefile files:",
    length(list.files("data/raw/census/cb_2023_us_county_500k")), "\n")
```

Fallbacks, in order, if a keyed endpoint errors: (a) re-run (responses are
cached per state — only failures refetch); (b) if 2010 SF1 rejects `P0010001`,
list variables via `https://api.census.gov/data/2010/dec/sf1/variables.json`
and use the total-population variable found there; (c) if the county popest
URL 404s, use the 2023 vintage
(`.../2020-2023/counties/totals/co-est2023-alldata.csv`) and rename
`POPESTIMATE2023` → `pop2024` with a comment.

- [ ] **Step 2: Run it**

Run: `Rscript R/05_pull_census.R` (~152 API calls, cached; a few minutes)
Expected: `places: ~32329`, each pop column ≥ ~25000 non-NA, `CDPs w/ pop_now` ≈ 12000+ (the whole point of the keyed pull), `zctas: ~33791`, counties > 3000, shapefile files ≥ 4.

- [ ] **Step 3: Commit**

```bash
git add R/05_pull_census.R
git commit -m "feat: census pulls incl. CDPs and income (keyed API + gazetteer)"
```

---

### Task 6: Place matcher (TDD)

**Files:**
- Create: `R/lib/places.R`, `tests/testthat/test-places.R`

**Interfaces:**
- Consumes: nothing at runtime (pure functions).
- Produces: `normalize_city(x)` → chr (lowercase, "st."→"saint", strip punctuation, squish).
- Produces: `strip_lsad(name_raw)` → chr (Census NAME without its LSAD suffix or "(balance)").
- Produces: `match_places(players_df, places_df)` → players_df + `geoid, matched_pop2000, matched_pop2010, matched_pop_now, matched_income1999, matched_income_now, aland_sqmi, lat, lon, match_tier (chr: "unique"|"lsad_pref"|"biggest_pop"|"unmatched")`. `players_df` needs `birth_city, birth_state`; `places_df` is `census_places.parquet` shape.

- [ ] **Step 1: Write the failing tests**

`tests/testthat/test-places.R`:
```r
suppressMessages(library(dplyr))
source(file.path(testthat::test_path(), "..", "..", "R", "lib", "places.R"))

test_that("normalize_city handles saint and punctuation", {
  expect_equal(normalize_city("St. Louis"), "saint louis")
  expect_equal(normalize_city("Winston-Salem"), "winston salem")
  expect_equal(normalize_city("O'Fallon"), "ofallon")
  expect_equal(normalize_city("  Tyler "), "tyler")
})

test_that("strip_lsad removes census suffixes and (balance)", {
  expect_equal(strip_lsad("Tyler city"), "Tyler")
  expect_equal(strip_lsad("Mount Olive town"), "Mount Olive")
  expect_equal(strip_lsad("Whitehouse CDP"), "Whitehouse")
  expect_equal(strip_lsad("Indianapolis city (balance)"), "Indianapolis")
  expect_equal(strip_lsad("Nashville-Davidson metropolitan government (balance)"),
               "Nashville-Davidson")
})

test_that("match_places: unique match, lsad preference, unmatched", {
  places <- tibble(
    state = c("TX", "AL", "AL", "PA"),
    geoid = c("4874144", "0101", "0102", "4299"),
    name_raw = c("Tyler city", "Mount Olive town", "Mount Olive CDP", "Erie city"),
    lsad = c("25", "43", "57", "25"),
    aland_sqmi = c(54.5, 3, 2, 19),
    lat = c(32.3, 33.6, 33.7, 42.1), lon = c(-95.3, -86.7, -86.8, -80.1),
    pop2000 = c(83650L, 3957L, NA, 103717L),
    pop2010 = c(96900L, 4079L, NA, 101786L),
    pop_now = c(110000L, 4100L, 900L, 93000L),
    income1999 = c(37000L, 31000L, 30000L, 33000L),
    income_now = c(62000L, 52000L, 50000L, 55000L))
  players <- tibble(
    birth_city = c("Tyler", "Mount Olive", "Nowhereville"),
    birth_state = c("TX", "AL", "ZZ"))
  out <- match_places(players, places)
  expect_equal(out$geoid, c("4874144", "0101", NA))
  expect_equal(out$match_tier, c("unique", "lsad_pref", "unmatched"))
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript tests/run_tests.R`
Expected: FAIL — `normalize_city` not found

- [ ] **Step 3: Implement `R/lib/places.R`**

```r
suppressMessages({ library(dplyr); library(stringr) })

normalize_city <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("\\bst[.]?\\s", "saint ") |>
    str_replace_all("[-]", " ") |>
    str_replace_all("[^a-z ]", "") |>
    str_squish()
}

strip_lsad <- function(name_raw) {
  name_raw |>
    str_remove("\\s*\\(balance\\)\\s*$") |>
    str_remove(paste0(
      "\\s(city and borough|metropolitan government|metro government|",
      "unified government|consolidated government|urban county|",
      "city|town|village|borough|municipality|comunidad|zona urbana|CDP)$"))
}

match_places <- function(players_df, places_df) {
  lookup <- places_df |>
    mutate(city_norm = normalize_city(strip_lsad(name_raw)),
           incorporated = lsad != "57",
           best_pop = coalesce(pop_now, pop2010, pop2000, 0L))

  candidates <- players_df |>
    mutate(.row = row_number(),
           city_norm = normalize_city(birth_city)) |>
    left_join(lookup, by = c("city_norm", "birth_state" = "state"),
              relationship = "many-to-many")

  picked <- candidates |>
    group_by(.row) |>
    mutate(n_cand = sum(!is.na(geoid))) |>
    arrange(desc(incorporated), desc(best_pop), .by_group = TRUE) |>
    slice(1) |>
    ungroup() |>
    mutate(match_tier = case_when(
      is.na(geoid)                 ~ "unmatched",
      n_cand == 1                  ~ "unique",
      incorporated & n_cand > 1    ~ "lsad_pref",
      TRUE                         ~ "biggest_pop"))

  players_df |>
    mutate(.row = row_number()) |>
    left_join(picked |>
                select(.row, geoid, matched_pop2000 = pop2000,
                       matched_pop2010 = pop2010, matched_pop_now = pop_now,
                       matched_income1999 = income1999,
                       matched_income_now = income_now,
                       aland_sqmi, lat, lon, match_tier),
              by = ".row") |>
    select(-.row)
}
```

- [ ] **Step 4: Run tests to verify pass**

Run: `Rscript tests/run_tests.R`
Expected: PASS. Note the test's `lsad_pref` case: incorporated "Mount Olive town" must beat the CDP.

- [ ] **Step 5: Commit**

```bash
git add R/lib/places.R tests/testthat/test-places.R
git commit -m "feat: census place matcher with LSAD preference"
```

---

### Task 7: RAE analysis + figure (no ESPN dependency)

**Files:**
- Create: `R/07_rae.R`
- Create: `docs/figures/` (output dir)

**Interfaces:**
- Consumes: `data/processed/spine.parquet`.
- Produces: `docs/figures/rae_nfl.png`, `data/processed/rae_table.csv` (`era, month, n, share, expected_share, ratio`).

- [ ] **Step 1: Write `R/07_rae.R`**

```r
suppressMessages({ library(dplyr); library(arrow); library(ggplot2); library(lubridate) })

spine <- read_parquet("data/processed/spine.parquet") |>
  filter(!is.na(birth_date))

days_in_month <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
expected <- tibble(month = 1:12, expected_share = days_in_month / sum(days_in_month))

rae <- spine |>
  mutate(era = coalesce(era, "pre-1990"),
         month = month(birth_date)) |>
  count(era, month) |>
  group_by(era) |>
  mutate(share = n / sum(n)) |>
  ungroup() |>
  left_join(expected, by = "month") |>
  mutate(ratio = share / expected_share)

dir.create("docs/figures", showWarnings = FALSE, recursive = TRUE)
write.csv(rae, "data/processed/rae_table.csv", row.names = FALSE)

p <- rae |>
  mutate(month_lab = factor(month.abb[month], levels = month.abb)) |>
  ggplot(aes(month_lab, ratio, group = era)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  geom_col(fill = "#2C7FB8") +
  facet_wrap(~era, nrow = 1) +
  labs(title = "NFL births by month vs. expected",
       subtitle = "Ratio of player birth-month share to days-adjusted uniform baseline",
       x = NULL, y = "Observed / expected",
       caption = "Data: nflverse. Baseline: days-in-month adjusted uniform.") +
  theme_minimal(base_size = 16) +
  theme(panel.grid.minor = element_blank())

ggsave("docs/figures/rae_nfl.png", p, width = 12, height = 6.75, dpi = 320)
cat("wrote docs/figures/rae_nfl.png\n")
print(rae |> group_by(era) |> summarise(n = sum(n)))
```

- [ ] **Step 2: Run and eyeball**

Run: `Rscript R/07_rae.R`
Expected: PNG written; open it (`open docs/figures/rae_nfl.png`) and confirm bars hover near 1.0 (the NFL is expected to show a *weak* RAE relative to hockey — a flat chart is itself a finding, not a bug).

- [ ] **Step 3: Commit**

```bash
git add R/07_rae.R docs/figures/rae_nfl.png
git commit -m "feat: relative age effect analysis + figure"
```

---

### Task 8: Birthplace table (parse ESPN cache, DOB-validated join)

**Files:**
- Create: `R/04_build_birthplace.R`

**Interfaces:**
- Consumes: `data/raw/espn/*.json` (Task 4 complete), `data/processed/spine.parquet`, `parse_espn_athlete`.
- Produces: `data/processed/birthplace.parquet` — spine columns + `birth_city, birth_state, birth_country, espn_dob, dob_match (lgl)`. Only rows with `parse_ok`, non-NA city+state, and `dob_match %in% c(TRUE, NA)` (NA = nflverse birth_date missing) survive. Prints join stats by era.

- [ ] **Step 1: Write `R/04_build_birthplace.R`**

```r
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
```

- [ ] **Step 2: Run and record stats**

Run: `Rscript R/04_build_birthplace.R`
Expected: ~15–16.5k birthplace rows; per-era rates close to the Task 3 sample; DOB mismatches a small number (tens, not thousands — if thousands, the espn_id crosswalk is broken: stop and investigate before proceeding).

- [ ] **Step 3: Commit**

```bash
git add R/04_build_birthplace.R
git commit -m "feat: DOB-validated ESPN birthplace table"
```

---

### Task 9: Match birthplaces to census places

**Files:**
- Create: `R/06_match_places.R`

**Interfaces:**
- Consumes: `data/processed/birthplace.parquet`, `data/processed/census_places.parquet`, `match_places()`.
- Produces: `data/processed/birthplace_matched.parquet` (birthplace + matcher columns), `data/processed/match_report.csv` (`era, n, matched, match_rate, no_pop_share`). US-born filter: `birth_country` is NA or "USA", and `birth_state` in `state.abb` + "DC".

- [ ] **Step 1: Write `R/06_match_places.R`**

```r
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
  print(n = 20)
```

- [ ] **Step 2: Run and review the unmatched list**

Run: `Rscript R/06_match_places.R`
Expected: match_rate ≥ ~0.85 per era. Review the top-20 unmatched: if a *systematic* miss appears (e.g. every "St." city failing, or a big city failing on a name variant), fix `normalize_city`/`strip_lsad` (add a regression test in `tests/testthat/test-places.R` first), re-run. One-off unincorporated communities are expected losses — leave them.

- [ ] **Step 3: Commit**

```bash
git add R/06_match_places.R
git commit -m "feat: birthplace-to-census-place matching with match-rate report"
```

---

### Task 10: Birthplace-effect analysis + figures

**Files:**
- Create: `R/08_birthplace_effect.R`
- Modify: `R/lib/bins.R` (add `cote_bin()`)
- Test: `tests/testthat/test-bins.R` (add bin tests)

**Interfaces:**
- Consumes: `data/processed/birthplace_matched.parquet`, `data/processed/census_places.parquet`.
- Produces: `cote_bin(pop)` → ordered factor with levels `<2.5k, 2.5k–10k, 10k–30k, 30k–50k, 50k–100k, 100k–250k, 250k–500k, 500k+`.
- Produces: `docs/figures/cote_bins.png`, `docs/figures/density_gradient.png`, `data/processed/effect_tables.rds` (list of the two plotted tables).

- [ ] **Step 1: Add failing bin tests to `tests/testthat/test-bins.R`**

```r
test_that("cote_bin buckets populations", {
  expect_equal(as.character(cote_bin(c(1000, 60000, 5e6))),
               c("<2.5k", "50k–100k", "500k+"))
  expect_true(is.na(cote_bin(NA)))
  expect_true(is.ordered(cote_bin(1000)))
})
```

Run: `Rscript tests/run_tests.R` — expected FAIL (`cote_bin` not found).

- [ ] **Step 2: Add `cote_bin` to `R/lib/bins.R`**

```r
cote_bin <- function(pop) {
  cut(pop,
      breaks = c(0, 2500, 10000, 30000, 50000, 100000, 250000, 500000, Inf),
      labels = c("<2.5k", "2.5k–10k", "10k–30k", "30k–50k",
                 "50k–100k", "100k–250k", "250k–500k", "500k+"),
      right = FALSE, ordered_result = TRUE)
}
```

Run: `Rscript tests/run_tests.R` — expected PASS.

- [ ] **Step 3: Write `R/08_birthplace_effect.R`**

Era→population-vintage mapping (approximation, stated on the chart): 1990s/2000s
rookies → `pop2000`; 2010s → `pop2010`; 2020s → `pop_now`. Decennial/ACS pulls
cover CDPs, so suburbs stay in the analysis; the small remainder with no
vintage population (places that didn't exist then) is counted in the caption.

```r
suppressMessages({ library(dplyr); library(arrow); library(ggplot2); library(tidyr) })
source("R/lib/bins.R")

matched <- read_parquet("data/processed/birthplace_matched.parquet") |>
  filter(match_tier != "unmatched", !is.na(era))
places <- read_parquet("data/processed/census_places.parquet")

pl_vintage  <- c("1990s" = "pop2000", "2000s" = "pop2000",
                 "2010s" = "pop2010", "2020s" = "pop_now")

vintage_pop <- function(df) {
  df |> mutate(pop = case_when(
    era %in% c("1990s", "2000s") ~ matched_pop2000,
    era == "2010s"               ~ matched_pop2010,
    era == "2020s"               ~ matched_pop_now))
}

player_bins <- matched |>
  vintage_pop() |>
  filter(!is.na(pop)) |>
  mutate(bin = cote_bin(pop)) |>
  count(era, bin, name = "players") |>
  group_by(era) |> mutate(player_share = players / sum(players)) |> ungroup()

no_vintage_pop <- matched |> vintage_pop() |>
  summarise(share = mean(is.na(pop))) |> pull(share)

pop_bins <- purrr::map_dfr(names(pl_vintage), function(e) {
  col <- pl_vintage[[e]]
  places |>
    filter(!is.na(.data[[col]])) |>
    mutate(bin = cote_bin(.data[[col]])) |>
    group_by(bin) |>
    summarise(pop = sum(.data[[col]]), .groups = "drop") |>
    mutate(era = e, pop_share = pop / sum(pop))
})

effect <- player_bins |>
  left_join(pop_bins, by = c("era", "bin")) |>
  mutate(rep_ratio = player_share / pop_share)

p1 <- ggplot(effect, aes(bin, rep_ratio, fill = era)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("1990s" = "#A6BDDB", "2000s" = "#74A9CF",
                               "2010s" = "#2B8CBE", "2020s" = "#045A8D")) +
  labs(title = "Where NFL players come from, relative to where people live",
       subtitle = "Representation ratio: share of players born in each place size ÷ share of population living there",
       x = "Birthplace population (Census places incl. CDPs)",
       y = "Representation ratio (1 = proportional)",
       fill = "Rookie era",
       caption = sprintf(
         "Data: nflverse + ESPN + US Census (decennial 2000/2010, ACS 2023 by era). %.0f%% of matched players lacked a vintage population and are excluded.",
         100 * no_vintage_pop)) +
  theme_minimal(base_size = 16) +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1))
ggsave("docs/figures/cote_bins.png", p1, width = 12, height = 6.75, dpi = 320)

# Density gradient: deciles of place density (people/sq mi), share of players per decile
density_tbl <- matched |>
  mutate(pop = coalesce(matched_pop_now, matched_pop2010, matched_pop2000),
         density = pop / aland_sqmi) |>
  filter(!is.na(density), is.finite(density), aland_sqmi > 0) |>
  mutate(decile = ntile(density, 10)) |>
  count(era, decile) |>
  group_by(era) |> mutate(share = n / sum(n)) |> ungroup()

p2 <- ggplot(density_tbl, aes(decile, share, color = era)) +
  geom_line(linewidth = 1.2) + geom_point(size = 2.5) +
  scale_x_continuous(breaks = 1:10) +
  scale_color_manual(values = c("1990s" = "#A6BDDB", "2000s" = "#74A9CF",
                                "2010s" = "#2B8CBE", "2020s" = "#045A8D")) +
  labs(title = "NFL player birthplaces by population density",
       subtitle = "Share of players by decile of birthplace density (1 = most rural, 10 = most urban); deciles over matched places",
       x = "Birthplace density decile", y = "Share of players", color = "Rookie era",
       caption = "Data: nflverse + ESPN + Census Gazetteer (land area).") +
  theme_minimal(base_size = 16) +
  theme(panel.grid.minor = element_blank())
ggsave("docs/figures/density_gradient.png", p2, width = 12, height = 6.75, dpi = 320)

saveRDS(list(effect = effect, density = density_tbl),
        "data/processed/effect_tables.rds")
print(effect |> select(era, bin, rep_ratio) |>
        pivot_wider(names_from = era, values_from = rep_ratio))
cat("\nwrote cote_bins.png + density_gradient.png\n")
```

- [ ] **Step 4: Run and interrogate the numbers**

Run: `Rscript R/08_birthplace_effect.R`
Expected: both PNGs written. Sanity checks before trusting the story: (a) rep_ratio for 500k+ should be < 1 if the small-town effect exists at all; (b) no bin should have players but zero population share (would indicate a binning bug); (c) compare eras — the interesting claim is the *change*. If every ratio ≈ 1.0 exactly, suspect a join bug, not a null result.

- [ ] **Step 5: Commit**

```bash
git add R/lib/bins.R R/08_birthplace_effect.R tests/testthat/test-bins.R docs/figures/cote_bins.png docs/figures/density_gradient.png
git commit -m "feat: birthplace effect replication + density gradient"
```

---

### Task 10b: High-school geocoding + hometown table (HS-preferred, birthplace fallback)

**Files:**
- Create: `R/lib/schools.R`, `tests/testthat/test-schools.R`, `R/06b_hometown.R`

**Interfaces:**
- Consumes: `data/raw/sleeper_players.json` (NFL snapshot), `data/processed/birthplace_matched.parquet`, `data/processed/spine.parquet`, `data/processed/census_zcta.parquet`, `match_places()` + `normalize_city()` from `R/lib/places.R`.
- Produces: `parse_sleeper_hs(x)` → tibble `hs_name (chr), hs_state (chr or NA)` from strings like `"Whitehouse (TX)"` / bare names / `"St. Patrick's (Ottawa, CAN)"`.
- Produces: `match_schools(hs_df, nces_df)` → adds `nces_city, nces_state, nces_zip, nces_lat, nces_lon, school_match (lgl)`; match by normalized name within state (strip "high school"/"hs" suffixes, punctuation; require unique match, else unmatched).
- Produces: `data/processed/hometown.parquet` — one row per spine player with: `hometown_source ("high_school"|"birthplace"|NA), hometown_city, hometown_state, hometown_geoid, hometown_lat, hometown_lon, hometown_zcta (chr or NA), pop/density/income columns from the place match, era`.
- Produces: `data/processed/divergence.csv` — for players with BOTH geocoded HS and matched birthplace: `n_both, n_same_city, n_diff_city, divergence_rate` (same-city = normalized city+state equal).

- [ ] **Step 1: Verify NCES EDGE geocode files (live check, then pin URLs)**

Candidate URLs (verify with `curl -sI`, follow the pattern to the latest year if 404):
```
https://nces.ed.gov/programs/edge/data/EDGE_GEOCODE_PUBLICSCH_2324.zip
https://nces.ed.gov/programs/edge/data/EDGE_GEOCODE_PRIVATESCH_2324.zip
```
Each zip contains a file (xlsx or txt/csv per year) with columns including
`NCESSCH/PPIN, NAME, CITY, STATE, ZIP, LAT, LON`. If xlsx-only, convert with
`Rscript -e 'openxlsx::read.xlsx(...)'` (install openxlsx if needed) or use the
Excel-free CSV variant if present. Download both to `data/raw/nces/`, note the
actual filenames and columns in a comment in `R/lib/schools.R`, and write the
combined public+private lookup to `data/processed/nces_schools.parquet` with
exactly the columns `name (chr), city (chr), state (chr USPS), zip (chr),
lat (dbl), lon (dbl)` — that file is what `R/06b_hometown.R` reads.

- [ ] **Step 2: Write failing tests for the parser and matcher**

`tests/testthat/test-schools.R`:
```r
suppressMessages(library(dplyr))
source(file.path(testthat::test_path(), "..", "..", "R", "lib", "schools.R"))

test_that("parse_sleeper_hs handles paren-state, bare, and foreign formats", {
  out <- parse_sleeper_hs(c("Whitehouse (TX)", "Central", "St. Patrick's (Ottawa, CAN)"))
  expect_equal(out$hs_name, c("Whitehouse", "Central", "St. Patrick's"))
  expect_equal(out$hs_state, c("TX", NA, NA))   # non-US-state parens -> NA state
})

test_that("match_schools finds unique in-state name matches", {
  nces <- tibble(name = c("Whitehouse High School", "Central High School",
                          "Central High School"),
                 city = c("Whitehouse", "Springfield", "Shelbyville"),
                 state = c("TX", "IL", "IL"),
                 zip = c("75791", "62701", "62565"),
                 lat = c(32.22, 39.8, 39.4), lon = c(-95.2, -89.6, -88.8))
  hs <- tibble(hs_name = c("Whitehouse", "Central"), hs_state = c("TX", "IL"))
  out <- match_schools(hs, nces)
  expect_true(out$school_match[1])
  expect_equal(out$nces_city[1], "Whitehouse")
  expect_false(out$school_match[2])   # ambiguous within IL -> unmatched
})
```

Run: `Rscript tests/run_tests.R` — expected FAIL (functions not found).

- [ ] **Step 3: Implement `R/lib/schools.R`**

```r
suppressMessages({ library(dplyr); library(stringr) })

parse_sleeper_hs <- function(x) {
  paren <- str_match(x, "^(.*?)\\s*\\(([^)]+)\\)\\s*$")
  name <- coalesce(paren[, 2], x)
  st <- paren[, 3]
  st <- if_else(!is.na(st) & st %in% c(state.abb, "DC"), st, NA_character_)
  tibble::tibble(hs_name = str_squish(name), hs_state = st)
}

normalize_school <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("\\bst[.]?\\s", "saint ") |>
    str_remove_all("\\b(senior|junior|jr|sr)\\b") |>
    str_remove_all("\\b(high school|highschool|high|school|hs|academy|prep|preparatory)\\b") |>
    str_replace_all("[^a-z ]", "") |>
    str_squish()
}

match_schools <- function(hs_df, nces_df) {
  lookup <- nces_df |>
    mutate(school_norm = normalize_school(name)) |>
    filter(school_norm != "")
  hs_df |>
    mutate(.row = dplyr::row_number(), school_norm = normalize_school(hs_name)) |>
    left_join(lookup, by = c("school_norm", "hs_state" = "state"),
              relationship = "many-to-many") |>
    group_by(.row) |>
    mutate(n_cand = sum(!is.na(name))) |>
    slice(1) |>
    ungroup() |>
    mutate(school_match = n_cand == 1,
           nces_city  = ifelse(school_match, city, NA_character_),
           nces_state = ifelse(school_match, hs_state, NA_character_),
           nces_zip   = ifelse(school_match, as.character(zip), NA_character_),
           nces_lat   = ifelse(school_match, lat, NA_real_),
           nces_lon   = ifelse(school_match, lon, NA_real_)) |>
    select(hs_name, hs_state, nces_city, nces_state, nces_zip,
           nces_lat, nces_lon, school_match)
}
```
Note: ambiguous names (multiple NCES schools with the same normalized name in
a state) are deliberately unmatched — precision over recall; those players
fall back to birthplace.

- [ ] **Step 4: Run tests to verify pass**

Run: `Rscript tests/run_tests.R`
Expected: PASS.

- [ ] **Step 5: Write `R/06b_hometown.R`**

```r
suppressMessages({ library(dplyr); library(arrow); library(jsonlite); library(purrr) })
source("R/lib/schools.R"); source("R/lib/places.R")
`%||%` <- function(a, b) if (is.null(a)) b else a

spine <- read_parquet("data/processed/spine.parquet")
bp <- read_parquet("data/processed/birthplace_matched.parquet")

# --- Sleeper HS for NFL, joined via sleeper_id (fallback: name+birth_date) ---
sl <- fromJSON("data/raw/sleeper_players.json", simplifyVector = FALSE) |>
  keep(is.list) |>
  map_dfr(~ tibble(sleeper_id = as.character(.x$player_id %||% NA),
                   full_name = .x$full_name %||% NA_character_,
                   birth_date = .x$birth_date %||% NA_character_,
                   position = .x$position %||% NA_character_,
                   high_school = .x$high_school %||% NA_character_)) |>
  filter(position != "DEF" | is.na(position), !is.na(high_school))

hs <- bind_cols(sl, parse_sleeper_hs(sl$high_school))

# --- NCES lookup (public + private, columns pinned in R/lib/schools.R) ---
nces <- arrow::read_parquet("data/processed/nces_schools.parquet")  # built in Step 1 verification; name/city/state/zip/lat/lon

hs_geo <- bind_cols(hs, match_schools(hs |> select(hs_name, hs_state), nces) |>
                          select(-hs_name, -hs_state))

# Join HS geography to spine. Prefer sleeper_id if spine has it; else name+DOB.
use_sleeper_id <- mean(!is.na(spine$sleeper_id)) > 0.3
if (use_sleeper_id) {
  hs_join <- spine |> inner_join(hs_geo, by = "sleeper_id", suffix = c("", ".sl"))
} else {
  hs_join <- spine |>
    mutate(bd = as.character(birth_date)) |>
    inner_join(hs_geo |> mutate(bd = birth_date),
               by = c("display_name" = "full_name", "bd"), suffix = c("", ".sl"))
}
hs_join <- hs_join |> filter(school_match)

# HS city -> census place (for pop/density/income), HS zip -> ZCTA
hs_matched <- match_places(
  hs_join |> transmute(gsis_id, birth_city = nces_city, birth_state = nces_state),
  read_parquet("data/processed/census_places.parquet")) |>
  rename_with(~ paste0("hs_", .x), -gsis_id)

hometown <- spine |>
  left_join(hs_join |> select(gsis_id, nces_city, nces_state, nces_zip,
                              nces_lat, nces_lon), by = "gsis_id") |>
  left_join(hs_matched, by = "gsis_id") |>
  left_join(bp |> select(gsis_id, birth_city, birth_state, geoid, lat, lon,
                         matched_pop2000, matched_pop2010, matched_pop_now,
                         matched_income1999, matched_income_now, aland_sqmi,
                         match_tier), by = "gsis_id") |>
  mutate(
    hometown_source = case_when(
      !is.na(nces_city)                              ~ "high_school",
      !is.na(geoid) & match_tier != "unmatched"      ~ "birthplace",
      TRUE                                           ~ NA_character_),
    hometown_city  = if_else(hometown_source == "high_school", nces_city, birth_city),
    hometown_state = if_else(hometown_source == "high_school", nces_state, birth_state),
    hometown_geoid = if_else(hometown_source == "high_school", hs_geoid, geoid),
    hometown_lat   = if_else(hometown_source == "high_school", nces_lat, lat),
    hometown_lon   = if_else(hometown_source == "high_school", nces_lon, lon),
    hometown_zcta  = if_else(hometown_source == "high_school",
                             substr(nces_zip, 1, 5), NA_character_))

write_parquet(hometown, "data/processed/hometown.parquet")

both <- hometown |>
  filter(!is.na(nces_city), !is.na(birth_city), match_tier != "unmatched")
divergence <- tibble(
  n_both = nrow(both),
  n_same_city = sum(normalize_city(both$nces_city) == normalize_city(both$birth_city) &
                    both$nces_state == both$birth_state),
  n_diff_city = n_both - n_same_city,
  divergence_rate = n_diff_city / n_both)
write.csv(divergence, "data/processed/divergence.csv", row.names = FALSE)
print(count(hometown, hometown_source))
print(divergence)
```
(Exact column plumbing may need adjustment against the real intermediate
files — the contract is the Produces block above; keep it.)

- [ ] **Step 6: Run and record**

Run: `Rscript R/06b_hometown.R`
Expected: `hometown_source` counts printed (high_school should dominate 2020s,
birthplace should dominate 1990s/2000s); divergence table with a plausible
rate (literature suggests meaningful born≠raised divergence; if it's 0% or
100%, the city normalization or join is broken).

- [ ] **Step 7: Commit**

```bash
git add R/lib/schools.R tests/testthat/test-schools.R R/06b_hometown.R
git commit -m "feat: HS-preferred hometown table with birthplace fallback + divergence stat"
```

---

### Task 11: County per-capita heatmap

**Files:**
- Create: `R/09_county_map.R`

**Interfaces:**
- Consumes: `data/processed/hometown.parquet` (`hometown_lat/hometown_lon`, HS-preferred per the user decision), `data/raw/census/cb_2023_us_county_500k/`, `data/processed/census_counties.parquet`.
- Produces: `docs/figures/county_map.png`, `data/processed/county_rates.csv` (`county_fips, players, pop2024, per_million`).

- [ ] **Step 1: Write `R/09_county_map.R`**

```r
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
        legend.position = "right")
ggsave("docs/figures/county_map.png", p, width = 12, height = 8, dpi = 320)
cat("top 10 counties by per-capita production (min 5 players):\n")
rates |> filter(players >= 5) |> arrange(desc(per_million)) |> head(10) |> print()
```

- [ ] **Step 2: Run and eyeball**

Run: `Rscript R/09_county_map.R` (~1–2 min for the spatial join)
Expected: map renders with visible geographic structure (the South should glow — if the map is uniform or empty, the point-in-polygon join failed). Top-10 list should look plausible (small Southern counties, Samoa-heavy patterns are a known real phenomenon at the state level).

- [ ] **Step 3: Commit**

```bash
git add R/09_county_map.R docs/figures/county_map.png
git commit -m "feat: county per-capita production heatmap"
```

---

### Task 11b: Hometown income gradient (the money hypothesis)

**Files:**
- Create: `R/10_income.R`

**Interfaces:**
- Consumes: `data/processed/birthplace_matched.parquet`, `data/processed/census_places.parquet`.
- Produces: `docs/figures/income_gradient.png`, `data/processed/income_table.csv` (`era, income_quartile, n, share`).

Method: era-fair income comparison via **population-weighted percentile ranks
within each income vintage**. For 1990s/2000s cohorts use `income1999`
(2000 SF3 — income earned when those players were kids); for 2010s/2020s use
`income_now` (ACS5 2023). Rank every place by income within its vintage,
weighting by that vintage's population, so "quartile 4" always means "the
places where the richest quarter of Americans lived." No inflation adjustment
needed.

- [ ] **Step 1: Write `R/10_income.R`**

```r
suppressMessages({ library(dplyr); library(arrow); library(ggplot2) })

matched <- read_parquet("data/processed/birthplace_matched.parquet") |>
  filter(match_tier != "unmatched", !is.na(era))
places <- read_parquet("data/processed/census_places.parquet")

# Population-weighted income quartile cutpoints per vintage
wq <- function(income, pop) {
  ok <- !is.na(income) & !is.na(pop) & pop > 0
  o <- order(income[ok])
  cw <- cumsum(as.numeric(pop[ok][o])) / sum(as.numeric(pop[ok][o]))
  list(income = income[ok][o], cw = cw)
}
cuts <- function(w) sapply(c(.25, .5, .75), function(q) w$income[which.max(w$cw >= q)])

cut99  <- cuts(wq(places$income1999, places$pop2000))
cutnow <- cuts(wq(places$income_now, places$pop_now))

income_tbl <- matched |>
  mutate(vintage = if_else(era %in% c("1990s", "2000s"), "1999", "now"),
         income = if_else(vintage == "1999", matched_income1999, matched_income_now),
         q = case_when(
           is.na(income) ~ NA_integer_,
           vintage == "1999" ~ findInterval(income, cut99) + 1L,
           TRUE              ~ findInterval(income, cutnow) + 1L)) |>
  filter(!is.na(q)) |>
  count(era, income_quartile = q) |>
  group_by(era) |> mutate(share = n / sum(n)) |> ungroup()

write.csv(income_tbl, "data/processed/income_table.csv", row.names = FALSE)

p <- ggplot(income_tbl, aes(factor(income_quartile), share, fill = era)) +
  geom_hline(yintercept = 0.25, linetype = "dashed", color = "grey40") +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  scale_fill_manual(values = c("1990s" = "#A6BDDB", "2000s" = "#74A9CF",
                               "2010s" = "#2B8CBE", "2020s" = "#045A8D")) +
  scale_x_discrete(labels = c("Q1 (poorest)", "Q2", "Q3", "Q4 (richest)")) +
  labs(title = "Do NFL players increasingly come from richer hometowns?",
       subtitle = "Share of players by hometown income quartile (population-weighted, era-matched income vintages); dashed line = proportional",
       x = "Hometown median household income quartile", y = "Share of players",
       fill = "Rookie era",
       caption = "Data: nflverse + ESPN + US Census (2000 SF3 income for 1990s/2000s cohorts, ACS 2023 for 2010s/2020s).") +
  theme_minimal(base_size = 16) +
  theme(panel.grid.minor = element_blank())
ggsave("docs/figures/income_gradient.png", p, width = 12, height = 6.75, dpi = 320)
print(income_tbl |> tidyr::pivot_wider(names_from = era, values_from = share,
                                       id_cols = income_quartile))
```

- [ ] **Step 2: Run and interrogate**

Run: `Rscript R/10_income.R`
Expected: PNG written; each era's four shares sum to 1. Michael's hypothesis
predicts Q4 rising across eras for pay-to-play sports — for the NFL
(school-based) a *flat* pattern is plausible and is itself the story's
contrast baseline. If any quartile share is > 0.6, suspect the weighted
cutpoints, not reality.

- [ ] **Step 3: Commit**

```bash
git add R/10_income.R docs/figures/income_gradient.png
git commit -m "feat: hometown income gradient by era"
```

---

### Task 12: GitHub Pages report + publish

**Files:**
- Create: `docs/index.html`
- Uses: all four PNGs in `docs/figures/`, tables in `data/processed/` for inline numbers.

**Interfaces:**
- Consumes: figures + `effect_tables.rds`, `match_report.csv`, `rae_table.csv`, `county_rates.csv` (pull real numbers into the copy — no invented stats).
- Produces: live page at `https://stranger9977.github.io/hometown-effect/`.

- [ ] **Step 1: Build `docs/index.html`**

Before writing any HTML/chart markup, read the `dataviz` skill (it triggers on
dashboards/report pages) and follow its palette/typography rules. Structure — a
single scrolling story page, self-contained CSS (no CDNs), light+dark via
`prefers-color-scheme`:

1. Hook: the 2006 finding (11–21x small-town) and Michael's suburb critique.
2. "What we did": nflverse spine → ESPN birthplace (n = actual row count) →
   census match (match rate from `match_report.csv`, stated plainly) →
   HS-preferred hometown (source counts + the born-vs-raised divergence rate
   from `divergence.csv` — a headline stat, present it prominently).
3. Figure sections: `figures/cote_bins.png`, `figures/density_gradient.png`,
   `figures/income_gradient.png`, `figures/county_map.png`,
   `figures/rae_nfl.png` — each with a 2–3 sentence takeaway written from the
   actual tables, and an honest-limitations line (match rate, era/vintage
   approximation, birthplace ≠ hometown).
4. "What's next": high-school supplement (Sleeper has schools for NFL/NBA/MLB),
   PFR historical backfill, MLB/NHL/NBA breadth, ZCTA-level income.

Every number in the copy must come from the generated tables — read them before
writing. `<img>` tags use relative `figures/...` paths, `max-width:100%`.

- [ ] **Step 2: Verify the page locally**

Run: `open docs/index.html` — check images load, copy reads well, dark mode works
(toggle macOS appearance or use DevTools emulation).

- [ ] **Step 3: Commit and push everything**

```bash
git add docs/index.html docs/figures/
git commit -m "feat: shareable GitHub Pages report"
git push
```

- [ ] **Step 4: Flip repo public + enable Pages**

```bash
gh repo edit stranger9977/hometown-effect --visibility public --accept-visibility-change-consequences
gh api repos/stranger9977/hometown-effect/pages -X POST \
  -f "source[branch]=main" -f "source[path]=/docs" 2>/dev/null \
  || gh api repos/stranger9977/hometown-effect/pages -X PUT \
       -f "source[branch]=main" -f "source[path]=/docs"
```

- [ ] **Step 5: Verify live**

Run: `sleep 90 && curl -sI https://stranger9977.github.io/hometown-effect/ | head -3`
Expected: `HTTP/2 200`. Then fetch the page and confirm an `<img>` URL also returns 200.

- [ ] **Step 6: Final commit of any tweaks + push**

```bash
git add -A && git commit -m "chore: pages polish" --allow-empty && git push
```

---

## Execution order & dependencies

- Task 1 → 2 → 3 strictly in order (gate at 3).
- Task 4 launches **in the background** immediately after 3 passes.
- Tasks 5, 6, 7 run while 4 downloads (no dependency).
- Task 8 waits on 4; then 9 → 10 → 10b → 11 → 11b → 12 in order
  (10b also needs Tasks 5 and 6; era-comparison figures in 10/11b stay
  birthplace-based, headline map in 11 uses the hometown table).

## Out of scope (explicitly)

- High-school supplement (Sleeper join — snapshots for NFL/NBA/MLB saved in
  `data/raw/`), PFR backfill, other leagues.
- ZCTA-level income analysis (data banked by Task 5 for the high-school phase).
- 1990 su-99-10 fixed-width files (era approximation documented instead).
