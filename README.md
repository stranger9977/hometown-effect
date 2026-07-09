# hometown-effect

Where do pros actually come from — and has money changed the answer?

A modern, suburb-corrected re-examination of the "birthplace effect" (Côté,
Macdonald, Baker & Abernethy 2006, *Journal of Sports Sciences*), which found
US pros were 11–21x more likely to come from towns of 50k–100k than from big
cities. NFL proof-of-concept first; MLB/NHL/NBA next.

**Read the report:** the analysis writeup lives at `docs/index.html`
(served via GitHub Pages).

## Headline POC findings (NFL)

- The small-town effect doesn't survive the suburb fix: places <2.5k
  underproduce in every era; the engine is mid-size cities (250k–500k, ~2x),
  and 500k+ metros slid from 1.07x (1990s) to 0.82x (2020s).
- 35% of players with both records went to high school in a different city
  than they were born in — birthplace was always a noisy proxy.
- The NFL's relative age effect is weak (no month >~15% above baseline).

## Reproducing

Requirements: R ≥ 4.5 with `tidyverse`, `arrow`, `jsonlite`, `curl`, `sf`,
`usmap`, `testthat` (and `openxlsx` for the NCES private-school file).

Setup:
1. Get a free Census API key (https://api.census.gov/data/key_signup.html),
   activate it via the signup email, and put `CENSUS_API_KEY=<key>` in
   `~/.Renviron`. Without it, `05` runs in a documented keyless fallback mode
   (incorporated places only, no income).
2. NCES school geocodes: download the EDGE public+private zips (URLs pinned in
   the header comment of `R/lib/schools.R`) into `data/raw/nces/` — `06b`
   names the files it expects if they're missing.

Run from the repo root, in order:

```
Rscript R/01_build_spine.R        # nflverse spine (auto-downloads)
Rscript R/02_espn_sample.R        # 100-id ESPN hit-rate gate
Rscript R/03_espn_pull.R          # full ESPN birthplace pull (~10 min, resumable)
Rscript R/04_build_birthplace.R   # parse + DOB-validated join
Rscript R/05_pull_census.R        # census places/counties/shapes (+ income w/ key)
Rscript R/06_match_places.R       # birthplace -> census place matching
Rscript R/06b_hometown.R          # Sleeper HS + NCES geocode -> hometown table
Rscript R/07_rae.R                # relative age effect figure
Rscript R/08_birthplace_effect.R  # size bins + density figures
Rscript R/09_county_map.R         # county per-capita heatmap
Rscript tests/run_tests.R         # test suite
```

Every join prints its match rate; `data/` is gitignored (all inputs are
re-downloadable), figures export to `docs/figures/`.

## Layout

```
R/               numbered pipeline scripts; pure logic in R/lib/ (tested)
data/raw/        untouched downloads (gitignored)
data/processed/  parquet intermediates (gitignored)
docs/            GitHub Pages root: index.html report, figures/, specs/, plans/
tests/           testthat suite + fixtures
```
