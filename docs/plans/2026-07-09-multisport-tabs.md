# Multi-sport tabs + public-report tone — Plan addendum

> Executed with subagent-driven development. Builds on the POC plan
> (2026-07-08); reuses its Global Constraints, era cohorts (rookie/debut
> 1990+ → 1990s/2000s/2010s/2020s), match_places, vintage logic, figure specs.

**Goal:** docs/index.html becomes a tabbed, public-voiced report covering
NFL, MLB, NHL, NBA with the same suburb-corrected analysis per sport, plus a
cross-sport comparison. Tone: neutral publication voice — no first-person
email-recap framing (user directive 2026-07-09).

## Task A — Standardized multi-sport player tables

Create `R/11_pull_mlb.R`, `R/12_pull_nhl.R`, `R/13_pull_nba.R`, each writing
`data/processed/players_{sport}.parquet` with the SAME schema:
`sport (chr), player_name (chr), birth_city (chr), birth_state (chr),
birth_country (chr), birth_date (chr YYYY-MM-DD or NA), era (chr or NA)`.

- **MLB** — `data/raw/People.RData` (Lahman v14; if missing:
  `https://raw.githubusercontent.com/cdalzell/Lahman/master/data/People.RData`).
  Filter non-null `debut` (players only). era from debut year (1990+ cohorts,
  else NA). birth_state is 2-letter for US. birth_date from birthDate or
  birthYear/Month/Day.
- **NHL** — `data/raw/nhl_all.json` (bulk search: 23,471 players incl.
  birthCity/birthStateProvince/birthCountry, lastSeasonId; refetch URL:
  `https://search.d3.nhle.com/api/v1/search/player?culture=en-us&limit=100000&q=%2A`).
  Filter to players who actually played (non-null lastSeasonId). For players
  with lastSeasonId >= 19891990, fetch `https://api-web.nhle.com/v1/player/{id}/landing`
  (curl pool, ≤6 concurrent, cached to data/raw/nhl_landing/, resumable) for
  birthDate + first NHL season (min regular-season entry in seasonTotals,
  gameTypeId==2) → era. Older players: era NA, no birth_date. Keep ALL birth
  countries (RAE uses everyone; geography filters US later).
- **NBA** — scrape `https://www.basketball-reference.com/friv/birthplaces.fcgi?country=US&state={XX}`
  for the 50 states + DC. ≤20 req/min (3.2s sleep), browser User-Agent, cache
  each HTML to data/raw/bbref/. Parse table rows: player name, From (rookie
  year → era), City. birth_state = the state queried; birth_country = USA;
  birth_date = NA (skip NBA RAE). If Cloudflare 403s: fall back to Wayback
  (`http://web.archive.org/web/2026/{url}`, verify not a stored 403). If both
  fail → the NBA tab ships as "coming soon"; do not fabricate.

Print row/era counts per sport. Commit scripts only.

## Task B — Multi-sport analysis + figures

Create `R/14_multisport_effect.R` (+ move the era→vintage helper into
`R/lib/bins.R` as `vintage_pop(df)` with a testthat test so 08 and 14 share it).

For each sport (incl. NFL from birthplace_matched): filter US-born,
`match_places`, print match rate (report as a first-class table:
`data/processed/multisport_match.csv`). Then per sport:
- `docs/figures/cote_bins_{mlb,nhl,nba}.png` — same rep-ratio chart as NFL's
  (same era palette, same theme, captions state incorporated-places caveat).
- `docs/figures/county_map_{mlb,nhl,nba}.png` — per-capita county map,
  same spec as R/09 (white bg, sqrt scale, magma, legible caption).
- `docs/figures/rae_{mlb,nhl}.png` — birth-month ratio charts (MLB: all
  players with birth_date; NHL: all countries, note the Canadian-dominated
  pool). NBA skipped (no birth dates).
- `docs/figures/crosssport_bins.png` — THE payoff figure: rep ratio by place
  size, one line/color per sport, faceted 1990s vs 2020s. Sport colors:
  Okabe-Ito categorical (NFL #0072B2, MLB #D55E00, NHL #009E73, NBA #CC79A7);
  run the dataviz palette validator if node is available, else note it.
- `data/processed/multisport_tables.rds` — all plotted tables for page copy.

QA every PNG by reading it (caption visible from left edge, legends, no
overlap — the POC shipped two figure defects; don't repeat).

## Task C — Tabbed public-voice report

Rework `docs/index.html`:
1. **Tone pass (whole page):** neutral public voice. Remove "Michael asked
   Côté", "Côté told Michael", byline chattiness. The correspondence becomes:
   "the original authors have confirmed suburbs were never controlled for and
   proximity to larger metros was never examined" — no names in the body
   beyond citing the 2006 paper; keep the byline line only.
2. **Tabs:** NFL (default) | MLB | NHL | NBA | Across sports. No external JS;
   accessible buttons (role=tablist, aria-selected, keyboard arrows), CSS
   consistent with current design, deep-linkable via location.hash.
3. Each sport tab: intro stat tiles (n players, match rate), bins figure +
   takeaway from the actual tables, county map, RAE where it exists,
   sport-specific caveats (NHL: US-born only for geography, n≈5.6k; NBA: no
   birth dates; MLB: Lahman completeness).
4. "Across sports" tab: crosssport_bins.png + the divergence/flattening
   story told comparatively; keep the money-hypothesis paragraph as pending.
5. Numbers only from generated tables. Push to main (Pages auto-redeploys),
   verify live 200 + one new figure 200.

## Order

A → B → C, review after each; final live verification + numbers spot-check.
