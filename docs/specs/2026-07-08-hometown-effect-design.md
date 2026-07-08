# hometown-effect — Design

**Date:** 2026-07-08
**Collaborators:** Nick (analysis), Michael Mackelvie (video/story)
**Status:** Approved design, POC-first

## Goal

Back a video on *where pros come from, when they're born, and whether money has
changed the answer*. Re-examine the birthplace effect (Côté, Macdonald, Baker &
Abernethy 2006: US pros 11–21x more likely to come from towns of 50k–100k) with
modern data, fixing its known flaws:

1. **Suburb problem** — a 25k-person suburb of Dallas is not a small town.
   Use population *density* and metro context, not raw city-size bins.
2. **Birthplace ≠ where you grew up** — supplement with high-school location
   when it changes the story.
3. **The money hypothesis** — has hometown affluence become a stronger
   predictor as youth sports professionalized (pay-to-play)? Contrast
   school-based sports (football) with club-based ones (baseball, hockey).
4. **Relative age effect refresh** — birth-month distributions by sport,
   free once birthdates are loaded.

### Context: Michael's correspondence with Côté (Feb 2025, paraphrased)

Michael emailed Jean Côté (via a David Epstein referral) asking (1) whether the
2006 study's "small towns" could really be suburbs — his own hometown,
Wilsonville OR, is a "small town" 20 minutes from Portland — and (2) whether
the study was ever revisited. Côté's reply, paraphrased:

- International replications since 2006 are inconsistent, but **in North
  American pro sports the effect keeps showing up**.
- Population size/density should be read as a **proxy** for the sports
  activities and social dynamics of a place, not a cause.
- **Proximity to a bigger center has not been examined** — he agrees it should
  be, considering physical/social infrastructure. (Our density + metro-context
  approach is exactly this.)
- Birthplace studies are inherently **lagged ~15–20 years** (they capture the
  development environments of athletes now in their 20s), and youth sport has
  professionalized dramatically in the interim — which is Michael's money
  hypothesis in Côté's own framing.

The Wilsonville-vs-Portland question is the story hook; "we ran the revisit
Côté said hadn't happened" is the credibility beat. (Full email stays private;
do not quote it verbatim or publish contact details.)

Headline questions, in order of video appeal:

- Is the small-town effect real once you measure it properly — and is it dying?
- Which places overproduce pros per capita (US map heatmaps)?
- Do richer zip codes increasingly produce the pros, and does it differ by sport?
- Which sports still show a strong relative age effect?

## Deliverable

A **GitHub Pages HTML report** at `docs/index.html` (Pages configured to serve
`/docs` on `main`), shareable with Michael: a scrolling story page with the US
map heatmaps, era-comparison charts, RAE charts, and short written takeaways.
Figures export at video resolution to `docs/figures/`. Repo flips public when
Pages goes live (Pages requires public on the free plan).

## Stack

R 4.5.2 (tidyverse, arrow, nflreadr — all installed; `usmap` + `sf` installed
and verified rendering a 3,222-county choropleth). Python/other tools only
where R fights us. Quarto is not installed; the report page is hand-built HTML
(matches the video-pitch aesthetic better than notebook output anyway).

## Phase 0 — POC (tonight): NFL, nflverse spine + ESPN birthplace

All source claims below were empirically verified on 2026-07-08 (see
"Scouting evidence" at the end).

**Spine:** nflverse `players.parquet` — 25,033 players, has `birth_date`,
`rookie_season`, draft info, `college_name`, and ID crosswalks (`espn_id`,
`pfr_id`, `sleeper_id`, `gsis_id`). It has **no birthplace and no high school**.

**Birthplace source: ESPN core v2 API** (keyless):
`https://sports.core.api.espn.com/v2/sports/football/leagues/nfl/athletes/{espn_id}`
returns `birthPlace {city, state, country}` + `dateOfBirth`.

- Join ceiling: 16,765/25,033 nflverse players have `espn_id`
  (2020s rookies 99.4%, 2010s 97.3%, 2000s 76.2%, 1990s 75.8%, 1980s 18.6%).
  Expected yield ≈ 15.5–16.5k players with city+state; solid for 1990+.
- **Step 1 before the full pull: sample ~100 random espn_ids and measure the
  birthPlace hit rate** (the 100% estimate rests on 6 spot checks).
- Pull at ~5 req/s with on-disk JSON cache and checkpointed resume (~30–60 min,
  one-time job). Iterate over nflverse ids only — ESPN's own athlete list
  contains junk placeholder records.
- Parse `dateOfBirth` as the leading date substring only (values carry a tz
  offset like `1995-09-17T07:00Z`; tz conversion can shift the day).
- Don't require `country` (often absent for US-born players); don't impute USA.
- Validate every join row by comparing ESPN `dateOfBirth` to nflverse
  `birth_date`; mismatches are crosswalk errors, not data.

**POC outputs:**
1. RAE: birth-month distribution, NFL, by rookie-era cohort (nflverse alone).
2. Birthplace city → Census place population + density → modern replication of
   the Côté bins **and** the density-based redo, split by rookie era
   (e.g. pre-2000 vs 2000–2012 vs 2013+, adjusted to where coverage supports).
3. First US map heatmap: pros per million residents by county.
4. Money hypothesis, first cut: share of players born in top/bottom-quartile
   income places by era (era-matched income vintages, percentile-ranked).

## Phase 1 — Census join

**Key status: DONE — user's key lives in `~/.Renviron` as `CENSUS_API_KEY`
(auto-loaded by R). Never commit or print it; the repo gitignores `.Renviron`.**

With the key, the primary population source is the API (covers CDPs, killing
the incorporated-only bias): decennial 2000 SF1 `P001001` and 2010 SF1
`P0010001` for all places per state, ACS5 2023 `B01003_001E` for current
populations, plus **median household income** — ACS5 2023 `B19013_001E` (place
and ZCTA) and 2000 SF3 `P053001` (era-appropriate income for older cohorts).
Income comparisons across eras use within-vintage percentile ranks, which
sidesteps inflation adjustment.

Keyless files still used:

- **Place Gazetteer 2023** (`2023_Gaz_place_national.zip`): 32,329 places
  (incl. 12,523 CDPs) — the canonical name-matching table (NAME, LSAD, GEOID).
- **ZCTA Gazetteer 2023**: 33,791 ZCTAs with `ALAND_SQMI` → density
  denominators without shapefiles.
- **County popest** (`co-est2024-alldata.csv`) for county map denominators;
  TIGER cartographic county shapefile for point-in-polygon and mapping.
  (The place popest CSVs and 1990s fixed-width files are now fallback-only.)

**City-name matcher** (birth city string → Census place): lowercase, strip
punctuation, strip LSAD suffix (city/town/village/borough/CDP), join on
(state, base name). Resolve the 208 within-state collisions by preferring
incorporated LSAD over CDP; handle "(balance)" consolidated cities; PA/NJ/MN
townships and unincorporated communities won't match — route to a county-level
fallback. **Report the match rate as a first-class number** before any
conclusions; the 2006 paper's suburb blindness is exactly the kind of silent
data artifact we're claiming to fix.

## Hometown definition (user decision 2026-07-08)

**High school location is the primary geography wherever Sleeper provides it;
birthplace is the fallback.** Each player gets a `hometown_source` flag
("high_school" | "birthplace"). Rules:

- High schools geocode via the NCES EDGE school files (name + state →
  city/ZIP/lat/lon), public + private. Fuzzy name matching; unmatched schools
  fall back to birthplace.
- **Era comparisons use birthplace-only** for proxy consistency — Sleeper HS
  coverage is modern-era, and switching proxies mid-timeline would confound
  the then-vs-now story. Headline modern-era maps and stats use the
  HS-preferred hometown.
- For players with both, report the divergence rate (born in X, raised in Y)
  — a video-worthy stat and the empirical answer to "does birthplace even
  measure the right thing?"

## Phase 2 — High-school supplement (see how the story changes)

- **Modern era, cheap — and multi-sport:** Sleeper snapshots (all downloaded
  to `data/raw/`, verified 2026-07-08). `high_school` (school name + state, no
  town) is populated for **NFL 10,808, MLB 4,158, NBA 1,797 — NHL zero**
  (`/v1/players/{nfl,mlb,nba,nhl}`). MLB's Sleeper dump also carries
  `birth_city` for 5,875 players (cross-check against Lahman). NFL joins via
  nflverse `sleeper_id` (NOT Sleeper's own `gsis_id`: 32% populated, 866
  values whitespace-corrupted); NBA/MLB join by name + birth_date. Parse state
  with `\(([A-Z]{2})\)$`; ~9% are bare names; filter out team-DEF records.
- **Town-level and historical:** PFR player pages have birthplace + high school
  back to at least 1956 rookies, but live scraping is Cloudflare-403-blocked
  today. Later path: curl_cffi/Playwright at ≤20 req/min; the
  `friv/birthplaces.cgi` state-index pages give birthplaces for ~every US-born
  player ever in ~150–250 requests (100x cheaper than player pages); school
  towns come from `schools/high_schools.cgi?id=` per *unique school*.
- First cut of "does high school change the story": state-level comparison of
  birth state vs high-school state (movers vs stayers), modern era only.

## Phase 3 — Breadth: the other leagues

| League | Source (verified) | Effort |
|---|---|---|
| MLB | Lahman v14.0-0 `People` table (`People.RData` from cdalzell/Lahman) — 19,998/20,096 US-born with birthCity, 1871–2025. Filter non-null `debut`. | trivial |
| NHL | One call: `search.d3.nhle.com/api/v1/search/player?limit=100000&q=%2A` — 23,471 players with birthCity/StateProvince inline; 5,611 US-born. Filter `lastSeasonId` non-null (actually played). Per-player landing endpoint only if DOB needed. | trivial |
| NBA | Basketball-Reference `friv/birthplaces.fcgi?country=US&state=XX` — 50 index pages → all 4,699 US-born with city+state. Same Cloudflare stack as PFR (worked today; plan for curl_cffi anyway). B-R player pages also carry High School with town. | one evening |

No shared player IDs across leagues; cross-league comparison happens on
normalized (city, state) and on distributions, not player-level joins.
Canada-born NHL players are out of scope for US census joins (note in report).

## Visualizations (dataviz skill before building; all export video-res)

- Per-capita production choropleths by county, per sport, per era —
  `usmap`/`sf`; hex/binned variant to keep empty-county noise down.
- Then-vs-now diff maps (where production grew/died between eras).
- Côté-bin replication chart: odds ratio by city-size bin, 2006 vs now.
- Density gradient: P(pro | birthplace density decile) by sport and era.
- Hometown median-income distributions by sport and era (needs API key).
- RAE month charts by sport with cutoff-date annotations.

## Repo layout

```
R/               numbered pipeline scripts (01_pull_*, 02_clean_*, ...)
data/raw/        untouched downloads (gitignored; snapshot Sleeper here)
data/processed/  parquet intermediates (gitignored)
docs/            GitHub Pages root: index.html, figures/, specs/
output/figures/  working exports before they're promoted to docs/figures
```

## Risks (ranked, from scouting)

1. ESPN birthplace hit rate unmeasured league-wide → 100-id sample first;
   if <90%, pivot to PFR index pages as primary.
2. ESPN throttling mid-scrape → 5 req/s, disk cache, checkpointed resume.
3. Birthplace→place match rate (CDPs, unincorporated, townships, collisions)
   → tiered matcher with county fallback; publish the match rate.
4. ~~No Census key yet~~ → resolved: key in `~/.Renviron` (never commit/print).
5. Sports Reference Cloudflare (PFR 403 today) → curl_cffi/Playwright later,
   Wayback CDX fallback (filter statuscode:200).
6. Pre-1990 era hole in ESPN join → PFR birthplaces.cgi backfill later;
   frame POC-era claims to the coverage we actually have.
7. Silent join corruption → DOB cross-check on every ESPN row; Sleeper joins
   only via nflverse `sleeper_id`.

## Scouting evidence

Full structured scout reports and decision memo:
`/private/tmp/claude-501/-Users-nick/894d474a-0a68-42e6-877b-804c54b296b4/tasks/wl5r5c2j4.output`
(six agents, 2026-07-08: Sleeper, ESPN, PFR, Census, MLB/NHL/NBA, synthesis).
Key artifacts already in scratchpad: `sleeper_players.json` (14MB snapshot),
`players.parquet`, ESPN sample responses, `People.RData`, `nhl_all.json`,
county choropleth render test.
