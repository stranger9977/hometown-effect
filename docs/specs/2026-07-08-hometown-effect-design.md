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

## Phase 1 — Census join

**Prerequisite (user action): free instant API key from
`https://api.census.gov/data/key_signup.html`, exported as `CENSUS_API_KEY`.**
Keyless api.census.gov is dead as of 2025 (302 → missing_key.html — verified).

Keyless files that don't wait on the key:

- **Place Gazetteer 2023** (`2023_Gaz_place_national.zip`): 32,329 places
  (incl. 12,523 CDPs) — the canonical name-matching table (NAME, LSAD, GEOID).
- **ZCTA Gazetteer 2023**: 33,791 ZCTAs with `ALAND_SQMI` → density
  denominators without shapefiles.
- **Popest CSVs**: `sub-est2024.csv` (current, incorporated places only),
  `sub-est00int.csv` (2000s), `su-99-10_{st}.txt` (1990s; fixed-width,
  two stacked blocks, SUMLEV 157 county-parts must be summed; slow server —
  download once, cache).

With the key: ACS5 place populations incl. CDPs in one pass
(`B01003_001E for=place:*`), and ZCTA median household income (`B19013_001E`)
+ population for the money analysis.

**City-name matcher** (birth city string → Census place): lowercase, strip
punctuation, strip LSAD suffix (city/town/village/borough/CDP), join on
(state, base name). Resolve the 208 within-state collisions by preferring
incorporated LSAD over CDP; handle "(balance)" consolidated cities; PA/NJ/MN
townships and unincorporated communities won't match — route to a county-level
fallback. **Report the match rate as a first-class number** before any
conclusions; the 2006 paper's suburb blindness is exactly the kind of silent
data artifact we're claiming to fix.

## Phase 2 — High-school supplement (see how the story changes)

- **Modern era, cheap:** Sleeper snapshot (already downloaded). `high_school`
  populated for 10,808 players (96.9% of actives) as school name + state —
  no town. Join via nflverse `sleeper_id` (NOT Sleeper's own `gsis_id`: 32%
  populated, 866 values whitespace-corrupted). Parse state with
  `\(([A-Z]{2})\)$`; ~9% are bare names; filter out 32 team-DEF records.
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
4. No Census key yet → sign up tonight; keyless files unblock the POC.
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
