# hometown-effect

Where do pros actually come from — and has money changed the answer?

A modern, suburb-corrected re-examination of the "birthplace effect" (Côté,
Macdonald, Baker & Abernethy 2006, *Journal of Sports Sciences*), which found
US pros were 11–21x more likely to come from towns of 50k–100k than from big
cities. That study binned raw birthplace city sizes — so a kid born in a
25,000-person suburb of Dallas counted as a small-town kid, and the authors
never controlled for it.

## What this project does differently

1. **Development location, not just birthplace** — geocode players' high
   schools (NCES school directory) to ZIP/ZCTA, joined to Census ACS data.
2. **Density + income, not city-size bins** — population density and median
   household income of the hometown ZCTA, so the suburb problem disappears
   and the "pay-to-play" money hypothesis becomes testable.
3. **Across sports and across time** — NFL first (deep dive), then
   NBA / MLB / NHL breadth pass; cohorts compared by debut decade.
4. **Relative age effect refresh** — birth-month distributions by sport,
   nearly free once birthdates are loaded.

## Stack

R (nflverse/nflreadr, tidyverse, ggplot2 + sf for US map heatmaps),
Python where it shines (geocoding, census joins). See `docs/` for the design.

## Layout

```
R/               numbered pipeline scripts (data pulls, cleaning, joins)
data/raw/        untouched downloads (gitignored)
data/processed/  parquet/csv intermediates (gitignored)
output/figures/  working chart exports
docs/            GitHub Pages root: index.html report, figures/, specs/
```
