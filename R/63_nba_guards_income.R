# =============================================================================
# 63_nba_guards_income.R -- Michael's question: in basketball, do GUARDS (point
# guards, shooting guards) come from wealthier hometowns than POSTS (centers,
# power forwards)? His theory: guard play is more skill/training dependent, so
# money buys more of it, while size (posts) is luck of the draw.
#
# The birthplace pages we cached (data/raw/bbref) carry each player's slug, name,
# birth city, and birth date, but NO position. Positions come from the season
# totals pages (NBA_YYYY_totals.html), which list every player-season with a Pos
# column and the same player slug. We join the two by slug, then run R/15's
# hometown income-quartile match. Primary position = the position the player
# logged the most games at across their career.
# HONEST OR BUST: report whichever way the data goes.
# =============================================================================

suppressMessages({
  library(dplyr); library(arrow); library(rvest); library(xml2); library(curl)
  library(ggplot2); library(tidyr); library(stringr); library(purrr)
})
source("R/lib/bins.R")
source("R/lib/places.R")
source("R/lib/theme_hometown.R")

UA <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
tot_dir <- "data/raw/bbref_totals"
dir.create(tot_dir, showWarnings = FALSE, recursive = TRUE)
SEASONS <- 1955:2025   # position data is reliable from the mid-1950s on
SLEEP_SECS <- 3.2      # politeness, matches R/13

tot_url  <- function(y) sprintf("https://www.basketball-reference.com/leagues/NBA_%d_totals.html", y)
tot_path <- function(y) file.path(tot_dir, sprintf("%d.html", y))

fetch_one <- function(url) {
  h <- curl::new_handle(useragent = UA, followlocation = TRUE, timeout = 30)
  res <- tryCatch(curl::curl_fetch_memory(url, handle = h), error = function(e) NULL)
  if (is.null(res)) return(list(status = -1L, body = ""))
  list(status = res$status_code, body = rawToChar(res$content))
}
valid <- function(status, body) status == 200L && grepl("data-stat=\"pos\"", body, fixed = TRUE)

for (y in SEASONS) {
  p <- tot_path(y)
  if (file.exists(p) && valid(200L, readr::read_file(p))) next
  r <- fetch_one(tot_url(y))
  if (valid(r$status, r$body)) { writeLines(r$body, p); cat(sprintf("fetched %d\n", y)) }
  else cat(sprintf("skip %d (status %d)\n", y, r$status))
  Sys.sleep(SLEEP_SECS)
}

# --- slug -> primary position (most games) ----------------------------------
parse_totals <- function(y) {
  p <- tot_path(y); if (!file.exists(p)) return(NULL)
  pg <- read_html(p)
  rows <- html_elements(pg, "table#totals_stats tbody tr:not(.thead)")
  if (length(rows) == 0) rows <- html_elements(pg, "table#totals tbody tr:not(.thead)")
  if (length(rows) == 0) return(NULL)
  # bbref's current schema: player column is data-stat='name_display' with the
  # player slug in data-append-csv; games is data-stat='games'.
  slug <- rows |> html_element("[data-stat='name_display']") |> html_attr("data-append-csv")
  pos  <- rows |> html_element("[data-stat='pos']")          |> html_text2()
  g    <- rows |> html_element("[data-stat='games']")        |> html_text2() |> as.integer()
  tibble(slug, pos, g) |> filter(!is.na(slug), !is.na(pos), pos != "")
}
totals <- map(SEASONS, parse_totals) |> list_rbind()

# primary bbref position = first listed token, games-weighted across career
prim_pos <- totals |>
  mutate(pos1 = str_extract(pos, "^[A-Z]+"),
         g = coalesce(g, 0L)) |>
  filter(pos1 %in% c("PG","SG","SF","PF","C")) |>
  group_by(slug, pos1) |> summarise(g = sum(g), .groups = "drop") |>
  group_by(slug) |> slice_max(g, n = 1, with_ties = FALSE) |> ungroup() |>
  mutate(role = case_when(pos1 %in% c("PG","SG") ~ "Guard",
                          pos1 %in% c("C","PF")  ~ "Post",
                          TRUE                    ~ "Wing")) |>
  select(slug, pos1, role)

# --- slug <-> (name, state, birth_date) from cached birthplace pages ---------
states <- c(datasets::state.abb, "DC")
parse_bp <- function(st) {
  p <- file.path("data/raw/bbref", paste0(st, ".html")); if (!file.exists(p)) return(NULL)
  pg <- read_html(p)
  rows <- html_elements(pg, "table#stats tbody tr:not(.thead)")
  if (length(rows) == 0) return(NULL)
  slug <- rows |> html_element("td[data-stat='player'] a") |> html_attr("href") |>
    str_extract("/players/[a-z]/([a-z0-9]+)\\.html", group = 1)
  name <- rows |> html_element("td[data-stat='player']") |> html_text2() |> (\(x) sub("[*]$","",x))()
  bdate<- rows |> html_element("td[data-stat='birth_date']") |> html_attr("csk")
  tibble(slug, player_name = name, birth_state = st, birth_date = bdate)
}
bp <- map(states, parse_bp) |> list_rbind() |> filter(!is.na(slug))

# --- attach position to players_nba, then run the income match ---------------
nba <- read_parquet("data/processed/players_nba.parquet") |>
  filter(birth_country == "USA", birth_state %in% states, !is.na(era)) |>
  left_join(bp |> select(slug, player_name, birth_state, birth_date),
            by = c("player_name","birth_state","birth_date")) |>
  left_join(prim_pos, by = "slug") |>
  filter(!is.na(role), role %in% c("Guard","Post"))

places <- read_parquet("data/processed/census_places.parquet")
wq <- function(income, pop) {
  ok <- !is.na(income) & !is.na(pop) & pop > 0
  o <- order(income[ok]); cw <- cumsum(as.numeric(pop[ok][o])) / sum(as.numeric(pop[ok][o]))
  list(income = income[ok][o], cw = cw)
}
cuts <- function(w) sapply(c(.25,.5,.75), function(q) w$income[which.max(w$cw >= q)])
cut99  <- cuts(wq(places$income1999, places$pop2000))
cutnow <- cuts(wq(places$income_now,  places$pop_now))

matched <- match_places(nba, places) |>
  filter(match_tier != "unmatched") |>
  mutate(vintage = if_else(era %in% c("1990s","2000s"), "1999", "now"),
         income  = if_else(vintage == "1999", matched_income1999, matched_income_now),
         q = case_when(is.na(income) ~ NA_integer_,
                       vintage == "1999" ~ findInterval(income, cut99) + 1L,
                       TRUE              ~ findInterval(income, cutnow) + 1L)) |>
  filter(!is.na(q))

dist <- matched |> count(role, q) |> group_by(role) |> mutate(share = n/sum(n)) |> ungroup()
summ <- matched |> group_by(role) |>
  summarise(n = n(), med_income = median(income, na.rm = TRUE),
            q4 = mean(q == 4), q1 = mean(q == 1), .groups = "drop")
cat("=== NBA guards vs posts, hometown income (US-born, 1990+ debut) ===\n")
print(as.data.frame(summ))
g_q4 <- summ$q4[summ$role=="Guard"]; p_q4 <- summ$q4[summ$role=="Post"]
cat(sprintf("\nRichest-quartile hometown share: guards %.1f%%, posts %.1f%% (gap %+.1f pts)\n",
            100*g_q4, 100*p_q4, 100*(g_q4-p_q4)))
cat(sprintf("Median hometown income: guards $%s, posts $%s\n",
            format(round(summ$med_income[summ$role=="Guard"]), big.mark=","),
            format(round(summ$med_income[summ$role=="Post"]), big.mark=",")))

# HONEST OR BUST headline set after the run (see below).
cat("\ndone-parse\n")

# --- chart ------------------------------------------------------------------
diff_pts <- 100*(g_q4 - p_q4)
verdict <- if (diff_pts >= 1.5) {
  "do come from richer hometowns than posts, matching the hunch"
} else if (diff_pts <= -1.5) {
  "come from poorer hometowns than posts, against the hunch"
} else { "come from about the same hometowns as posts, not the split the hunch predicted" }
title <- if (diff_pts >= 1.5) {
  "In basketball, guards come from richer hometowns than posts, just as Michael guessed"
} else if (diff_pts <= -1.5) {
  "In basketball, guards come from poorer hometowns than posts, the opposite of the hunch"
} else {
  "In basketball, guards and posts come from about the same hometown income mix"
}

qlab <- c("Poorest\nquartile","2nd","3rd","Richest\nquartile")
plotdf <- dist |> mutate(qf = factor(q, levels=1:4, labels=qlab),
                         role = factor(role, levels=c("Post","Guard")))
pal_role <- c(Post = "#009E73", Guard = "#CC79A7")
pj <- ggplot(plotdf, aes(qf, share, fill = role)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.66) +
  geom_hline(yintercept = 0.25, linetype = "22", colour = ink_baseline, linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.0f%%", 100*share)),
            position = position_dodge(width = 0.72), vjust = -0.5,
            size = 3.1, fontface = "bold", colour = ink_body) +
  scale_fill_manual(values = pal_role, name = NULL) +
  scale_y_continuous(labels = function(x) paste0(round(100*x), "%"),
                     expand = expansion(mult = c(0, 0.12))) +
  labs(
    title = title,
    subtitle = "Hometown income quartile of US-born NBA players (1990 or later debut), guards versus posts. Dashed line: even 25%.",
    x = NULL, y = "Share of that role's players",
    caption = fig_caption(
      "Basketball-Reference (birthplaces + season totals for position) + US Census place income; same income-quartile method as our cross-sport charts",
      sprintf("\nRole is each player's primary career position (most games): guard = PG/SG, post = C/PF (wings/SF excluded). US-born players with a matched\nhometown, 1990+ debut (guards n=%d, posts n=%d). Income vintage: 2000 census for 1990s/2000s debuts, recent ACS for 2010s/2020s.",
              summ$n[summ$role=="Guard"], summ$n[summ$role=="Post"]),
      sprintf("\nMichael's hunch was that guards, being more skill and training dependent, come from wealthier hometowns than posts. The distributions are\nnearly identical: about %.0f%% of each come from a richest-quartile hometown, and roughly two-thirds of both come from the poorest two quartiles.\nGuards' median hometown income is modestly higher ($%s vs $%s), a faint tilt toward the hunch, but not the clear split it predicted.",
              100*g_q4,
              format(round(summ$med_income[summ$role=="Guard"]), big.mark=","),
              format(round(summ$med_income[summ$role=="Post"]), big.mark=",")))) +
  theme_hometown(grid = "y") +
  theme(legend.position = "top", legend.justification = "left",
        legend.key.size = unit(11, "pt"), legend.text = element_text(size = rel(0.85)))
save_fig("docs/figures/ba_nba_guards_income.png", pj, w = 11, h = 5.8)
cat("wrote chart\n")
