suppressMessages(library(dplyr))
source(file.path(testthat::test_path(), "..", "..", "R", "lib", "places.R"))

test_that("normalize_city handles saint, punctuation, and spacing variants", {
  expect_equal(normalize_city("St. Louis"), "saintlouis")
  expect_equal(normalize_city("Winston-Salem"), "winstonsalem")
  expect_equal(normalize_city("O'Fallon"), "ofallon")
  expect_equal(normalize_city("  Tyler "), "tyler")
  # spacing variants must collide: ESPN "La Grange" vs Census "LaGrange city"
  expect_equal(normalize_city("La Grange"), normalize_city("LaGrange"))
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

consolidated_places <- function() {
  tibble(
    state = c("TN", "NY", "HI", "NC"),
    geoid = c("4752006", "3651000", "1571550", "3775000"),
    name_raw = c("Nashville-Davidson metropolitan government (balance)",
                 "New York city", "Urban Honolulu CDP", "Winston-Salem city"),
    lsad = c("00", "25", "57", "25"),
    aland_sqmi = c(475.9, 300.4, 60.5, 132.4),
    lat = c(36.17, 40.66, 21.32, 36.10),
    lon = c(-86.78, -73.94, -157.80, -80.26),
    pop2000 = c(545524L, 8008278L, 371657L, 185776L),
    pop2010 = c(601222L, 8175133L, 337256L, 229617L),
    pop_now = c(689447L, 8258035L, 343421L, 249545L),
    income1999 = c(39232L, 38293L, 45112L, 37380L),
    income_now = c(62000L, 76000L, 78000L, 55000L))
}

test_that("consolidated governments match their common city name", {
  players <- tibble(birth_city = c("Nashville", "Winston-Salem"),
                    birth_state = c("TN", "NC"))
  out <- match_places(players, consolidated_places())
  expect_equal(out$geoid, c("4752006", "3775000"))
  expect_false(any(out$match_tier == "unmatched"))
})

test_that("borough and known-alias cities map to their census place", {
  players <- tibble(
    birth_city = c("Brooklyn", "Staten Island", "Honolulu"),
    birth_state = c("NY", "NY", "HI"))
  out <- match_places(players, consolidated_places())
  expect_equal(out$geoid, c("3651000", "3651000", "1571550"))
})
