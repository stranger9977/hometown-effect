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
