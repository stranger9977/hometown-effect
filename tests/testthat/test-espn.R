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
