source(file.path(testthat::test_path(), "..", "..", "R", "lib", "bins.R"))

test_that("era_cohort maps rookie seasons to cohorts", {
  expect_equal(era_cohort(c(1990, 1999, 2000, 2013, 2020, 2025)),
               c("1990s", "1990s", "2000s", "2010s", "2020s", "2020s"))
  expect_true(is.na(era_cohort(1989)))
  expect_true(is.na(era_cohort(NA)))
})

test_that("cote_bin buckets populations", {
  expect_equal(as.character(cote_bin(c(1000, 60000, 5e6))),
               c("<2.5k", "50k–100k", "500k+"))
  expect_true(is.na(cote_bin(NA)))
  expect_true(is.ordered(cote_bin(1000)))
})

test_that("vintage_pop picks the population column matching each row's era", {
  df <- tibble::tibble(
    era             = c("1990s", "2000s", "2010s", "2020s", NA_character_),
    matched_pop2000 = c(100,     200,     NA,       NA,      NA),
    matched_pop2010 = c(NA,      NA,      300,      NA,      NA),
    matched_pop_now = c(NA,      NA,      NA,       400,     NA))
  out <- vintage_pop(df)
  expect_equal(out$pop, c(100, 200, 300, 400, NA))
})
