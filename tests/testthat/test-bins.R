source(file.path(testthat::test_path(), "..", "..", "R", "lib", "bins.R"))

test_that("era_cohort maps rookie seasons to cohorts", {
  expect_equal(era_cohort(c(1990, 1999, 2000, 2013, 2020, 2025)),
               c("1990s", "1990s", "2000s", "2010s", "2020s", "2020s"))
  expect_true(is.na(era_cohort(1989)))
  expect_true(is.na(era_cohort(NA)))
})
