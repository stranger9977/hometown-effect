suppressMessages(library(dplyr))
source(file.path(testthat::test_path(), "..", "..", "R", "lib", "schools.R"))

test_that("parse_sleeper_hs handles paren-state, bare, and foreign formats", {
  out <- parse_sleeper_hs(c("Whitehouse (TX)", "Central", "St. Patrick's (Ottawa, CAN)"))
  expect_equal(out$hs_name, c("Whitehouse", "Central", "St. Patrick's"))
  expect_equal(out$hs_state, c("TX", NA, NA))   # non-US-state parens -> NA state
})

test_that("match_schools finds unique in-state name matches", {
  nces <- tibble(name = c("Whitehouse High School", "Central High School",
                          "Central High School"),
                 city = c("Whitehouse", "Springfield", "Shelbyville"),
                 state = c("TX", "IL", "IL"),
                 zip = c("75791", "62701", "62565"),
                 lat = c(32.22, 39.8, 39.4), lon = c(-95.2, -89.6, -88.8))
  hs <- tibble(hs_name = c("Whitehouse", "Central"), hs_state = c("TX", "IL"))
  out <- match_schools(hs, nces)
  expect_true(out$school_match[1])
  expect_equal(out$nces_city[1], "Whitehouse")
  expect_false(out$school_match[2])   # ambiguous within IL -> unmatched
})
