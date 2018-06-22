context("test-internals.R")

test_that("failed patterns", {
  expect_false(
    suppressMessages(validate_date(2017, 12, -1)))
  expect_false(
    suppressMessages(validate_date(-2018, 1, 1)))
  expect_false(
    suppressMessages(validate_date(2017, 13, 1))
  )
})

test_that("input arguments validation", {
  expect_true(
    validate_date(2017, 1, 1)
  )
})
