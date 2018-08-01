context("test-palette.R")

test_that("Regular works", {

  res <-
    jma_pal(palette = "absolute")
  expect_is(res, "character")
  expect_length(res, 8L)

  res <-
    jma_pal(palette = "absolute", .attribute = TRUE)
  expect_is(res, "list")
  expect_named(
    res,
    c("colors", "precipitation", "snow", "wind")
  )
  expect_length(unlist(res), 50L)

  res <-
    jma_pal(palette = "relative", .attribute = TRUE)
  expect_is(res, "list")
  expect_named(
    res,
    c("colors", "amedas", "forecast")
  )
  expect_length(unlist(res), 52L)

})

test_that("Failed Patterns", {

  expect_error(
    jma_pal(palette = "soutai")
  )

})
