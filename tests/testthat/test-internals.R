context("test-internals.R")

test_that("failed patterns", {
  expect_false(
    suppressMessages(validate_date(2017, 12, -1)))
  expect_false(
    suppressMessages(validate_date(-2018, 1, 1)))
  expect_false(
    suppressMessages(validate_date(2017, 13, 1))
  )
  expect_message(
    jma_url(item = "annually", "0010", year = 2017, month = 13),
    "Input arguments should be calender format\nCheck: month"
  )
})

test_that("input arguments validation", {
  expect_true(
    validate_date(2017, 1, 1)
  )
})

test_that("multiplication works", {
  x <-
    jma_url(item = "daily",
            block_no = "0422",
            year = 2017,
            month = 6)
  expect_is(x, "list")
  expect_named(x, c("url", "station_type"))
  expect_equal(
    x,
    list(
      url = "https://www.data.jma.go.jp/obd/stats/etrn/view/daily_a1.php?prec_no=48&block_no=0422&year=2017&month=6&day=&view=", # nolint
      station_type = "a1")
  )
  x <-
    jma_url("annually",
            "0010",
            2017,
            12)
  expect_equal(
    x,
    list(
      url = "https://www.data.jma.go.jp/obd/stats/etrn/view/annually_a.php?prec_no=12&block_no=0010&year=2017&month=12&day=&view=", # nolint
      station_type = "a")
  )
})

test_that("Set coords", {

  res <-
    check_input_coords(longitude = 140.10, latitude = 36.08, geometry = NULL)
  expect_is(res, "list")
  expect_named(res, c("longitude", "latitude"))

  expect_equivalent(
    check_input_coords(geometry = st_point(c(140.10, 36.08))),
    check_input_coords(longitude = 140.10, latitude = 36.08, geometry = NULL))

  expect_message(
    check_input_coords(longitude = 123.00, latitude = 32.0, geometry = st_point(c(140.10, 36.08)))
  )

})
