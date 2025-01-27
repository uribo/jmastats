context("test-data.R")

test_that("check stations statement", {
  expect_is(stations,
            c("sf", "tbl_df", "tbl", "data.frame"))
  expect_equal(dim(stations),
               c(1323L, 14L))
  expect_named(
    stations,
    c(
      "area",
      "station_no",
      "station_type",
      "station_name",
      "address",
      "elevation",
      "observation_begin",
      "note1",
      "note2",
      "katakana",
      "prec_no",
      "block_no",
      "pref_code",
      "geometry"
    )
  )
  expect_equal(
    stations |>
      purrr::map(class) |>
      unname(),
    list(
      "character",
      "integer",
      "character",
      "character",
      "character",
      "integer",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character",
      "character",
      c("sfc_POINT", "sfc")
    )
  )
})

test_that("tidal station", {
  expect_equal(
    dim(tide_station),
    c(2019, 7)
  )
})

test_that("earthquake station", {
  expect_equal(
    dim(earthquake_station),
    c(671, 7)
  )
})
