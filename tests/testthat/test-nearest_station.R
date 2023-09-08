context("test-nearest_station.R")

test_that("multiplication works", {

  skip_if_not_installed("lwgeom")
  col_vars <- c("area", "station_no", "station_name", "block_no", "distance", "geometry")
  res <-
    nearest_station(142.9313, 43.70417)
  expect_s3_class(
    res,
    "sf"
  )
  expect_equal(
    dim(res),
    c(1, 6)
  )
  expect_named(
    res,
    col_vars
  )
  expect_equal(
    res$station_no,
    12471L
  )

  expect_equal(
    nrow(nearest_station(longitude = 141.6084, latitude = 40.50878)),
    nrow(dplyr::filter(stations, station_no == 31602))-1L
  )

  res <-
    nearest_station(longitude = NULL,
                    latitude  = NULL,
                    geometry  = sf::st_point(c(131.533333333, 34.04)))
  expect_equal(
    res$station_no,
    81371L
  )

  res <-
    pick_neighbor_stations(longitude = 140.10,
                           latitude = 36.08,
                           distance = 3500)
  expect_equal(dim(res), c(1, 6))
  expect_s3_class(
    res,
    "sf"
  )
  expect_s3_class(
    res,
    "tbl_df"
  )
  expect_named(
    res,
    col_vars
  )
  expect_identical(
    res,
    pick_neighbor_stations(longitude = 140.10,
                           latitude = 36.08,
                           distance = 3.5, .unit = "km")
  )
  expect_equal(
    nrow(pick_neighbor_stations(longitude = 140.10,
                           latitude = 36.08,
                           distance = 6,
                           .unit = "km")
               ),
    1L)
})

test_that("failed", {
  skip_if_not_installed("lwgeom")
  expect_message(
    nearest_station(longitude = 60, latitude = 30.4)
  )
})
