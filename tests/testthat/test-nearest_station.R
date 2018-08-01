context("test-nearest_station.R")

test_that("multiplication works", {

  skip_if_not_installed("lwgeom")
  res <-
    nearest_station(142.9313, 43.70417)
  expect_s3_class(
    res,
    "sf"
  )
  expect_equal(
    dim(res),
    c(1, 5)
  )
  expect_named(
    res,
    c("area", "station_no", "station_name", "distance", "geometry")
  )
  expect_equal(
    res$station_no,
    12471L
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
                           distance = 300000)
  expect_s3_class(
    res,
    "sf"
  )
  expect_s3_class(
    res,
    "tbl_df"
  )

  expect_equal(
    nrow(pick_neighbor_stations(longitude = 140.10,
                           latitude = 36.08,
                           distance = 6,
                           .unit = "km")
               ),
    1L)
})
