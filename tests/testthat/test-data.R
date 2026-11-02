context("test-data.R")

test_that("check stations statement", {
  expect_is(jmastats::stations, c("sf", "tbl_df", "tbl", "data.frame"))
  expect_equal(dim(stations), c(1323L, 14L))
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
    c(2089, 7)
  )
})

test_that("earthquake station", {
  expect_equal(
    dim(earthquake_station),
    c(670, 7)
  )
})

test_that("package functions do not refer to bundled datasets by bare name", {
  # Lazy data is only on the search path when the package is attached, so
  # namespace code must use `jmastats::stations` etc. (#35).
  skip_if_not_installed("codetools")
  dataset_names <- c("stations", "tide_station", "earthquake_station")
  ns <- asNamespace("jmastats")
  function_names <- Filter(
    function(name) is.function(get(name, envir = ns, inherits = FALSE)),
    ls(ns, all.names = TRUE)
  )
  offenders <- lapply(
    stats::setNames(function_names, function_names),
    function(name) {
      globals <- codetools::findGlobals(
        get(name, envir = ns, inherits = FALSE),
        merge = FALSE
      )
      intersect(globals$variables, dataset_names)
    }
  )
  offenders <- offenders[lengths(offenders) > 0L]
  expect_length(offenders, 0L)
})
