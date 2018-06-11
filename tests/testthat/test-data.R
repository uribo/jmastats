context("test-data.R")

# sealr::transcribe(stations, detail = FALSE, load_testthat = FALSE, ts = FALSE, mask_seal = TRUE)
test_that("check stations statement", {
  expect_is(stations,
            c("sf", "tbl_df", "tbl", "data.frame"))
  expect_equal(dim(stations),
               c(1334L, 13L))
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
      "geometry"
    )
  )
  expect_equal(
    stations %>% purrr::map(class) %>% unname(),
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
      c("sfc_POINT", "sfc")
    )
  )
})