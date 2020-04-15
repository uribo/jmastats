test_that("", {
  expect_error(
    read_tide_level(.year = 1997, .month = 3, .stn = "TK")
  )
  expect_equal(
    request_tide_level_url(.year = 1997, .month = 4, .stn = "TK"),
    "https://www.data.jma.go.jp/gmd/kaiyou/data/db/tide/genbo/1997/199704/hry199704TK.txt"
  )
})
