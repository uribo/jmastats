test_that("block no", {
  expect_identical(
    check_block_no(block_no = "47991"),
    "47991"
  )
  expect_warning(
    expect_identical(
      check_block_no(block_no = 0371),
      "0371"
    )
  )
  expect_error(
    check_block_no(999999)
  )
  expect_error(
    check_block_no(47992)
  )
  expect_error(
    check_block_no("47992")
  )
})

test_that("block no is validated against the station list", {
  # Stations added in the 2026-09 dataset refresh (#32)
  expect_identical(check_block_no("1677"), "1677")
  expect_identical(check_block_no("1678"), "1678")
  # Numeric 5-digit input is accepted with a warning and returned as a string
  expect_warning(
    expect_identical(check_block_no(47991), "47991"),
    "assumed to be given as a string"
  )
  # Retired stations are rejected instead of yielding an empty URL
  expect_error(check_block_no("1357"), "not in the station list")
  expect_error(check_block_no("1252"), "not in the station list")
  expect_error(check_block_no("abcd"), "4 or 5 digits")
})
