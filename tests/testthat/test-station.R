test_that("block no", {
  expect_identical(
    check_block_no(block_no = "47991"),
    "47991"
  )
  expect_warning(
    expect_identical(
      check_block_no(block_no = 0371),
      "0371"
  ))
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
