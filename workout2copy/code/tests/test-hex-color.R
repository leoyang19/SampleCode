context("Test for is-hex-color")
test_that("is_hex() works as expected", {
  expect_true(is_hex("#FF00A7"))
  expect_true(is_hex("#ff0000"))
  expect_true(is_hex("#123456"))
  expect_true(is_hex("#12Fb56"))
  expect_false(is_hex("FF0000"))
  expect_false(is_hex("#1234GF"))
  expect_false(is_hex("#1234567"))
  expect_false(is_hex("blue"))
  expect_error(is_hex(FF00A7))
  expect_error(is_hex(TRUE))
})



test_that("is_hex_alpha() works as expected", {
  expect_true(is_hex_alpha("#FF000078"))
  expect_true(is_hex_alpha("#ffda0078"))
  expect_false(is_hex_alpha("#FF0000"))
  4
  expect_false(is_hex_alpha("#ffda00"))
  expect_error(is_hex_alpha(FF00A7))
  expect_error(is_hex_alpha(TRUE))
})
