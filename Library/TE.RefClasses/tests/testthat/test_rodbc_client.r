context("Testing VirtualRODBCClient")
#########################
#
# VirtualRODBCClient Tests
#
#########################


test_that("Canot create VirtualRODBCClient object", {
  expect_error(new("VirtualRODBCClient"),
               regexp = "trying to generate an object from a virtual")
})
