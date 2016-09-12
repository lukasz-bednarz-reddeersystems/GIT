
#########################
#
# VirtualPPModelClient Tests
#
#########################


test_that("Canot create VirtualPPModelClient object", {
  expect_error(new("VirtualPPModelClient"), regexp = "trying to generate an object from a virtual class")
})
