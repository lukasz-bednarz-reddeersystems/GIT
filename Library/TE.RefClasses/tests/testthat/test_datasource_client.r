context("Test VirtualDataSourceClient")

#########################
#
# VirtualDataSourceClient Tests
#
#########################


test_that("Canot create VirtualDataSourceClient object", {
  expect_error(new("VirtualDataSourceClient"),
               regexp = "trying to generate an object from a virtual class")
})

