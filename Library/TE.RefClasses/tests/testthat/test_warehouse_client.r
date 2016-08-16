context("Testing VirtualWarehouseClient")

#########################
#
# VirtualWarehouseClient Tests
#
#########################


test_that("Canot create VirtualWarehouseClient object", {
  expect_error(new("VirtualWarehouseClient"), regexp = "trying to generate an object from a virtual class")
})
