sourceTo("../common/warehouse_client/warehouse_client.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(testthat)


#########################
#
# DataStoreClient Tests
#
#########################


test_that("Canot create VirtualWarehouseClient object", {
  expect_error(new("VirtualWarehouseClient"), regexp = "trying to generate an object from a virtual class")
})
