sourceTo("../common/rodbc_client/rodbc_client.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(testthat)
#########################
#
# DataStoreClient Tests
#
#########################


test_that("Canot create VirtualDataStoreClient object", {
  expect_error(new("VirtualDataStoreClient"))
})
