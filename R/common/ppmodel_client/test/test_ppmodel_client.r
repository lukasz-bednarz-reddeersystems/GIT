sourceTo("../common/ppmodel_client/ppmodel_client.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(testthat)


#########################
#
# VirtualPPModelClient Tests
#
#########################


test_that("Canot create VirtualPPModelClient object", {
  expect_error(new("VirtualPPModelClient"), regexp = "trying to generate an object from a virtual class")
})
