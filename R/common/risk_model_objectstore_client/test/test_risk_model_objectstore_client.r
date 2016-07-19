sourceTo("../common/risk_model_objectstore_client/risk_model_objectstore_client.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(testthat)

#########################################
#
# VirtualRiskModelObjectstoreClient Tests
#
#########################################


test_that("Canot create VirtualRiskModelObjectstoreClient object", {
  expect_error(new("VirtualRiskModelObjectstoreClient"))
})
