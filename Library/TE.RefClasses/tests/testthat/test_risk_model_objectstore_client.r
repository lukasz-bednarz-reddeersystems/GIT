context("Testing Risk Model Objectstore")

#########################################
#
# VirtualRiskModelObjectstoreClient Tests
#
#########################################


test_that("Canot create VirtualRiskModelObjectstoreClient object", {
  expect_error(new("VirtualRiskModelObjectstoreClient"),
               regexp = "trying to generate an object from a virtual class")
})
