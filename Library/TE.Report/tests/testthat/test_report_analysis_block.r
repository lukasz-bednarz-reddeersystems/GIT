context("Testing VirtualReportAnalysisBlock")

##################################
#
# VirtualReportAnalysisBlock Tests
#
##################################
tested.class          <-  "VirtualReportAnalysisBlock"

test_that(paste("Cannot create", tested.class, "object"), {
  expect_error(new(tested.class), regexp = "trying to generate an object from a virtual class")
})
