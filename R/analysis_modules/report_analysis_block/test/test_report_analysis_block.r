sourceTo("../analysis_modules/report_analysis_block/report_analysis_block.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(testthat)

#########################
#
# VirtualReportAnalysisBlock Tests
#
#########################
tested.class          <-  "VirtualReportAnalysisBlock"

test_that(paste("Cannot create", tested.class, "object"), {
  expect_error(new(tested.class), regexp = "trying to generate an object from a virtual class")
})