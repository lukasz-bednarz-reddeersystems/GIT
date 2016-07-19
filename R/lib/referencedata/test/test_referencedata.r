sourceTo("../lib/referencedata/referencedata.R", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(testthat)

test_that("Cannot Create VirtualReferenceData class", {
          expect_error(new("VirtualReferenceData"))
})