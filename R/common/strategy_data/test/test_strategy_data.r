sourceTo("../common/strategy_data/strategy_data.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(testthat)
library(plyr)


tested.class          <-  "VirtualStrategyData"

test_that(paste("Canot create", tested.class, "object"), {
  expect_error(new(tested.class), regexp = "trying to generate an object from a virtual class")
})