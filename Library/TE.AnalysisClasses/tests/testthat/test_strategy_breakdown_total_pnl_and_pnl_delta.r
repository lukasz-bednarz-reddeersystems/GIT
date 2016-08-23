context("Testing StrategyBreakdownTotalAndDeltaPnLAnalysisBlock")

####################################################
#
# StrategyBreakdownTotalAndDeltaPnLAnalysisBlock Tests
#
####################################################
# get required data

if (Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")) {
  strat.brdwn.an <- new("StrategyBreakdownAnalysisBlock")

  valid.key_values <- dated_twelve_monthly_lookback(11, "2016-06-30")
  colnames(valid.key_values) <- c("TraderID", "start", "end")

  strat.brdwn.an <- dataRequest(strat.brdwn.an, valid.key_values)
  strat.brdwn.an <- Process(strat.brdwn.an)

  strat.brdwn.rd <- getOutputObject(strat.brdwn.an)
}

# test vectors
tested.class <- "StrategyBreakdownTotalAndDeltaPnLAnalysisBlock"
valid.required_cols <- c("Type", "Quantity", "Value", "Strategy", "Quarter" )

test_that(paste("Can create", tested.class, "object"), {
  expect_is(new(tested.class), tested.class)
})



test_that(paste("Can use basic accessors of ", tested.class, "object"), {

  object <- new(tested.class)
  expect_is(object, tested.class)

  expect_is(getStrategyDataObject(object), "StrategyBreakDownData")

  expect_is(getOutputGGPlotData(object), "data.frame")
  expect_is(getOutputFrontendData(object), "data.frame")

  expect_equal(getRequiredVariablesNames(object), valid.required_cols)

})


test_that(paste("Cannot set Strategy Data with invalid input on ", tested.class, "object"), {

  object <- new(tested.class)
  expect_is(object, tested.class)

  expect_error(setStrategyDataObject(object, new("TradeData")),
               regexp = "unable to find an inherited method for function")

  expect_is(getStrategyDataObject(object), "StrategyBreakDownData")

})

test_that(paste("Can set Strategy Data with valid input on ", tested.class, "object"), {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)
  expect_is(object, tested.class)

  object <- setStrategyDataObject(object, new("StrategyBreakDownData"))
  expect_is(getStrategyDataObject(object), "StrategyBreakDownData")

  object <- setStrategyDataObject(object, strat.brdwn.rd)
  expect_is(getStrategyDataObject(object), "StrategyBreakDownData")
  expect_equal(getStrategyDataObject(object), strat.brdwn.rd)
  expect_gt(getStoredNRows(getStrategyDataObject(object)),0)

})


test_that(paste("Can Run Process() on", tested.class), {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)


  # strategy data verification
  object <- setStrategyDataObject(object, strat.brdwn.rd)
  expect_is(getStrategyDataObject(object), "StrategyBreakDownData")
  expect_equal(getStrategyDataObject(object), strat.brdwn.rd)


  object <- Process(object)

  expect_is(getOutputGGPlot(object), "ggplot")
  expect_is(getOutputGGPlotData(object), "data.frame")

})


