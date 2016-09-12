context("Testing StrategyBreakdownValueTradedPerSignalAnalysisBlock")


##########################################################
#
# StrategyBreakdownValueTradedPerSignalAnalysisBlock Tests
#
##########################################################
# get required data
if (Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")) {
  strat.brdwn.an <- new("StrategyBreakdownAnalysisBlock")

  valid.key_values <- dated_twelve_monthly_lookback(11, "2016-06-30")
  colnames(valid.key_values) <- c("TraderID", "start", "end")

  strat.brdwn.an <- dataRequest(strat.brdwn.an, valid.key_values)
  trade.data.rd <- getTradeDataObject(strat.brdwn.an)
  trade.data.df <- getReferenceData(trade.data.rd)
}
# test vectors
tested.class <- "StrategyBreakdownValueTradedPerSignalAnalysisBlock"
valid.required_cols <- c(strategy_breakdown_per_signal_base_cols,
                         strategy_breakdown_per_signal_signal_cols)
init.key_values <- data.frame(TraderID = character(),
                              start    = as.Date(character()),
                              end    = as.Date(character()))

test_that(paste("Can create", tested.class, "object"), {
  expect_is(new(tested.class), tested.class)
})



test_that(paste("Can use basic accessors of ", tested.class, "object"), {

  object <- new(tested.class)
  expect_is(object, tested.class)

  expect_is(getTradeDataObject(object), "TradeData")
  expect_is(getEventDataObject(object), "EventData")

  expect_is(getOutputGGPlotData(object), "data.frame")
  expect_is(getOutputFrontendData(object), "data.frame")

  expect_is(getOutputObject(object), "TradedSignalsData")

  expect_equal(getRequiredVariablesNames(object), valid.required_cols)

})


test_that(paste("Cannot set Trade Data with invalid input on ", tested.class, "object"), {

  object <- new(tested.class)
  expect_is(object, tested.class)

  expect_error(setTradeDataObject(object, new("EventData")),
               regexp = "unable to find an inherited method for function")

  expect_is(getTradeDataObject(object), "TradeData")

})

test_that(paste("Can set Trade Data with valid input on ", tested.class, "object"), {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)
  expect_is(object, tested.class)

  object <- setTradeDataObject(object, new("TradeData"))
  expect_is(getTradeDataObject(object), "TradeData")

  object <- setTradeDataObject(object, trade.data.rd)
  expect_is(getTradeDataObject(object), "TradeData")
  expect_equal(getTradeDataObject(object), trade.data.rd)
  expect_gt(getStoredNRows(getTradeDataObject(object)),0)

})

test_that("Cannot dataRequest() with invalid key_values", {

  object <- new(tested.class)


  invalid.key_values <- data.frame(TraderID = integer(),
                                   start = as.Date(character()),
                                   end = as.Date(character()))

  expect_error(dataRequest(object, invalid.key_values),
               regexp = "Zero row query keys data.frame passed")

  invalid.key_values <- data.frame(lC = numeric(), dtD = as.Date(character()))

  expect_error(dataRequest(object, invalid.key_values),
               regexp = "Invalid column names of query keys passed")


  expect_equal(getDataSourceQueryKeyValues(object), init.key_values)

})


test_that("Can dataRequest() with valid key_values with Trade Data set", {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  valid.key_values <- dated_twelve_monthly_lookback(11, "2016-06-30")
  colnames(valid.key_values) <- c("TraderID", "start", "end")

  object <- setTradeDataObject(object, new("TradeData"))
  expect_is(getTradeDataObject(object), "TradeData")

  object <- dataRequest(object, valid.key_values)

  expect_equal(getDataSourceQueryKeyValues(object), valid.key_values)

  # trade data verification
  trade_data <- getTradeDataObject(object)
  expect_is(trade_data, "TradeData")
  expect_gt(getStoredNRows(trade_data), 0)
  expect_equal(getReferenceData(trade_data), trade.data.df)

  # event data verification
  event_data <- getEventDataObject(object)
  expect_is(event_data, "EventData")
  expect_gt(getStoredNRows(event_data), 0)

})


test_that("Can dataRequest() with valid key_values without Trade Data set", {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  valid.key_values <- dated_twelve_monthly_lookback(11, "2016-06-30")
  colnames(valid.key_values) <- c("TraderID", "start", "end")



  object <- dataRequest(object, valid.key_values)

  expect_equal(getDataSourceQueryKeyValues(object), valid.key_values)

  # trade data verification
  trade_data <- getTradeDataObject(object)
  expect_is(trade_data, "TradeData")
  expect_gt(getStoredNRows(trade_data), 0)
  expect_equal(getReferenceData(trade_data), trade.data.df)

  # event data verification
  event_data <- getEventDataObject(object)
  expect_is(event_data, "EventData")
  expect_gt(getStoredNRows(event_data), 0)

})

test_that(paste("Can Run Process() on", tested.class), {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  valid.key_values <- dated_twelve_monthly_lookback(11, "2016-06-30")
  colnames(valid.key_values) <- c("TraderID", "start", "end")

  # trade data verification
  object <- setTradeDataObject(object, trade.data.rd)
  expect_is(getTradeDataObject(object), "TradeData")
  expect_equal(getTradeDataObject(object), trade.data.rd)

  trade_data <- getTradeDataObject(object)
  expect_is(trade_data, "TradeData")
  expect_gt(getStoredNRows(trade_data), 0)
  expect_equal(getReferenceData(trade_data), trade.data.df)

  object <- dataRequest(object, valid.key_values)


  object <- Process(object)

  expect_is(getOutputGGPlot(object), "ggplot")
  expect_is(getOutputGGPlotData(object), "data.frame")

})


