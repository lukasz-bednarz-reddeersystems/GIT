context("Testing PnLTradedInLongShortHedgeAnalysisBlock")

#################################################
#
# ValueTradedInLongShortHedgeAnalysisBlock Tests
#
#################################################

# Generate Pre-requisite Data
if (Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")) {
  strat.brdwn.an <- new("StrategyBreakdownAnalysisBlock")

  valid.key_values <- dated_twelve_monthly_lookback(11, "2016-06-30")
  colnames(valid.key_values) <- c("TraderID", "start", "end")

  strat.brdwn.an <- dataRequest(strat.brdwn.an, valid.key_values)

  trade_data.rd <- getTradeDataObject(strat.brdwn.an)
}

# test vectors
tested.class          <-  "ValueTradedInLongShortHedgeAnalysisBlock"
valid.column_name_map <- hash(c("TraderID", "start", "end"), c("id", "start", "end"))
init.key_values       <- data.frame(TraderID = character(),
                                    start    = as.Date(character()),
                                    end    = as.Date(character()))

test_that(paste("Can create", tested.class, "object"), {
  expect_is(new(tested.class), tested.class)
})



test_that(paste("Can use basic accessors of ", tested.class, "object"), {

  object <- new(tested.class)
  expect_is(object, tested.class)

  expect_is(getTradeDataObject(object), "TradeData")

  expect_is(getOutputGGPlotData(object), "data.frame")
  expect_is(getOutputFrontendData(object), "data.frame")
  expect_is(getOutputObject(object), "ValueTradedData")

  expect_equal(getDataSourceClientColumnNameMap(object), valid.column_name_map)


})


test_that("Cannot setTradeDataObject() with invalid type", {

  object <- new(tested.class)

  expect_error(setTradeDataObject(object, new("PositionData")),
               regexp = "unable to find an inherited method for function")

})


test_that("Can setTradeDataObject() with valid type", {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  object <- setTradeDataObject(object, new("TradeData"))
  expect_is(getTradeDataObject(object), "TradeData")


  object <- setTradeDataObject(object, trade_data.rd)
  expect_is(getTradeDataObject(object), "TradeData")

  # trade data verification
  trade_data <- getTradeDataObject(object)
  expect_is(trade_data, "TradeData")
  expect_gt(nrow(getReferenceData(trade_data)), 0)
  expect_equal(getReferenceData(trade_data),
               getReferenceData(trade_data.rd))

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



test_that("Can dataRequest() with valid key_values with trade data not set", {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  valid.key_values <- dated_twelve_monthly_lookback(11, "2016-06-30")
  colnames(valid.key_values) <- c("TraderID", "start", "end")

  object <- dataRequest(object, valid.key_values)

  expect_equal(getDataSourceQueryKeyValues(object), valid.key_values)

  # trade data verification
  trade_data <- getTradeDataObject(object)
  expect_is(trade_data, "TradeData")
  expect_gt(nrow(getReferenceData(trade_data)), 0)
  expect_equal(getReferenceData(trade_data),
               getReferenceData(trade_data.rd))

})


test_that("Can dataRequest() with valid key_values with trade data set", {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)
  object <- setTradeDataObject(object, trade_data.rd)

  valid.key_values <- dated_twelve_monthly_lookback(11, "2016-06-30")
  colnames(valid.key_values) <- c("TraderID", "start", "end")

  object <- dataRequest(object, valid.key_values)

  expect_equal(getDataSourceQueryKeyValues(object), valid.key_values)

  # trade data verification
  trade_data <- getTradeDataObject(object)
  expect_is(trade_data, "TradeData")
  expect_gt(nrow(getReferenceData(trade_data)), 0)
  expect_equal(getReferenceData(getTradeDataObject(object)),
               getReferenceData(trade_data.rd))


})


test_that(paste("Can Process() on", tested.class), {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  valid.key_values <- dated_twelve_monthly_lookback(11, "2016-06-30")
  colnames(valid.key_values) <- c("TraderID", "start", "end")

  object <- dataRequest(object, valid.key_values)

  expect_equal(getDataSourceQueryKeyValues(object), valid.key_values)

  # trade data verification
  trade_data <- getTradeDataObject(object)
  expect_is(trade_data, "TradeData")
  expect_gt(nrow(getReferenceData(trade_data)), 0)


  object <- Process(object)

  expect_is(getOutputGGPlot(object), "ggplot")
  expect_is(getOutputGGPlotData(object), "data.frame")
  expect_is(getOutputObject(object), "ValueTradedData")

})


