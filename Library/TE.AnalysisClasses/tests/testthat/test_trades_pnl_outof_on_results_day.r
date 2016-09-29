context("Testing TradesPnLOutOfOnResultsDayAnalysisBlock")

#####################################
#
# TradesPnLOutOfOnResultsDayAnalysisBlock Tests
#
#####################################

tested.class          <-  "TradesPnLOutOfOnResultsDayAnalysisBlock"
valid.column_name_map <- hash(c("TraderID", "start", "end"), c("id", "start", "end"))
init.key_values       <- data.frame(TraderID = character(),
                                    start    = as.Date(character()),
                                    end    = as.Date(character()))



if(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")) {
  valid.key_values <- dated_twelve_monthly_lookback(11, today())
  colnames(valid.key_values) <- c("TraderID", "start", "end")
  trades_on_numbers.an <- new("TradesPerformanceOnResultsDayAnalysisBlock")
  trades_on_numbers.an <- dataRequest(trades_on_numbers.an, valid.key_values)

  trades_on_numbers.an <- Process(trades_on_numbers.an)

  trades_on_numbers.rd <- getOutputObject(trades_on_numbers.an)
}

test_that(paste("Can create", tested.class, "object"), {
  expect_is(new(tested.class), tested.class)
})



test_that(paste("Can use basic accessors of ", tested.class, "object"), {

  object <- new(tested.class)
  expect_is(object, tested.class)

  expect_is(getTradeDataObject(object), "TradesOnResultsDayData")

  expect_is(getOutputGGPlotData(object), "data.frame")
  expect_is(getOutputFrontendData(object), "data.frame")

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



test_that("Can set TradeData with externally processed result", {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  # trade data verification
  object <- setTradeDataObject(object, trades_on_numbers.rd)
  trade_data <- getTradeDataObject(object)
  expect_is(trade_data, "TradesOnResultsDayData")
  expect_equal(trade_data, trades_on_numbers.rd)


})


test_that("Can dataRequest() with valid key_values and no trade data set", {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  valid.key_values <- dated_twelve_monthly_lookback(11, today())
  colnames(valid.key_values) <- c("TraderID", "start", "end")

  object <- dataRequest(object, valid.key_values)

  expect_equal(getDataSourceQueryKeyValues(object), valid.key_values)

  # trade data verification
  trade_data <- getTradeDataObject(object)
  expect_is(trade_data, "TradesOnResultsDayData")
  expect_gt(nrow(getReferenceData(trade_data)), 0)


})


test_that(paste("Can Process() on", tested.class), {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  #valid.key_values <- dated_twelve_monthly_lookback(101, today())
  valid.key_values <- dated_this_year(101L, today())

  colnames(valid.key_values) <- c("TraderID", "start", "end")

  object <- dataRequest(object, valid.key_values)

  expect_equal(getDataSourceQueryKeyValues(object), valid.key_values)

  # trade data verification
  trade_data <- getTradeDataObject(object)
  expect_is(trade_data, "TradesOnResultsDayData")
  expect_gt(nrow(getReferenceData(trade_data)), 0)


  object <- Process(object)

  expect_is(getOutputGGPlot(object), "ggplot")
  expect_is(getOutputGGPlotData(object), "data.frame")
  expect_is(getOutputObject(object), "NULL")

})


