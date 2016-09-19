context("Testing TradesPerformanceOnResultsDayAnalysisBlock")

#####################################
#
# TradesPerformanceOnResultsDayAnalysisBlock Tests
#
#####################################
tested.class          <-  "TradesPerformanceOnResultsDayAnalysisBlock"
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
  expect_is(getPriceDataObject(object), "PriceData")
  expect_is(getEventDataObject(object), "EventData")

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



test_that("Can dataRequest() with valid key_values", {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  valid.key_values <- dated_twelve_monthly_lookback(11, today())
  colnames(valid.key_values) <- c("TraderID", "start", "end")

  object <- dataRequest(object, valid.key_values)

  expect_equal(getDataSourceQueryKeyValues(object), valid.key_values)

  # trade data verification
  trade_data <- getTradeDataObject(object)
  expect_is(trade_data, "TradeData")
  expect_gt(nrow(getReferenceData(trade_data)), 0)

  # position data verification
  position_data <- getPositionDataObject(object)
  expect_is(position_data, "PositionData")
  expect_gt(nrow(getReferenceData(position_data)), 0)


  # price data verification
  price_data <- getPriceDataObject(object)
  expect_is(price_data, "PriceData")
  expect_gt(nrow(getReferenceData(price_data)), 0)

  # event data verification
  event_data <- getEventDataObject(object)
  expect_is(event_data, "EventData")
  expect_gt(nrow(getReferenceData(event_data)), 0)


})


test_that(paste("Can Process() on", tested.class), {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  valid.key_values <- dated_twelve_monthly_lookback(70, today())
  colnames(valid.key_values) <- c("TraderID", "start", "end")

  object <- dataRequest(object, valid.key_values)

  expect_equal(getDataSourceQueryKeyValues(object), valid.key_values)

  # trade data verification
  trade_data <- getTradeDataObject(object)
  expect_is(trade_data, "TradeData")
  expect_gt(nrow(getReferenceData(trade_data)), 0)

  # position data verification
  position_data <- getPositionDataObject(object)
  expect_is(position_data, "PositionData")
  expect_gt(nrow(getReferenceData(position_data)), 0)

  # price data verification
  price_data <- getPriceDataObject(object)
  expect_is(price_data, "PriceData")
  expect_gt(nrow(getReferenceData(price_data)), 0)

  # event data verification
  event_data <- getEventDataObject(object)
  expect_is(event_data, "EventData")
  expect_gt(nrow(getReferenceData(event_data)), 0)


  object <- Process(object)

  expect_is(getOutputGGPlot(object), "ggplot")
  expect_is(getOutputGGPlotData(object), "data.frame")
  expect_is(getOutputObject(object), "ExtendedTradeData")

})


