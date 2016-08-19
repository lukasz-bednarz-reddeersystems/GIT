context("Testing ExtendedTradesAnalysisBlock")

###################################
#
# ExtendedTradesAnalysisBlock Tests
#
###################################

# Generate Pre-requisite Data
if (Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")) {

  offside.pos.an <- new("OffsidePositionsAnalysisBlock")

  valid.key_values <- dated_twelve_monthly_lookback(11, '2016-06-30')
  # valid.key_values <- data.frame(id = 11,
  #                              start = as.Date("2015-07-31"),
  #                              end = as.Date("2015-08-31"))
  colnames(valid.key_values) <- c("TraderID", "start", "end")

  offside.pos.an <- dataRequest(offside.pos.an, valid.key_values)
  offside.pos.an <- Process(offside.pos.an)

  offside.pos.rd <- getOutputObject(offside.pos.an)
}

# test vectors

tested.class          <-  "AverageDownTradesAnalysisBlock"
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

  expect_is(getPositionDataObject(object), "OffsidePositionData")
  expect_is(getTradeDataObject(object), "TradeData")
  expect_is(getOutputObject(object), "AverageDownTradesData")

  expect_is(getOutputGGPlotData(object), "data.frame")
  expect_is(getOutputFrontendData(object), "data.frame")

  expect_equal(getDataSourceClientColumnNameMap(object), valid.column_name_map)

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



test_that("Can dataRequest() with valid key_values and position data set", {

  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  valid.key_values <- dated_twelve_monthly_lookback(11, '2016-06-30')
  colnames(valid.key_values) <- c("TraderID", "start", "end")

  object <- setPositionDataObject(object, offside.pos.rd)

  object <- dataRequest(object, valid.key_values)

  expect_equal(getDataSourceQueryKeyValues(object), valid.key_values)

  # trade data verification
  position_data <- getPositionDataObject(object)
  expect_is(position_data, "OffsidePositionData")
  expect_gt(getStoredNRows(position_data), 0)

  trade_data <- getTradeDataObject(object)
  expect_is(trade_data, "TradeData")
  expect_gt(getStoredNRows(trade_data), 0)

})



test_that("Can dataRequest() with valid key_values and no position data set", {

  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  valid.key_values <- dated_twelve_monthly_lookback(11, '2016-06-30')
  colnames(valid.key_values) <- c("TraderID", "start", "end")

  object <- dataRequest(object, valid.key_values)

  expect_equal(getDataSourceQueryKeyValues(object), valid.key_values)

  # trade data verification
  position_data <- getPositionDataObject(object)
  expect_is(position_data, "OffsidePositionData")
  expect_gt(getStoredNRows(position_data), 0)

  trade_data <- getTradeDataObject(object)
  expect_is(trade_data, "TradeData")
  expect_gt(getStoredNRows(trade_data), 0)


})


test_that(paste("Can Process() on", tested.class, "without position data set."), {

  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  valid.key_values <- dated_three_monthly_lookback(11, '2016-06-30')
  colnames(valid.key_values) <- c("TraderID", "start", "end")

  #object <- setPositionDataObject(object, offside.pos.rd)

  object <- dataRequest(object, valid.key_values)

  expect_equal(getDataSourceQueryKeyValues(object), valid.key_values)

  # trade data verification
  position_data <- getPositionDataObject(object)
  expect_is(position_data, "OffsidePositionData")
  expect_gt(getStoredNRows(position_data), 0)

  trade_data <- getTradeDataObject(object)
  expect_is(trade_data, "TradeData")
  expect_gt(getStoredNRows(trade_data), 0)

  object <- Process(object)

  expect_is(getOutputGGPlot(object), "ggplot")
  expect_is(getOutputGGPlotData(object), "data.frame")
  expect_is(getOutputObject(object), "AverageDownTradesData")

})


test_that(paste("Can Process() on", tested.class, "with position data set."), {

  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  valid.key_values <- dated_three_monthly_lookback(11, '2016-06-30')
  colnames(valid.key_values) <- c("TraderID", "start", "end")

  object <- setPositionDataObject(object, offside.pos.rd)

  object <- dataRequest(object, valid.key_values)

  expect_equal(getDataSourceQueryKeyValues(object), valid.key_values)

  # trade data verification
  position_data <- getPositionDataObject(object)
  expect_is(position_data, "OffsidePositionData")
  expect_gt(getStoredNRows(position_data), 0)

  trade_data <- getTradeDataObject(object)
  expect_is(trade_data, "TradeData")
  expect_gt(getStoredNRows(trade_data), 0)

  object <- Process(object)

  expect_is(getOutputGGPlot(object), "ggplot")
  expect_is(getOutputGGPlotData(object), "data.frame")
  expect_is(getOutputObject(object), "AverageDownTradesData")

})

