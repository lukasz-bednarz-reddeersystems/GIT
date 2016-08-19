context("Testing AverageDownTradesFocusAnalysisBlock")

#######################################
#
# AverageDownTradesAnalysisBlock Tests
#
#######################################

if (Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")) {
  # Generate Pre-requisite Data
  avg.down.trd.an <- new("AverageDownTradesAnalysisBlock")

  valid.key_values <- dated_three_monthly_lookback(11, '2016-06-30')
  colnames(valid.key_values) <- c("TraderID", "start", "end")

  avg.down.trd.an <- dataRequest(avg.down.trd.an, valid.key_values)

  avg.down.trd.an <- Process(avg.down.trd.an)

  avg.down.trd.rd <- getOutputObject(avg.down.trd.an)
}

# test vectors
tested.class          <-  "AverageDownTradesFocusAnalysisBlock"

test_that(paste("Can create", tested.class, "object"), {
  expect_is(new(tested.class), tested.class)
})



test_that(paste("Can use basic accessors of ", tested.class, "object"), {

  object <- new(tested.class)
  expect_is(object, tested.class)

  expect_is(getTradeDataObject(object), "AverageDownTradesData")
  expect_is(getOutputGGPlotData(object), "data.frame")
  expect_is(getOutputFrontendData(object), "data.frame")

})

test_that(paste("Can setTradeData() on", tested.class), {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  object <- setTradeDataObject(object, avg.down.trd.rd)

  trade_data <- getTradeDataObject(object)
  expect_is(trade_data, "AverageDownTradesData")
  expect_gt(getStoredNRows(trade_data), 0)
  expect_equal(trade_data, avg.down.trd.rd)

})


test_that(paste("Can Process() on", tested.class), {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  object <- setTradeDataObject(object, avg.down.trd.rd)

  trade_data <- getTradeDataObject(object)
  expect_is(trade_data, "AverageDownTradesData")
  expect_gt(getStoredNRows(trade_data), 0)
  expect_equal(trade_data, avg.down.trd.rd)

  object <- Process(object)

  expect_is(getOutputGGPlot(object), "ggplot")
  expect_is(getOutputGGPlotData(object), "data.frame")


})


