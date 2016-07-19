sourceTo("../analysis_modules/pnl_traded_in_long_short_hedge/pnl_traded_in_long_short_hedge.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(testthat)

#########################
#
# PnLTradedInLongShortHedgeAnalysisBlock Tests
#
#########################
# Generate Pre-requisite Data

val.traded.an <- new("ValueTradedInLongShortHedgeAnalysisBlock")

valid.key_values <- dated_twelve_monthly_lookback(11, "2016-06-30")
colnames(valid.key_values) <- c("TraderID", "start", "end")

val.traded.an <- dataRequest(val.traded.an, valid.key_values)
val.traded.an <- Process(val.traded.an)

val.traded.rd <- getOutputObject(val.traded.an)

# test vectors
tested.class          <-  "PnLTradedInLongShortHedgeAnalysisBlock"

test_that(paste("Can create", tested.class, "object"), {
  expect_is(new(tested.class), tested.class)
})



test_that(paste("Can use basic accessors of ", tested.class, "object"), {
  
  object <- new(tested.class)
  expect_is(object, tested.class)
  
  expect_is(getTradeDataObject(object), "ValueTradedData")

  expect_is(getOutputGGPlotData(object), "data.frame")
  expect_is(getOutputFrontendData(object), "data.frame")

})


test_that("Cannot setTradeDataObject() with invalid type", {
  
  object <- new(tested.class)
  
  expect_error(setTradeDataObject(object, new("TradeData")),
               regexp = "unable to find an inherited method for function")
  
})


test_that("Can setTradeDataObject() with valid type", {
  
  object <- new(tested.class)
  
  object <- setTradeDataObject(object, new("ValueTradedData"))
  expect_is(getTradeDataObject(object), "ValueTradedData")
  
  
  object <- setTradeDataObject(object, val.traded.rd)
  
  # trade data verification
  trade_data <- getTradeDataObject(object)
  expect_is(trade_data, "ValueTradedData")
  expect_gt(nrow(getReferenceData(trade_data)), 0)
  expect_equal(getReferenceData(trade_data),
               getReferenceData(val.traded.rd))
  
})


test_that(paste("Can Process() on", tested.class), {
  
  object <- new(tested.class)
  
  object <- setTradeDataObject(object, val.traded.rd)
  
  # trade data verification
  trade_data <- getTradeDataObject(object)
  expect_is(trade_data, "ValueTradedData")
  expect_gt(nrow(getReferenceData(trade_data)), 0)
  expect_equal(getReferenceData(trade_data),
               getReferenceData(val.traded.rd))

  object <- Process(object)
  
  expect_is(getOutputGGPlot(object), "ggplot")
  expect_is(getOutputGGPlotData(object), "data.frame")


})


