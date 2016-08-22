sourceTo("../analysis_modules/extended_trades/extended_trades.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/extended_trades_summary/extended_trades_summary.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(testthat)

#########################
#
# ExtendedTradesSummaryAnalysisBlock Tests
#
#########################

# compute Extended stock analyisis
ext.stock.an <- new("ExtendedTradesAnalysisBlock")

valid.key_values <- dated_twelve_monthly_lookback(11, today())
colnames(valid.key_values) <- c("TraderID", "start", "end")
ext.stock.an <- dataRequest(ext.stock.an, valid.key_values)
ext.stock.an <- Process(ext.stock.an)

ext.stock.rd <- getOutputObject(ext.stock.an)

trades.rd     <- getTradeDataObject(ext.stock.an)

# test vectors
tested.class = "ExtendedTradesSummaryAnalysisBlock"


test_that(paste("Can create", tested.class, "object"), {
  expect_is(new(tested.class), tested.class)
})


test_that(paste("Can use basic accessors of ", tested.class, "object"), {
  
  object <- new(tested.class)
  expect_is(object, tested.class)

  expect_is(getTradeDataObject(object), "TradeData")
  expect_is(getExtendedTradeDataObject(object), "ExtendedTradeData")
  expect_is(getOutputGGPlotData(object), "data.frame")
  expect_is(getOutputFrontendData(object), "data.frame")
  
})


test_that("Can setTradeDataObject() with valid value", {
  
  object <- new(tested.class)

  object <- setTradeDataObject(object, trades.rd)
  expect_equal(getTradeDataObject(object), trades.rd)
  
  object <- setExtendedTradeDataObject(object, ext.stock.rd)
  expect_equal(getExtendedTradeDataObject(object), ext.stock.rd)

  
})


test_that(paste("Can call Process() on", tested.class), {
  
  object <- new(tested.class)
  
  object <- setTradeDataObject(object, trades.rd)
  object <- setExtendedTradeDataObject(object, ext.stock.rd)

  object <- Process(object)
  
  expect_is(getOutputGGPlot(object), "ggplot")
  expect_is(getOutputGGPlotData(object), "data.frame")

})


