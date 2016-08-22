context("Testing RelativeMarketReturnAnalysisBlock")

####################################################
#
# RelativeMarketReturnAnalysisBlock Tests
#
####################################################

# compute Extended stock analyisis
if (Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")) {
  ext.stock.an <- new("ExtendedTradesAnalysisBlock")

  valid.key_values <- dated_twelve_monthly_lookback(11, today())
  colnames(valid.key_values) <- c("TraderID", "start", "end")
  ext.stock.an <- dataRequest(ext.stock.an, valid.key_values)
  ext.stock.an <- Process(ext.stock.an)

  ext.stock.rd <- getOutputObject(ext.stock.an)

  index.rd     <- new("MarketDataSX5E",
                      min(valid.key_values$start),
                      max(valid.key_values$end))

  trades.rd     <- getTradeDataObject(ext.stock.an)

  # compute Market return
  market.ret.an <- new("MarketReturnAnalysisBlock")

  market.ret.an <- setTradeDataObject(market.ret.an, ext.stock.rd)
  market.ret.an <- setMarketDataObject(market.ret.an, index.rd)

  market.ret.an <- Process(market.ret.an)

  ext.ret.rd    <- getOutputObject(market.ret.an)

}

# test vectors
tested.class = "RelativeMarketReturnAnalysisBlock"


test_that(paste("Can create", tested.class, "object"), {
  expect_is(new(tested.class), tested.class)
})



test_that(paste("Can use basic accessors of ", tested.class, "object"), {

  object <- new(tested.class)
  expect_is(object, tested.class)

  expect_is(getTradeDataObject(object), "TradeData")
  expect_is(getExtendedTradeDataObject(object), "TradesExtendedReturnPerMonth")
  expect_is(getOutputGGPlotData(object), "data.frame")
  expect_is(getOutputFrontendData(object), "data.frame")

})


test_that("Can setTradeDataObject() with valid value", {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  object <- setTradeDataObject(object, trades.rd)
  expect_equal(getTradeDataObject(object), trades.rd)

  object <- setExtendedTradeDataObject(object, ext.ret.rd)
  expect_equal(getExtendedTradeDataObject(object), ext.ret.rd)

  object <- setMarketDataObject(object, index.rd)
  expect_equal(getMarketDataObject(object), index.rd)

})


test_that(paste("Can call Process() on", tested.class), {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  object <- setTradeDataObject(object, trades.rd)
  object <- setExtendedTradeDataObject(object, ext.ret.rd)
  object <- setMarketDataObject(object, index.rd)

  object <- Process(object)

  expect_is(getOutputGGPlot(object), "ggplot")
  expect_is(getOutputGGPlotData(object), "data.frame")
  expect_is(getOutputObject(object), "TradesExtendedReturnPerMonth")

})


