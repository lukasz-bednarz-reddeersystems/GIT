sourceTo("../analysis_modules/strategy_breakdown_signal_characteristic_and_effeciveness/strategy_breakdown_signal_characteristic_and_effeciveness.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(testthat)

####################################################
#
# StrategyBreakdownSignalCharacteristicAndEffectivenessAnalysisBlock Tests
#
####################################################
# get required data
strat.brdwn.an <- new("StrategyBreakdownValueTradedPerSignalAnalysisBlock")

valid.key_values <- dated_twelve_monthly_lookback(11, "2016-06-30")
colnames(valid.key_values) <- c("TraderID", "start", "end")

strat.brdwn.an <- dataRequest(strat.brdwn.an, valid.key_values)
strat.brdwn.an <- Process(strat.brdwn.an)

strat.brdwn.rd <- getOutputObject(strat.brdwn.an)

# test vectors
tested.class <- "StrategyBreakdownSignalCharacteristicAndEffectivenessAnalysisBlock"
valid.required_cols <- c(strategy_breakdown_per_signal_base_cols,
                         strategy_breakdown_per_signal_signal_cols)


test_that(paste("Can create", tested.class, "object"), {
  expect_is(new(tested.class), tested.class)
})



test_that(paste("Can use basic accessors of ", tested.class, "object"), {
  
  object <- new(tested.class)
  expect_is(object, tested.class)
  
  expect_is(getTradeDataObject(object), "TradedSignalsData")

  expect_is(getOutputGGPlotData(object), "data.frame")
  expect_is(getOutputFrontendData(object), "data.frame")

  expect_equal(getRequiredVariablesNames(object), valid.required_cols)

})


test_that(paste("Cannot set Trade Data with invalid input on ", tested.class, "object"), {
  
  object <- new(tested.class)
  expect_is(object, tested.class)
  
  expect_error(setTradeDataObject(object, new("TradeData")), 
               regexp = "unable to find an inherited method for function")
  
  expect_is(getTradeDataObject(object), "TradedSignalsData")

})

test_that(paste("Can set Trade Data with valid input on ", tested.class, "object"), {
  
  object <- new(tested.class)
  expect_is(object, tested.class)
  
  object <- setTradeDataObject(object, new("TradedSignalsData"))
  expect_is(getTradeDataObject(object), "TradedSignalsData")
  
  object <- setTradeDataObject(object, strat.brdwn.rd)
  expect_is(getTradeDataObject(object), "TradedSignalsData")
  expect_equal(getTradeDataObject(object), strat.brdwn.rd)
  expect_gt(getStoredNRows(getTradeDataObject(object)),0)
  
})

test_that(paste("Can Run Process() on", tested.class), {
  
  object <- new(tested.class)

  # trade data verification
  object <- setTradeDataObject(object, strat.brdwn.rd)
  expect_is(getTradeDataObject(object), "TradedSignalsData")
  expect_equal(getTradeDataObject(object), strat.brdwn.rd)

  trade_data <- getTradeDataObject(object)
  expect_gt(getStoredNRows(trade_data), 0)
  expect_equal(getReferenceData(trade_data), getReferenceData(strat.brdwn.rd))
 
  object <- Process(object)
  
  expect_is(getOutputGGPlot(object), "ggplot")
  expect_is(getOutputGGPlotData(object), "data.frame")

})


