sourceTo("../analysis_modules/positions_holding_day_zero_pnl/positions_holding_day_zero_pnl.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(testthat)

###############################################
#
# OffsidePositionsGainVsDaysAnalysisBlock Tests
#
###############################################

# Generate Pre-requisite Data
offside.pos.an <- new("OffsidePositionsAnalysisBlock")

valid.key_values <- dated_three_monthly_lookback(11, as.Date("2016-06-30"))
colnames(valid.key_values) <- c("TraderID", "start", "end")

offside.pos.an <- dataRequest(offside.pos.an, valid.key_values)
offside.pos.an <- Process(offside.pos.an)

offside.pos.rd <- getOutputObject(offside.pos.an)


# test vectors
tested.class          <-  "PositionsHoldingDayZeroPnLAnalysisBlock"


test_that(paste("Can create", tested.class, "object"), {
  expect_is(new(tested.class), tested.class)
})



test_that(paste("Can use basic accessors of ", tested.class, "object"), {
  
  object <- new(tested.class)
  expect_is(object, tested.class)
  
  expect_is(getPositionDataObject(object), "OffsidePositionData")

  expect_is(getOutputGGPlotData(object), "data.frame")
  expect_is(getOutputFrontendData(object), "data.frame")

  
})


test_that(paste("Can Process() on", tested.class), {
  
  object <- new(tested.class)

  # set data
  object <- setPositionDataObject(object, offside.pos.rd)
  object <- dataRequest(object, valid.key_values)
  
  # trade data verification
  position_data <- getPositionDataObject(object)
  expect_is(position_data, "OffsidePositionData")
  expect_gt(nrow(getReferenceData(position_data)), 0)

  
  object <- Process(object)
  
  expect_is(getOutputGGPlot(object), "ggplot")
  expect_is(getOutputGGPlotData(object), "data.frame")

})


