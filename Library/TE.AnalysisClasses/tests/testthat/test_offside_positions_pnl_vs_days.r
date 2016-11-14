context("Testing OffsidePositionsPnLVsDaysAnalysisBlock")

#####################################################
#
# OffsidePositionsPnLVsDaysAnalysisBlock Tests
#
#####################################################

if (Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")) {
  # Generate Pre-requisite Data
  offside.pos.an <- new("OffsidePositionsAnalysisBlock")

  valid.key_values <- dated_twelve_monthly_lookback(11, today())
  colnames(valid.key_values) <- c("TraderID", "start", "end")

  offside.pos.an <- dataRequest(offside.pos.an, valid.key_values)
  offside.pos.an <- Process(offside.pos.an)

  offside.pos.rd <- getOutputObject(offside.pos.an)

  offside.gain.an <- new("OffsidePositionsGainVsDaysAnalysisBlock")

  offside.gain.an <- setPositionDataObject(offside.gain.an, offside.pos.rd)

  offside.gain.an <- Process(offside.gain.an)
  offside.gain.rd <- getOutputObject(offside.gain.an)
}

# test vectors
tested.class          <-  "OffsidePositionsPnLVsDaysAnalysisBlock"


test_that(paste("Can create", tested.class, "object"), {
  expect_is(new(tested.class), tested.class)
})



test_that(paste("Can use basic accessors of ", tested.class, "object"), {

  object <- new(tested.class)
  expect_is(object, tested.class)

  expect_is(getPositionDataObject(object), "OffsidePositionGainData")

  expect_is(getOutputGGPlotData(object), "data.frame")
  expect_is(getOutputFrontendData(object), "list")



})


test_that(paste("Can Process() on", tested.class), {

  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  # set data
  object <- setPositionDataObject(object, offside.gain.rd)

  # trade data verification
  position_data <- getPositionDataObject(object)
  expect_is(position_data, "OffsidePositionGainData")
  expect_gt(nrow(getReferenceData(position_data)), 0)


  object <- Process(object)

  expect_is(getOutputGGPlot(object), "ggplot")
  expect_is(getOutputGGPlotData(object), "data.frame")
  outp <- getOutputObject(object)
  expect_is(outp, "OffsidePositionGainAndCategoryData")
  expect_gt(nrow(getReferenceData(outp)), 0)

})


