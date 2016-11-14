context("Testing PortfolioFactorStateProbCondLossAnalysisBlock")


##############################################
#
# PortfolioFactorStateProbCondLossAnalysisBlock Tests
#
##############################################
# test vectors
tested.class          <-  "PortfolioFactorStateProbCondLossAnalysisBlock"
valid.column_name_map <- hash(c("TraderID", "start", "end"), c("id", "start", "end"))
init.key_values       <- data.frame(TraderID = character(),
                                    start    = as.Date(character()),
                                    end    = as.Date(character()))
valid.risk_model_class <- "RiskModel.DevelopedEuropePrototype150.1.1"

test_that(paste("Can create", tested.class, "object"), {
  expect_is(new(tested.class), tested.class)
})



test_that(paste("Can use basic accessors of ", tested.class, "object"), {

  object <- new(tested.class)
  expect_is(object, tested.class)

  expect_is(getImpliedFactorReturnsDataObject(object), "ImpliedFactorReturnsState")
  expect_is(getPortfolioDataObject(object), "StrategyPortfolio")
  expect_is(getOutputObject(object), "PortfolioImpliedFactorReturnsStateProbData")

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


test_that("Can dataRequest() with valid key_values and no previous data set", {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  valid.key_values <- dated_eighteen_monthly_lookback(11, "2016-11-30")
  colnames(valid.key_values) <- c("TraderID", "start", "end")

  object <- dataRequest(object, valid.key_values)


  expect_equal(getDataSourceQueryKeyValues(object), valid.key_values)


  # implied factor returns data verification
  impl_ret <- getImpliedFactorReturnsDataObject(object)
  expect_is(impl_ret, "ImpliedFactorReturnsState")
  expect_gt(getStoredNRows(impl_ret), 0)

  # portfolio data verification
  portf_data <- getPortfolioDataObject(object)
  expect_is(portf_data, "StrategyPortfolio")
  expect_gt(getStoredNRows(portf_data), 0)

})


test_that(paste("Can Process() on", tested.class), {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  valid.key_values <- dated_eighteen_monthly_lookback(11, "2016-11-30")
  colnames(valid.key_values) <- c("TraderID", "start", "end")

  object <- dataRequest(object, valid.key_values)


  expect_equal(getDataSourceQueryKeyValues(object), valid.key_values)


  # implied factor returns data verification
  impl_ret <- getImpliedFactorReturnsDataObject(object)
  expect_is(impl_ret, "ImpliedFactorReturnsState")
  expect_gt(getStoredNRows(impl_ret), 0)

  # portfolio data verification
  portf_data <- getPortfolioDataObject(object)
  expect_is(portf_data, "StrategyPortfolio")
  expect_gt(getStoredNRows(portf_data), 0)


  object <- Process(object)

  expect_is(getOutputGGPlot(object), "ggplot")
  expect_is(getOutputGGPlotData(object), "data.frame")

  output <- getOutputObject(object)
  expect_is(output, "PortfolioFactorReturnsData")
  expect_gt(getStoredNRows(output), 0)

})
