context("Testing PortfolioVarianceDecompositionAnalysisBlock")

########################################################
#
# PortfolioVarianceDecompositionAnalysisBlock Tests
#
########################################################
tested.class          <-  "PortfolioVarianceDecompositionAnalysisBlock"
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

  expect_is(getPortfolioDataObject(object), "StrategyPortfolio")
  expect_is(getRiskModelObject(object), valid.risk_model_class)
  expect_is(getInstrumentBetasDataObject(object), "InstrumentBetasData")
  expect_is(getFactorCorrelationDataObject(object), "FactorCorrelationData")
  expect_is(getFactorVarianceDataObject(object), "FactorVarianceData")

  expect_is(getOutputGGPlotData(object), "data.frame")
  expect_is(getOutputFrontendData(object), "list")

  expect_is(getOutputObject(object), "PortfolioVarianceFactorDecompositionData")

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



test_that("Can dataRequest() with valid key_values", {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  valid.key_values <- dated_three_monthly_lookback(11, "2016-06-30")

  colnames(valid.key_values) <- c("TraderID", "start", "end")

  object <- dataRequest(object, valid.key_values)


  expect_equal(getDataSourceQueryKeyValues(object), valid.key_values)

  # portfolio data verification
  portf_data <- getPortfolioDataObject(object)
  expect_is(portf_data, "StrategyPortfolio")
  expect_gt(getStoredNRows(portf_data), 0)

  # instrument betas data verification
  ins_betas_data <- getInstrumentBetasDataObject(object)
  expect_is(ins_betas_data, "InstrumentBetasData")
  expect_gt(getStoredNRows(ins_betas_data), 0)

  # factor correlation data verification
  fct_corr_data <- getFactorCorrelationDataObject(object)
  expect_is(fct_corr_data, "FactorCorrelationData")
  expect_gt(getStoredNRows(fct_corr_data), 0)

  # factor variance data verification
  fct_var_data <- getFactorVarianceDataObject(object)
  expect_is(fct_var_data, "FactorVarianceData")
  expect_gt(getStoredNRows(fct_var_data), 0)


})


test_that(paste("Can Process() on", tested.class), {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  #valid.key_values <- dated_three_monthly_lookback(11, "2016-08-30")
  valid.key_values <- dated_full_month(11, "2016-10-30")
  #valid.key_values <- dated_eighteen_monthly_lookback(11, '2016-07-01')

  colnames(valid.key_values) <- c("TraderID", "start", "end")

  object <- dataRequest(object, valid.key_values)

  expect_equal(getDataSourceQueryKeyValues(object), valid.key_values)

  # portfolio data verification
  portf_data <- getPortfolioDataObject(object)
  expect_is(portf_data, "StrategyPortfolio")
  expect_gt(getStoredNRows(portf_data), 0)

  # instrument betas data verification
  ins_betas_data <- getInstrumentBetasDataObject(object)
  expect_is(ins_betas_data, "InstrumentBetasData")
  expect_gt(getStoredNRows(ins_betas_data), 0)

  # factor correlation data verification
  fct_corr_data <- getFactorCorrelationDataObject(object)
  expect_is(fct_corr_data, "FactorCorrelationData")
  expect_gt(getStoredNRows(fct_corr_data), 0)

  # factor variance data verification
  fct_var_data <- getFactorVarianceDataObject(object)
  expect_is(fct_var_data, "FactorVarianceData")
  expect_gt(getStoredNRows(fct_var_data), 0)


  object <- Process(object)

  expect_is(getOutputGGPlot(object), "ggplot")
  expect_is(getOutputGGPlotData(object), "data.frame")

  output <- getOutputObject(object)
  expect_is(output, "PortfolioVarianceFactorDecompositionData")
  expect_gt(getStoredNRows(output), 0)

})


