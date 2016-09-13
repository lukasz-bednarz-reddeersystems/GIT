context("Testing PortfolioFactorExposuresAnalysisBlock")

#############################################
#
# PortfolioFactorExposuresAnalysisBlock Tests
#
#############################################
# pre computed data
if (Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")) {
  portf.var.an <- new("IndexPortfolioVarianceDecompositionAnalysisBlock")

  valid.key_values <- dated_three_monthly_lookback("BE500 Index", "2016-05-30")
  colnames(valid.key_values) <- c("IndexTicker", "start", "end")

  portf.var.an <- dataRequest(portf.var.an, valid.key_values)

  portf_data <- getPortfolioDataObject(portf.var.an)
  betas_data <- getInstrumentBetasDataObject(portf.var.an)
}


# test vectors
tested.class          <-  "IndexPortfolioFactorExposuresAnalysisBlock"
valid.column_name_map <- hash(c("IndexTicker", "start", "end"), c("id", "start", "end"))
init.key_values       <- data.frame(IndexTicker = character(),
                                    start    = as.Date(character()),
                                    end    = as.Date(character()))

test_that(paste("Can create", tested.class, "object"), {
  expect_is(new(tested.class), tested.class)
})



test_that(paste("Can use basic accessors of ", tested.class, "object"), {

  object <- new(tested.class)
  expect_is(object, tested.class)

  expect_is(getPortfolioDataObject(object), "IndexPortfolio.BE500")
  expect_is(getRiskModelObject(object), "RiskModel.DevelopedEuropePrototype150")
  expect_is(getInstrumentBetasDataObject(object), "InstrumentBetasData")

  expect_is(getOutputGGPlotData(object), "data.frame")
  expect_is(getOutputFrontendData(object), "data.frame")
  expect_is(getOutputObject(object), "PortfolioFactorExposuresData")

  expect_equal(getDataSourceClientColumnNameMap(object), valid.column_name_map)


})


test_that("Cannot dataRequest() with invalid key_values", {

  object <- new(tested.class)


  invalid.key_values <- data.frame(IndexTicker = integer(),
                                   start = as.Date(character()),
                                   end = as.Date(character()))

  expect_error(dataRequest(object, invalid.key_values),
               regexp = "Zero row query keys data.frame passed")

  invalid.key_values <- data.frame(lC = numeric(), dtD = as.Date(character()))

  expect_error(dataRequest(object, invalid.key_values),
               regexp = "Invalid column names of query keys passed")


  expect_equal(getDataSourceQueryKeyValues(object), init.key_values)

})

test_that("Cannot set[Portfolio|InstrumentBetas]DataObject() with inValid Data", {

  object <- new(tested.class)

  # portfolio data verification
  expect_error(setPortfolioDataObject(object, new("InstrumentBetasData")),
               regexp = "unable to find an inherited method for function")
  portf_data <- getPortfolioDataObject(object)
  expect_is(portf_data, "IndexPortfolio.BE500")
  expect_equal(getStoredNRows(portf_data), 0)


  # instrument betas data verification
  expect_error(setInstrumentBetasDataObject(object, new("IndexPortfolio.BE500")),
               regexp = "unable to find an inherited method for function")
  ins_betas_data <- getInstrumentBetasDataObject(object)
  expect_is(ins_betas_data, "InstrumentBetasData")
  expect_equal(getStoredNRows(ins_betas_data), 0)

})


test_that("Can set[Portfolio|InstrumentBetas]DataObject() with Valid Data", {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)


  # portfolio data verification
  object <- setPortfolioDataObject(object, portf_data)
  ret_portf_data <- getPortfolioDataObject(object)
  expect_is(ret_portf_data, "IndexPortfolio.BE500")
  expect_gt(getStoredNRows(ret_portf_data), 0)
  expect_equal(ret_portf_data, portf_data)


  # instrument betas data verification
  object <- setInstrumentBetasDataObject(object, betas_data)
  ins_betas_data <- getInstrumentBetasDataObject(object)
  expect_is(ins_betas_data, "InstrumentBetasData")
  expect_gt(getStoredNRows(ins_betas_data), 0)
  expect_equal(ins_betas_data, betas_data)

})


test_that("Can dataRequest() with valid key_values and previous data set", {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  # portfolio data verification
  object <- setPortfolioDataObject(object, portf_data)

  # instrument betas data verification
  object <- setInstrumentBetasDataObject(object, betas_data)

  valid.key_values <- dated_full_month("BE500 Index", "2016-05-30")
  colnames(valid.key_values) <- c("IndexTicker", "start", "end")

  object <- dataRequest(object, valid.key_values)


  expect_equal(getDataSourceQueryKeyValues(object), valid.key_values)

  # portfolio data verification
  ret_portf_data <- getPortfolioDataObject(object)
  expect_is(ret_portf_data, "IndexPortfolio.BE500")
  expect_gt(getStoredNRows(portf_data), 0)
  expect_equal(ret_portf_data, portf_data)

  # instrument betas data verification
  ins_betas_data <- getInstrumentBetasDataObject(object)
  expect_is(ins_betas_data, "InstrumentBetasData")
  expect_gt(getStoredNRows(ins_betas_data), 0)
  expect_equal(ins_betas_data, betas_data)


})


test_that("Can dataRequest() with valid key_values and no previous data set", {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  valid.key_values <- dated_three_monthly_lookback("BE500 Index", "2016-05-30")
  colnames(valid.key_values) <- c("IndexTicker", "start", "end")

  object <- dataRequest(object, valid.key_values)


  expect_equal(getDataSourceQueryKeyValues(object), valid.key_values)

  # portfolio data verification
  portf_data <- getPortfolioDataObject(object)
  expect_is(portf_data, "IndexPortfolio.BE500")
  expect_gt(getStoredNRows(portf_data), 0)

  # instrument betas data verification
  ins_betas_data <- getInstrumentBetasDataObject(object)
  expect_is(ins_betas_data, "InstrumentBetasData")
  expect_gt(getStoredNRows(ins_betas_data), 0)

})


test_that(paste("Can Process() on", tested.class), {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  # portfolio data verification
  object <- setPortfolioDataObject(object, portf_data)

  # instrument betas data verification
  object <- setInstrumentBetasDataObject(object, betas_data)

  valid.key_values <- dated_three_monthly_lookback("BE500 Index", "2016-06-30")
  colnames(valid.key_values) <- c("IndexTicker", "start", "end")

  object <- dataRequest(object, valid.key_values)

  expect_equal(getDataSourceQueryKeyValues(object), valid.key_values)

  # portfolio data verification
  ret_portf_data <- getPortfolioDataObject(object)
  expect_is(ret_portf_data, "IndexPortfolio.BE500")
  expect_gt(getStoredNRows(portf_data), 0)
  expect_equal(ret_portf_data, portf_data)

  # instrument betas data verification
  ins_betas_data <- getInstrumentBetasDataObject(object)
  expect_is(ins_betas_data, "InstrumentBetasData")
  expect_gt(getStoredNRows(ins_betas_data), 0)
  expect_equal(ins_betas_data, betas_data)


  object <- Process(object)

  expect_is(getOutputGGPlot(object), "ggplot")
  expect_is(getOutputGGPlotData(object), "data.frame")

  output <- getOutputObject(object)
  expect_is(output, "PortfolioFactorExposuresData")
  expect_gt(getStoredNRows(output), 0)

})


context("Testing PortfolioFactorExposuresAnalysisBlock with non-default Risk Model")

#############################################
#
# PortfolioFactorExposuresAnalysisBlock Tests
#
#############################################

valid.risk_model      <- "RiskModel.DevelopedEuropePrototype150.1.1"
valid.risk_model_obj  <- new(valid.risk_model)

# pre computed data
if (Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")) {
  portf.var.an <- new("IndexPortfolioVarianceDecompositionAnalysisBlock")
  portf.var.an <- setRiskModelObject(portf.var.an, valid.risk_model_obj)

  valid.key_values <- dated_three_monthly_lookback("BE500 Index", "2016-08-30")
  colnames(valid.key_values) <- c("IndexTicker", "start", "end")

  portf.var.an <- dataRequest(portf.var.an, valid.key_values)

  portf_data <- getPortfolioDataObject(portf.var.an)
  betas_data <- getInstrumentBetasDataObject(portf.var.an)
}


# test vectors
tested.class          <-  "IndexPortfolioFactorExposuresAnalysisBlock"
valid.column_name_map <- hash(c("IndexTicker", "start", "end"), c("id", "start", "end"))
init.key_values       <- data.frame(IndexTicker = character(),
                                    start    = as.Date(character()),
                                    end    = as.Date(character()))

test_that(paste("Can create", tested.class, "object"), {
  object <- new(tested.class)
  object <- setRiskModelObject(object, valid.risk_model_obj)
  expect_is(object, tested.class)
})



test_that(paste("Can use basic accessors of ", tested.class, "object"), {

  object <- new(tested.class)
  object <- setRiskModelObject(object, valid.risk_model_obj)
  expect_is(object, tested.class)

  expect_is(getPortfolioDataObject(object), "IndexPortfolio.BE500")
  expect_is(getRiskModelObject(object), valid.risk_model)
  expect_is(getInstrumentBetasDataObject(object), "InstrumentBetasData")

  expect_is(getOutputGGPlotData(object), "data.frame")
  expect_is(getOutputFrontendData(object), "data.frame")
  expect_is(getOutputObject(object), "PortfolioFactorExposuresData")

  expect_equal(getDataSourceClientColumnNameMap(object), valid.column_name_map)


})


test_that("Cannot dataRequest() with invalid key_values", {

  object <- new(tested.class)
  object <- setRiskModelObject(object, valid.risk_model_obj)


  invalid.key_values <- data.frame(IndexTicker = integer(),
                                   start = as.Date(character()),
                                   end = as.Date(character()))

  expect_error(dataRequest(object, invalid.key_values),
               regexp = "Zero row query keys data.frame passed")

  invalid.key_values <- data.frame(lC = numeric(), dtD = as.Date(character()))

  expect_error(dataRequest(object, invalid.key_values),
               regexp = "Invalid column names of query keys passed")


  expect_equal(getDataSourceQueryKeyValues(object), init.key_values)

})

test_that("Cannot set[Portfolio|InstrumentBetas]DataObject() with inValid Data", {

  object <- new(tested.class)
  object <- setRiskModelObject(object, valid.risk_model_obj)

  # portfolio data verification
  expect_error(setPortfolioDataObject(object, new("InstrumentBetasData")),
               regexp = "unable to find an inherited method for function")
  portf_data <- getPortfolioDataObject(object)
  expect_is(portf_data, "IndexPortfolio.BE500")
  expect_equal(getStoredNRows(portf_data), 0)


  # instrument betas data verification
  expect_error(setInstrumentBetasDataObject(object, new("IndexPortfolio.BE500")),
               regexp = "unable to find an inherited method for function")
  ins_betas_data <- getInstrumentBetasDataObject(object)
  expect_is(ins_betas_data, "InstrumentBetasData")
  expect_equal(getStoredNRows(ins_betas_data), 0)

})


test_that("Can set[Portfolio|InstrumentBetas]DataObject() with Valid Data", {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)
  object <- setRiskModelObject(object, valid.risk_model_obj)


  # portfolio data verification
  object <- setPortfolioDataObject(object, portf_data)
  ret_portf_data <- getPortfolioDataObject(object)
  expect_is(ret_portf_data, "IndexPortfolio.BE500")
  expect_gt(getStoredNRows(ret_portf_data), 0)
  expect_equal(ret_portf_data, portf_data)


  # instrument betas data verification
  object <- setInstrumentBetasDataObject(object, betas_data)
  ins_betas_data <- getInstrumentBetasDataObject(object)
  expect_is(ins_betas_data, "InstrumentBetasData")
  expect_gt(getStoredNRows(ins_betas_data), 0)
  expect_equal(ins_betas_data, betas_data)

})


test_that("Can dataRequest() with valid key_values and previous data set", {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)
  object <- setRiskModelObject(object, valid.risk_model_obj)

  # portfolio data verification
  object <- setPortfolioDataObject(object, portf_data)

  # instrument betas data verification
  object <- setInstrumentBetasDataObject(object, betas_data)

  valid.key_values <- dated_full_month("BE500 Index", "2016-05-30")
  colnames(valid.key_values) <- c("IndexTicker", "start", "end")

  object <- dataRequest(object, valid.key_values)


  expect_equal(getDataSourceQueryKeyValues(object), valid.key_values)

  # portfolio data verification
  ret_portf_data <- getPortfolioDataObject(object)
  expect_is(ret_portf_data, "IndexPortfolio.BE500")
  expect_gt(getStoredNRows(portf_data), 0)
  expect_equal(ret_portf_data, portf_data)

  # instrument betas data verification
  ins_betas_data <- getInstrumentBetasDataObject(object)
  expect_is(ins_betas_data, "InstrumentBetasData")
  expect_gt(getStoredNRows(ins_betas_data), 0)
  expect_equal(ins_betas_data, betas_data)


})


test_that("Can dataRequest() with valid key_values and no previous data set", {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)
  object <- setRiskModelObject(object, valid.risk_model_obj)

  valid.key_values <- dated_three_monthly_lookback("BE500 Index", "2016-05-30")
  colnames(valid.key_values) <- c("IndexTicker", "start", "end")

  object <- dataRequest(object, valid.key_values)


  expect_equal(getDataSourceQueryKeyValues(object), valid.key_values)

  # portfolio data verification
  portf_data <- getPortfolioDataObject(object)
  expect_is(portf_data, "IndexPortfolio.BE500")
  expect_gt(getStoredNRows(portf_data), 0)

  # instrument betas data verification
  ins_betas_data <- getInstrumentBetasDataObject(object)
  expect_is(ins_betas_data, "InstrumentBetasData")
  expect_gt(getStoredNRows(ins_betas_data), 0)

})


test_that(paste("Can Process() on", tested.class), {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)
  object <- setRiskModelObject(object, valid.risk_model_obj)

  # portfolio data verification
  object <- setPortfolioDataObject(object, portf_data)

  # instrument betas data verification
  object <- setInstrumentBetasDataObject(object, betas_data)

  valid.key_values <- dated_three_monthly_lookback("BE500 Index", "2016-06-30")
  colnames(valid.key_values) <- c("IndexTicker", "start", "end")

  object <- dataRequest(object, valid.key_values)

  expect_equal(getDataSourceQueryKeyValues(object), valid.key_values)

  # portfolio data verification
  ret_portf_data <- getPortfolioDataObject(object)
  expect_is(ret_portf_data, "IndexPortfolio.BE500")
  expect_gt(getStoredNRows(portf_data), 0)
  expect_equal(ret_portf_data, portf_data)

  # instrument betas data verification
  ins_betas_data <- getInstrumentBetasDataObject(object)
  expect_is(ins_betas_data, "InstrumentBetasData")
  expect_gt(getStoredNRows(ins_betas_data), 0)
  expect_equal(ins_betas_data, betas_data)


  object <- Process(object)

  expect_is(getOutputGGPlot(object), "ggplot")
  expect_is(getOutputGGPlotData(object), "data.frame")

  output <- getOutputObject(object)
  expect_is(output, "PortfolioFactorExposuresData")
  expect_gt(getStoredNRows(output), 0)

})



