sourceTo("../analysis_modules/market_style_timeseries/market_style_timeseries.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(testthat)

################################
#
# MarketStyleAnalysisBlock Tests
#
################################

# test vectors
tested.class          <-  "MarketStyleAnalysisBlock"
valid.column_name_map <- hash(c("start", "end"), c("start", "end"))
init.key_values       <- data.frame(start    = as.Date(character()),
                                    end    = as.Date(character()))

test_that(paste("Can create", tested.class, "object"), {
  expect_is(new(tested.class), tested.class)
})



test_that(paste("Can use basic accessors of ", tested.class, "object"), {
  
  object <- new(tested.class)
  expect_is(object, tested.class)
  
  expect_is(getRiskModelObject(object), "RiskModel.DevelopedEuropePrototype150")
  expect_is(getMarketStyleDataObject(object), "MarketStyleData")
  
  expect_is(getOutputGGPlotData(object), "data.frame")
  expect_is(getOutputFrontendData(object), "data.frame")
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
  
  object <- new(tested.class)
  
  valid.key_values <- dated_full_month(11, "2016-05-30")
  colnames(valid.key_values) <- c("TraderID", "start", "end")
  
  # Request Data
  object <- dataRequest(object, valid.key_values)
  
  
  expect_equal(getDataSourceQueryKeyValues(object), valid.key_values[,-1])
  
  # market style data verification
  market_data <- getMarketStyleDataObject(object)
  expect_is(market_data, "MarketStyleData")
  expect_equal(getStoredNRows(market_data), 0)

  
  
})


test_that(paste("Can Process() on", tested.class), {
  
  object <- new(tested.class)
  
  valid.key_values <- dated_full_month(11, "2016-05-30")
  colnames(valid.key_values) <- c("TraderID", "start", "end")
  
  # Request Data
  object <- dataRequest(object, valid.key_values)
  
  
  expect_equal(getDataSourceQueryKeyValues(object), valid.key_values[,-1])
  
  # market style data verification
  market_data <- getMarketStyleDataObject(object)
  expect_is(market_data, "MarketStyleData")
  expect_gt(getStoredNRows(market_data), 0)
  
  # Request Data


  
  object <- Process(object)
  
  expect_is(getOutputGGPlot(object), "ggplot")
  expect_is(getOutputGGPlotData(object), "data.frame")

})


