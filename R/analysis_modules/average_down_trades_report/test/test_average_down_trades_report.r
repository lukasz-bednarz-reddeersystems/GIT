sourceTo("../analysis_modules/average_down_trades_report/average_down_trades_report.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(testthat)

#########################
#
# ExtendedTradesAnalysisBlock Tests
#
#########################
tested.class             <-  "AverageDownTradesReport"
valid.column_name_map    <- hash(c("TraderID", "start", "end"), c("id", "start", "end"))
valid.key_cols           <- c("TraderID", "start", "end")
init.key_values          <- data.frame(TraderID = character(),
                                    start    = as.Date(character()),
                                    end    = as.Date(character()))

valid.ggplot_list        <- list()
valid.ggplot_data_list   <- list()
valid.frontend_data_list <- list()
valid.output_list        <- list()

test_that(paste("Can create", tested.class, "object"), {
  expect_is(new(tested.class), tested.class)
})



test_that(paste("Can use basic accessors of ", tested.class, "object"), {
  
  object <- new(tested.class)
  expect_is(object, tested.class)
  
  expect_equal(getDataSourceQueryKeyColumnNames(object), valid.key_cols)
  expect_equal(getDataSourceQueryKeyValues(object), init.key_values)
  expect_equal(getDataSourceClientColumnNameMap(object), valid.column_name_map)
  
  expect_is(getOutputGGPlotList(object), "list")
  expect_is(getOutputObjectList(object), "list")
  expect_is(getOutputFrontendDataList(object), "list")
  expect_is(getOutputGGPlotDataList(object), "list")

  
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
  
  object <- new(tested.class)
  
  valid.key_values <- dated_three_monthly_lookback(11, '2016-06-30')
  colnames(valid.key_values) <- c("TraderID", "start", "end")
  
  object <- dataRequest(object, valid.key_values)
  
  expect_equal(getDataSourceQueryKeyValues(object), valid.key_values)

  
})


test_that(paste("Can Process() on", tested.class), {
  
  object <- new(tested.class)
  
  valid.key_values <- dated_twelve_monthly_lookback(11, today())
  #valid.key_values <- dated_three_monthly_lookback(11, '2016-06-30')
  
  colnames(valid.key_values) <- c("TraderID", "start", "end")
  
  object <- dataRequest(object, valid.key_values)
  
  expect_equal(getDataSourceQueryKeyValues(object), valid.key_values)
  
  # trade data verification

  object <- Process(object)
  
  expect_is(getOutputGGPlotList(object), "list")
  expect_is(getOutputGGPlotDataList(object), "list")
  expect_is(getOutputObjectList(object), "list")
  expect_is(getOutputFrontendDataList(object), "list")
  
  expect_true(setequal(names(getOutputGGPlotList(object)), 
                       c(average_down_trades_report_analysis_blocks, "Summary")))
  
  expect_true(setequal(names(getOutputGGPlotDataList(object)), 
                       average_down_trades_report_analysis_blocks))
  
  expect_true(setequal(names(getOutputObjectList(object)), 
                       setdiff(average_down_trades_report_analysis_blocks, c("AverageDownTradesFocus"))))
              
  expect_true(setequal(names(getOutputFrontendDataList(object)), 
                       average_down_trades_report_analysis_blocks))

})


