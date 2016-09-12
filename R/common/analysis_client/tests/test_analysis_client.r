library(R.utils)
options(modifiedOnlySource=TRUE)
sourceTo("../common/analysis_client/client_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(testthat)

####################################################
#
# StrategyBreakdownAnalysisBlockClient Tests
#
####################################################

block.class <- "StrategyBreakdownAnalysisBlock"
block_client <- new(paste(block.class,"Client",sep=""))

valid.id   <- 11
valid.date <- "2016-01-01"
valid.function <- dated_three_monthly_lookback
valid.key_values <- valid.function(valid.id, valid.date)
block_client <- dataRequest(block_client, valid.key_values)
block_data <- getReferenceData(block_client)
block <- getAnalysisBlock(block_client)
block_refdata <- getOutputObject(block)
block_plot <- getOutputGGPlot(block)
block_plot_data <- getOutputGGPlotData(block)

test_that(paste("Got block class", block.class), {
  expect_is(block, block.class)
})

test_that(paste("Got block data", block_data), {
  expect_is(block_data, "data.frame")
})


