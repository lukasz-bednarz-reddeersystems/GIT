context("Testing StrategyBreakdownAnalysisBlockClient")

####################################################
#
# StrategyBreakdownAnalysisBlockClient Tests
#
####################################################

tested.class <- "StrategyBreakdownAnalysisBlock"
client.class <- paste(tested.class,"Client",sep="")

valid.id   <- 11
valid.date <- "2016-01-01"
valid.function <- dated_three_monthly_lookback
valid.key_values <- valid.function(valid.id, valid.date)

test_that(sprintf("Can instantiate on %s class", client.class),{
  object <- new(client.class)

  expect_is(object, client.class)

})


test_that(sprintf("Can dataRequest on %s class", tested.class),{
  object <- new(client.class)

  expect_is(object, client.class)

  object <- dataRequest(object, valid.key_values)

  block_data <- getReferenceData(block_client)

  expect_is(block_data, "data.frame")
})


test_that(sprintf("Can getAnalysisBlock on %s class", tested.class),{
  object <- new(client.class)
  object <- dataRequest(object, valid.key_values)

  block_data <- getReferenceData(block_client)

  expect_is(block_data, "data.frame")


  block <- getAnalysisBlock(block_client)
  block_refdata <- getOutputObject(block)
  block_plot <- getOutputGGPlot(block)
  block_plot_data <- getOutputGGPlotData(block)

  expect_is(block, tested.class)
  expect_is(block_plot, "ggplot")
  expect_is(block_plot_data, "data.frame")


})



