#################################################################
#
# Test PortfolioFactorExposuresAnalysisBlockClient
#
#################################################################


tested.class <- "PortfolioFactorExposuresAnalysisBlockClient"
analysis.class <- "PortfolioFactorExposuresAnalysisBlock"

context(sprintf("Testing %s class.", tested.class))

valid.key_values <- dated_three_monthly_lookback(101, "2016-09-01")
#valid.key_values <- dated_twelve_monthly_lookback(11, "2016-09-01")

test_that(sprintf("can instantiate %s class", tested.class), {

  object <- new(tested.class)
  expect_is(object, tested.class)

})


test_that(sprintf("Can dataRequest on %s class", tested.class),{
  object <- new(tested.class)

  expect_is(object, tested.class)

  object <- dataRequest(object, valid.key_values, TRUE)

  block_data <- getReferenceData(object)

  expect_is(block_data, "data.frame")
})


test_that(sprintf("Can getAnalysisBlock on %s class", tested.class),{
  object <- new(tested.class)
  object <- dataRequest(object, valid.key_values)

  block_data <- getReferenceData(object)

  expect_is(block_data, "data.frame")


  block <- getAnalysisBlock(object)
  block_refdata <- getOutputObject(block)
  block_plot <- getOutputGGPlot(block)
  block_plot_data <- getOutputGGPlotData(block)

  expect_is(block, analysis.class)
  expect_is(block_plot, "ggplot")
  expect_is(block_plot_data, "data.frame")


})


#################################################################
#
# Test PortfolioFactorReturnsAnalysisBlockClient
#
#################################################################


tested.class <- "PortfolioFactorReturnsAnalysisBlockClient"
analysis.class <- "PortfolioFactorReturnsAnalysisBlock"

context(sprintf("Testing %s class.", tested.class))

# valid.key_values <- dated_three_monthly_lookback(11, "2016-09-01")


test_that(sprintf("can instantiate %s class", tested.class), {

  object <- new(tested.class)
  expect_is(object, tested.class)

})


test_that(sprintf("Can dataRequest on %s class", tested.class),{
  object <- new(tested.class)

  expect_is(object, tested.class)

  object <- dataRequest(object, valid.key_values, TRUE)

  block_data <- getReferenceData(object)

  expect_is(block_data, "data.frame")
})


test_that(sprintf("Can getAnalysisBlock on %s class", tested.class),{
  object <- new(tested.class)
  object <- dataRequest(object, valid.key_values)

  block_data <- getReferenceData(object)

  expect_is(block_data, "data.frame")


  block <- getAnalysisBlock(object)
  block_refdata <- getOutputObject(block)
  block_plot <- getOutputGGPlot(block)
  block_plot_data <- getOutputGGPlotData(block)

  expect_is(block, analysis.class)
  expect_is(block_plot, "ggplot")
  expect_is(block_plot_data, "data.frame")


})



#################################################################
#
# Test PortfolioVarianceDecompositionAnalysisBlockClientClient
#
#################################################################


tested.class <- "PortfolioVarianceDecompositionAnalysisBlockClient"
analysis.class <- "PortfolioVarianceDecompositionAnalysisBlock"

context(sprintf("Testing %s class.", tested.class))

# valid.key_values <- dated_three_monthly_lookback(11, "2016-09-01")


test_that(sprintf("can instantiate %s class", tested.class), {

  object <- new(tested.class)
  expect_is(object, tested.class)

})


test_that(sprintf("Can dataRequest on %s class", tested.class),{
  object <- new(tested.class)

  expect_is(object, tested.class)

  object <- dataRequest(object, valid.key_values, TRUE)

  block_data <- getReferenceData(object)

  expect_is(block_data, "data.frame")
})


test_that(sprintf("Can getAnalysisBlock on %s class", tested.class),{
  object <- new(tested.class)
  object <- dataRequest(object, valid.key_values)

  block_data <- getReferenceData(object)

  expect_is(block_data, "data.frame")


  block <- getAnalysisBlock(object)
  block_refdata <- getOutputObject(block)
  block_plot <- getOutputGGPlot(block)
  block_plot_data <- getOutputGGPlotData(block)

  expect_is(block, analysis.class)
  expect_is(block_plot, "ggplot")
  expect_is(block_plot_data, "data.frame")


})

#################################################################
#
# Test MarketStyleFactorStatisticAnalysisBlockClient
#
#################################################################


tested.class <- "MarketStyleFactorStatisticAnalysisBlockClient"
analysis.class <- "MarketStyleFactorStatisticAnalysisBlock"

context(sprintf("Testing %s class.", tested.class))

# valid.key_values <- dated_three_monthly_lookback(11, "2016-09-01")

test_that(sprintf("can instantiate %s class", tested.class), {

  object <- new(tested.class)
  expect_is(object, tested.class)

})


test_that(sprintf("Can dataRequest on %s class", tested.class),{
  object <- new(tested.class)

  expect_is(object, tested.class)

  object <- dataRequest(object, valid.key_values, TRUE)

  block_data <- getReferenceData(object)

  expect_is(block_data, "data.frame")
})


test_that(sprintf("Can getAnalysisBlock on %s class", tested.class),{
  object <- new(tested.class)
  object <- dataRequest(object, valid.key_values)

  block_data <- getReferenceData(object)

  expect_is(block_data, "data.frame")


  block <- getAnalysisBlock(object)
  block_refdata <- getOutputObject(block)
  block_plot <- getOutputGGPlot(block)
  block_plot_data <- getOutputGGPlotData(block)

  expect_is(block, analysis.class)
  expect_is(block_plot, "ggplot")
  expect_is(block_plot_data, "data.frame")


})

