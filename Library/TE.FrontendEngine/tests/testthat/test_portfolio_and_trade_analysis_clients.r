####################################################
#
# StrategyBreakdownAnalysisBlockClient Tests
#
####################################################

tested.class <- "StrategyBreakdownAnalysisBlockClient"
analysis.class <- "StrategyBreakdownAnalysisBlock"

context(sprintf("Testing %s class.", tested.class))

client.class <- paste(tested.class,"Client",sep="")

valid.id   <- 101
valid.date <- "2016-09-01"
valid.function <- dated_three_monthly_lookback
# valid.function <- dated_twelve_monthly_lookback

valid.key_values <- valid.function(valid.id, valid.date)

test_that(sprintf("Can instantiate on %s class", client.class),{
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



####################################################
#
# AverageDownTradesAnalysisBlockClient Tests
#
####################################################

tested.class <- "AverageDownTradesAnalysisBlockClient"
analysis.class <- "AverageDownTradesAnalysisBlock"


context(sprintf("Testing %s class.", tested.class))

client.class <- paste(tested.class,"Client",sep="")

# valid.id   <- 11
# valid.date <- "2016-09-01"
# valid.function <- dated_three_monthly_lookback
valid.key_values <- valid.function(valid.id, valid.date)

test_that(sprintf("Can instantiate on %s class", client.class),{
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



####################################################
#
# AverageDownTradesFocusAnalysisBlockClient Tests
#
####################################################

tested.class <- "AverageDownTradesFocusAnalysisBlockClient"
analysis.class <- "AverageDownTradesFocusAnalysisBlock"


context(sprintf("Testing %s class.", tested.class))

client.class <- paste(tested.class,"Client",sep="")

# valid.id   <- 11
# valid.date <- "2016-09-01"
# valid.function <- dated_three_monthly_lookback
valid.key_values <- valid.function(valid.id, valid.date)

test_that(sprintf("Can instantiate on %s class", client.class),{
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


####################################################
#
# BuysAndSellsAnalysisBlockClient Tests
#
####################################################

tested.class <- "BuysAndSellsAnalysisBlockClient"
analysis.class <- "BuysAndSellsAnalysisBlock"


context(sprintf("Testing %s class.", tested.class))

client.class <- paste(tested.class,"Client",sep="")

# valid.id   <- 11
# valid.date <- "2016-09-01"
# valid.function <- dated_three_monthly_lookback
valid.key_values <- valid.function(valid.id, valid.date)

test_that(sprintf("Can instantiate on %s class", client.class),{
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



####################################################
#
# ExtendedTradesAnalysisBlockClient Tests
#
####################################################

tested.class <- "ExtendedTradesAnalysisBlockClient"
analysis.class <- "ExtendedTradesAnalysisBlock"


context(sprintf("Testing %s class.", tested.class))

client.class <- paste(tested.class,"Client",sep="")

# valid.id   <- 11
# valid.date <- "2016-09-01"
# valid.function <- dated_three_monthly_lookback
valid.key_values <- valid.function(valid.id, valid.date)

test_that(sprintf("Can instantiate on %s class", client.class),{
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




####################################################
#
# MarketReturnAnalysisBlockClient Tests
#
####################################################

tested.class <- "MarketReturnAnalysisBlockClient"
analysis.class <- "MarketReturnAnalysisBlock"


context(sprintf("Testing %s class.", tested.class))

client.class <- paste(tested.class,"Client",sep="")

# valid.id   <- 11
# valid.date <- "2016-09-01"
# valid.function <- dated_three_monthly_lookback
valid.key_values <- valid.function(valid.id, valid.date)

test_that(sprintf("Can instantiate on %s class", client.class),{
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


####################################################
#
# OffsidePositionsAnalysisBlockClient Tests
#
####################################################

tested.class <- "OffsidePositionsAnalysisBlockClient"
analysis.class <- "OffsidePositionsAnalysisBlock"


context(sprintf("Testing %s class.", tested.class))

client.class <- paste(tested.class,"Client",sep="")

# valid.id   <- 11
# valid.date <- "2016-09-01"
# valid.function <- dated_three_monthly_lookback
valid.key_values <- valid.function(valid.id, valid.date)

test_that(sprintf("Can instantiate on %s class", client.class),{
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

####################################################
#
# OffsidePositionsCumulativePnLAnalysisBlockClient Tests
#
####################################################

tested.class <- "OffsidePositionsCumulativePnLAnalysisBlockClient"
analysis.class <- "OffsidePositionsCumulativePnLAnalysisBlock"


context(sprintf("Testing %s class.", tested.class))

client.class <- paste(tested.class,"Client",sep="")

# valid.id   <- 11
# valid.date <- "2016-09-01"
# valid.function <- dated_three_monthly_lookback
valid.key_values <- valid.function(valid.id, valid.date)

test_that(sprintf("Can instantiate on %s class", client.class),{
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


####################################################
#
# PnLTradedInLongShortHedgeAnalysisBlockClient Tests
#
####################################################

tested.class <- "PnLTradedInLongShortHedgeAnalysisBlockClient"
analysis.class <- "PnLTradedInLongShortHedgeAnalysisBlock"


context(sprintf("Testing %s class.", tested.class))

client.class <- paste(tested.class,"Client",sep="")

# valid.id   <- 11
# valid.date <- "2016-09-01"
# valid.function <- dated_three_monthly_lookback
valid.key_values <- valid.function(valid.id, valid.date)

test_that(sprintf("Can instantiate on %s class", client.class),{
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


####################################################
#
# PortfolioFactorExposureAnalysisBlockClient Tests
#
####################################################

tested.class <- "PortfolioFactorExposureAnalysisBlockClient"
analysis.class <- "PortfolioFactorExposureAnalysisBlock"


context(sprintf("Testing %s class.", tested.class))

client.class <- paste(tested.class,"Client",sep="")

# valid.id   <- 11
# valid.date <- "2016-09-01"
# valid.function <- dated_three_monthly_lookback
valid.key_values <- valid.function(valid.id, valid.date)

test_that(sprintf("Can instantiate on %s class", client.class),{
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


####################################################
#
# PositionRevisitsAnalysisBlockClient Tests
#
####################################################

tested.class <- "PositionRevisitsAnalysisBlockClient"
analysis.class <- "PositionRevisitsAnalysisBlock"


context(sprintf("Testing %s class.", tested.class))

client.class <- paste(tested.class,"Client",sep="")

# valid.id   <- 11
# valid.date <- "2016-09-01"
# valid.function <- dated_three_monthly_lookback
valid.key_values <- valid.function(valid.id, valid.date)

test_that(sprintf("Can instantiate on %s class", client.class),{
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



####################################################
#
# PositionsHoldingPeriodAnalysisBlockClient Tests
#
####################################################

tested.class <- "PositionsHoldingPeriodAnalysisBlockClient"
analysis.class <- "PositionsHoldingPeriodAnalysisBlock"


context(sprintf("Testing %s class.", tested.class))

client.class <- paste(tested.class,"Client",sep="")

# valid.id   <- 11
# valid.date <- "2016-09-01"
# valid.function <- dated_three_monthly_lookback
valid.key_values <- valid.function(valid.id, valid.date)

test_that(sprintf("Can instantiate on %s class", client.class),{
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
