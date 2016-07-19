sourceTo("../transformations/virtual_transformation.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../transformations/transformation_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(testthat)


##################################
#
# VirtualTransformationComputation
#
##################################

test_that("Canot create VirtualTransformationComputation object", {
  expect_error(new("VirtualTransformationComputation"))
})


##################################
#
# TestTransformationComputation
#
##################################

test_that("Can create TestTransformationComputation", {
  comp <- (new("TestTransformationComputation"))
  expect_is(comp, "TestTransformationComputation")
})


test_that("Can getRequiredVariablesNames", {
  comp <- (new("TestTransformationComputation"))
  expect_is(comp, "TestTransformationComputation")
  
  req.vars <- c("A", "B", "C")
  
  expect_equal(getRequiredVariablesNames(comp), req.vars)
  
})


test_that("Can getComputedVariablesNames", {
  comp <- (new("TestTransformationComputation"))
  expect_is(comp, "TestTransformationComputation")
  
  req.vars <- c("A", "B", "C")
  
  expect_equal(getComputedVariablesNames(comp), character())
  
})


test_that("Can getOutputVariablesNames", {
  comp <- (new("TestTransformationComputation"))
  expect_is(comp, "TestTransformationComputation")
  
  req.vars <- c("A", "B", "C")
  
  expect_equal(getOutputVariablesNames(comp), req.vars)
  
})

test_that("Can setInputData with valid input", {
  comp <- (new("TestTransformationComputation"))
  expect_is(comp, "TestTransformationComputation")
  
  valid.data <- data.frame(A = 1,
                           B = 2,
                           C = 3)
  
  comp <- setInputData(comp, valid.data)
  expect_equal(getInputData(comp), valid.data)
  
  valid.data <- data.frame(A = 1,
                           B = 2,
                           C = 3,
                           D = 4)
  
  comp <- setInputData(comp, valid.data)
  expect_equal(getInputData(comp), valid.data)
  
})

test_that("Cannot setInputData with invalid input", {
  comp <- (new("TestTransformationComputation"))
  
  expect_is(comp, "TestTransformationComputation")
  
  valid.data <- data.frame(A = 1,
                           B = 2,
                           C = 3)
  
  comp <- setInputData(comp, valid.data)
  expect_equal(getInputData(comp), valid.data)
  
  invalid.data <- data.frame(B = 2,
                           C = 3,
                           D = 4)
  
  expect_error(setInputData(comp, invalid.data))
  expect_equal(getInputData(comp), valid.data)
  
  invalid.data <- data.frame(B = numeric(),
                             C = numeric(),
                             A = numeric())
  
  expect_error(setInputData(comp, invalid.data))
  expect_equal(getInputData(comp), valid.data)
  
})

test_that("Can setOutputData with valid input", {
  comp <- (new("TestTransformationComputation"))
  expect_is(comp, "TestTransformationComputation")
  
  valid.data <- data.frame(A = 1,
                           B = 2,
                           C = 3)
  
  comp <- setOutputData(comp, valid.data)
  expect_equal(getOutputData(comp), valid.data)
  
  valid.data <- data.frame(A = 1,
                           B = 2,
                           C = 3,
                           D = 4)
  
  comp <- setOutputData(comp, valid.data)
  expect_equal(getOutputData(comp), valid.data)
  
})

test_that("Cannot setOutputData with invalid input TestTransformationComputation", {
  comp <- (new("TestTransformationComputation"))
  expect_is(comp, "TestTransformationComputation")
  
  valid.data <- data.frame(A = 1,
                           B = 2,
                           C = 3)
  
  comp <- setOutputData(comp, valid.data)
  expect_equal(getOutputData(comp), valid.data)
  
  invalid.data <- data.frame(B = 2,
                             C = 3,
                             D = 4)
  
  expect_error(setOutputData(comp, invalid.data))
  expect_equal(getOutputData(comp), valid.data)
  
  invalid.data <- data.frame(B = numeric(),
                             C = numeric(),
                             A = numeric())
  
  expect_error(setOutputData(comp, invalid.data))
  expect_equal(getOutputData(comp), valid.data)
  
})

test_that("Can computeTransformation on TestTransformationComputation", {
  comp <- (new("TestTransformationComputation"))
  expect_is(comp, "TestTransformationComputation")
  
  valid.data <- data.frame(A = 1,
                           B = 2,
                           C = 3)
  
  comp <- setInputData(comp, valid.data)
  expect_equal(getInputData(comp), valid.data)
  
  comp <- computeTransformation(comp)
  expect_equal(getOutputData(comp), valid.data)
  
  comp <- (new("RowMeansTransformationComputation"))
  expect_is(comp, "RowMeansTransformationComputation")
  
  
  req.vars <- c("A", "B", "C", "RowMean")
  expect_equal(getComputedVariablesNames(comp), "RowMean")
  expect_equal(getOutputVariablesNames(comp), req.vars)
  
  valid.data <- data.frame(A = 1,
                           B = 2,
                           C = 3)
  
  
  
  comp <- setInputData(comp, valid.data)
  expect_equal(getInputData(comp), valid.data)
  
  comp <- computeTransformation(comp)
  expect_equal(getOutputData(comp), row_mean_computation(valid.data))
  
})

##################################
#
# VirtualTransformation
#
##################################


test_that("Canot create VirtualTransformation object", {
  expect_error(new("VirtualTransformation"))
})


test_that("Canot create VirtualTransformation object", {
  expect_error(new("VirtualTransformation"))
})


##################################
#
# TestTransformation
#
##################################


test_that("Can create TestTransformation", {
  trans <- (new("TestTransformation"))
  expect_is(trans, "TestTransformation")
})


test_that("Can getRequiredVariablesNames", {
  trans <- (new("TestTransformationComputation"))
  expect_is(trans, "TestTransformationComputation")
  
  req.vars <- c("A", "B", "C")
  
  expect_equal(getRequiredVariablesNames(trans), req.vars)
  
})


test_that("Can getComputedVariablesNames", {
  trans <- (new("TestTransformationComputation"))
  expect_is(trans, "TestTransformationComputation")
  
  req.vars <- c("A", "B", "C")
  
  expect_equal(getComputedVariablesNames(trans), character())
  
})


test_that("Can getOutputVariablesNames", {
  trans <- (new("TestTransformationComputation"))
  expect_is(trans, "TestTransformationComputation")
  
  req.vars <- c("A", "B", "C")
  
  expect_equal(getOutputVariablesNames(trans), req.vars)
  
})

test_that("Can setComputationInput with valid input", {
  trans <- (new("TestTransformation"))
  expect_is(trans, "TestTransformation")
  
  valid.data <- data.frame(A = 1,
                           B = 2,
                           C = 3)
  
  trans <- setComputationInput(trans, valid.data)
  comp <- getComputation(trans)
  expect_is(comp, "TestTransformationComputation")
  
  expect_equal(getInputData(comp), valid.data)
  
  valid.data <- data.frame(A = 1,
                           B = 2,
                           C = 3,
                           D = 4)
  
  trans <- setComputationInput(trans, valid.data)
  comp <- getComputation(trans)
  expect_is(comp, "TestTransformationComputation")
  
  expect_equal(getInputData(comp), valid.data)
  
  
})

test_that("Cannot setComputationInput with invalid input", {
  
  trans <- (new("TestTransformation"))
  expect_is(trans, "TestTransformation")
  
  valid.data <- data.frame(A = 1,
                           B = 2,
                           C = 3)
  
  trans <- setComputationInput(trans, valid.data)
  comp <- getComputation(trans)
  expect_is(comp, "TestTransformationComputation")
  
  expect_equal(getInputData(comp), valid.data)
  
  
  invalid.data <- data.frame(B = 2,
                             C = 3,
                             D = 4)
  
  expect_error(setComputationInput(trans, invalid.data))
  
  comp <- getComputation(trans)
  expect_is(comp, "TestTransformationComputation")
  expect_equal(getInputData(comp), valid.data)
  
  invalid.data <- data.frame(B = numeric(),
                             C = numeric(),
                             A = numeric())
  
  expect_error(setComputationInput(trans, invalid.data))
  
  comp <- getComputation(trans)
  expect_is(comp, "TestTransformationComputation")
  expect_equal(getInputData(comp), valid.data)
  
})



test_that("Can triggerComputation on TestTransformation", {
  
  trans <- (new("TestTransformation"))
  expect_is(trans, "TestTransformation")
  
  valid.data <- data.frame(A = 1,
                           B = 2,
                           C = 3)
  
  trans <- setComputationInput(trans, valid.data)
  comp <- getComputation(trans)
  expect_is(comp, "TestTransformationComputation")
  expect_equal(getInputData(comp), valid.data)
  
  trans <- triggerComputation(trans)
  comp <- getComputation(trans)
  expect_is(comp, "TestTransformationComputation")
  expect_equal(getComputationOutput(trans), valid.data)
  
  
  expect_message(triggerComputation(trans), regexp = "Skipping ... ")
  trans <- triggerComputation(trans)
  comp <- getComputation(trans)
  expect_is(comp, "TestTransformationComputation")
  expect_equal(getComputationOutput(trans), valid.data)
  
  expect_message(triggerComputation(trans, TRUE), regexp = "Triggering Transformation computation:")
  trans <- triggerComputation(trans, TRUE)
  comp <- getComputation(trans)
  expect_is(comp, "TestTransformationComputation")
  expect_equal(getComputationOutput(trans), valid.data)
  
  trans <- (new("RowMeansTransformation"))
  expect_is(trans, "RowMeansTransformation")
  comp <- getComputation(trans)
  expect_is(comp, "RowMeansTransformationComputation")
  
  
  req.vars <- c("A", "B", "C", "RowMean")
  expect_equal(getComputedVariablesNames(trans), "RowMean")
  expect_equal(getOutputVariablesNames(trans), req.vars)
  
  valid.data <- data.frame(A = 1,
                           B = 2,
                           C = 3)
  
  
  trans <- setComputationInput(trans, valid.data)
  comp <- getComputation(trans)
  expect_is(comp, "RowMeansTransformationComputation")
  expect_equal(getInputData(comp), valid.data)
  
  trans <- triggerComputation(trans)
  comp <- getComputation(trans)
  expect_is(comp, "RowMeansTransformationComputation")
  expect_equal(getComputationOutput(trans), row_mean_computation(valid.data))
  
})


test_that("Cannot triggerComputation on InvalidTestTransformation", {
  
  trans <- (new("InvalidRowMeansTransformation"))
  expect_is(trans, "InvalidRowMeansTransformation")
  comp <- getComputation(trans)
  expect_is(comp, "InvalidRowMeansTransformationComputation")
  
  
  req.vars <- c("A", "B", "C", "RowMean")
  expect_equal(getComputedVariablesNames(trans), "RowMean")
  expect_equal(getOutputVariablesNames(trans), req.vars)
  
  valid.data <- data.frame(A = 1,
                           B = 2,
                           C = 3)
  
  
  trans <- setComputationInput(trans, valid.data)
  comp <- getComputation(trans)
  expect_is(comp, "InvalidRowMeansTransformationComputation")
  expect_equal(getInputData(comp), valid.data)
  
  expect_error( triggerComputation(trans) )
  comp <- getComputation(trans)
  expect_is(comp, "InvalidRowMeansTransformationComputation")
  expect_error(getComputationOutput(trans), NULL)
  
})



test_that("Can setComputation on TestTransformation with valid input", {
  
  
  trans <- (new("TestTransformation"))
  expect_is(trans, "TestTransformation")
  comp <- getComputation(trans)
  expect_is(comp, "TestTransformationComputation")
  
  comp <- new("RowMeansTransformationComputation")
  trans <- setComputation(trans, comp)
  comp <- getComputation(trans)
  expect_is(comp, "RowMeansTransformationComputation")
  
  valid.data <- data.frame(A = 1,
                           B = 2,
                           C = 3)
  
  
  trans <- setComputationInput(trans, valid.data)
  trans <- triggerComputation(trans)
  expect_equal(getComputationOutput(trans), row_mean_computation(valid.data))
  
  

  trans <- (new("RowMeansTransformation"))
  expect_is(trans, "RowMeansTransformation")
  comp <- getComputation(trans)
  expect_is(comp, "RowMeansTransformationComputation")
  
  comp <- new("InvalidRowMeansTransformationComputation")
  trans <- setComputation(trans, comp)
  comp <- getComputation(trans)
  expect_is(comp, "InvalidRowMeansTransformationComputation")
  
  
})



test_that("Cannot setComputation on TestTransformation with invalid input", {

  expect_error(setComputation(trans, trans))
  

  trans <- (new("RowMeansTransformation"))
  expect_is(trans, "RowMeansTransformation")
  comp <- getComputation(trans)
  expect_is(comp, "RowMeansTransformationComputation")
  
  comp <- new("TestTransformationComputation")
  
  expect_error(setComputation(trans, comp))
  

})

#############################################################
#
# Portfolio Transformations tests
#
#############################################################

portf <- new("StrategyPortfolio", trader = 11)
portf2 <- new("StrategyPortfolio", trader = 11)

test_that("Can buildPortfolioHistory", {
  expect_is(portf, "StrategyPortfolio")
  expect_equal(getTraderID(portf),11)
  
  start <- as.Date("2016-01-01")
  end <- as.Date("2016-05-04")
  
  portf <- buildPortfolioHistory(portf, start, end)
  expect_equal(getStartDate(portf), start)
  expect_equal(getEndDate(portf), end)
  
  required_colnms <- getRequiredVariablesNames(portf)
  stored_colnms <- getStoredVariablesNames(portf)
  
  expect_length(stored_colnms[(stored_colnms %in% required_colnms)], length(required_colnms))
  expect_gt(nrow(getReferenceData(portf)),0)
  expect_true (min(getReferenceData(portf)$Date) >= start)
  expect_true(max(getReferenceData(portf)$Date) <= end)
  
})



#############################################################
#
# DaysSinceLastFlatTransformationComputation tests
#
#############################################################
start <- as.Date("2016-04-01")
end <- as.Date("2016-05-04")
portf <- buildPortfolioHistory(portf, start, end)
portf.data <- getReferenceData(portf)
portf2 <- setReferenceData(portf2, getReferenceData(portf)[1:100,])
tested.class <- "DaysSinceLastFlatTransformationComputation"
req.vars <- c('Date','InstrumentID','Weight')
comp.vars <- c('DaysSinceLastFlat')
outp.vars <- c(req.vars,comp.vars)
tested.function <- days_since_last_flat

test_that(paste("Can create", tested.class ,"object"), {
  
  comp <- new(tested.class)
  expect_is(comp, tested.class)

})


test_that(paste("Can getRequiredVariablesNames on", tested.class,"class"), {
  comp <- new(tested.class)
  expect_is(comp, tested.class)
  expect_equal(getRequiredVariablesNames(comp), req.vars)
  
})


test_that(paste("Can getComputedVariablesNames on", tested.class,"class"), {
  comp <- new(tested.class)
  expect_is(comp, tested.class)
  expect_equal(getComputedVariablesNames(comp), comp.vars)
  
})


test_that(paste("Can getOutputVariablesNames on", tested.class,"class"), {
  comp <- new(tested.class)
  expect_is(comp, tested.class)
  expect_equal(getOutputVariablesNames(comp), outp.vars)
  
})

test_that(paste("Can setInputData with valid input on", tested.class,"class"), {
  comp <- new(tested.class)
  expect_is(comp, tested.class)
  
  valid.data <- portf.data
  
  comp <- setInputData(comp, valid.data)
  expect_equal(getInputData(comp), valid.data)
  
  valid.data <- cbind(portf.data, tested.function(portf.data))
  
  comp <- setInputData(comp, valid.data)
  expect_equal(getInputData(comp), valid.data)
  
})

test_that(paste("Cannot setInputData with invalid input on", tested.class,"class"), {
  comp <- new(tested.class)
  expect_is(comp, tested.class)
  
  valid.data <- portf.data
  
  comp <- setInputData(comp, valid.data)
  expect_equal(getInputData(comp), valid.data)
  
  invalid.data <- data.frame(B = 2,
                             C = 3,
                             D = 4)
  
  expect_error(setInputData(comp, invalid.data))
  expect_equal(getInputData(comp), valid.data)
  
  invalid.data <- portf.data[0,]
  
  expect_error(setInputData(comp, invalid.data))
  expect_equal(getInputData(comp), valid.data)
  
})

test_that(paste("Can setOutputData with valid data on", tested.class,"class"), {
  comp <- new(tested.class)
  expect_is(comp, tested.class)
  
  valid.data <- cbind(portf.data, tested.function(portf.data))
  
  comp <- setOutputData(comp, valid.data)
  expect_equal(getOutputData(comp), valid.data)
  
  valid.data <- cbind(portf.data, tested.function(portf.data), data.frame(Ones = 1))
  
  comp <- setOutputData(comp, valid.data)
  expect_equal(getOutputData(comp), valid.data)
  
})

test_that(paste("Cannot setOutputData with invalid input on", tested.class,"class"), {
  comp <- new(tested.class)
  expect_is(comp, tested.class)
  
  valid.data <- tested.function(portf.data)
  
  comp <- setOutputData(comp, valid.data)
  expect_equal(getOutputData(comp), valid.data)
  
  invalid.data <- portf.data
  
  expect_error(setOutputData(comp, invalid.data))
  expect_equal(getOutputData(comp), valid.data)
  
  invalid.data <- valid.data[0,]
  
  expect_error(setOutputData(comp, invalid.data))
  expect_equal(getOutputData(comp), valid.data)
  
})

test_that(paste("Can computeTransformation on", tested.class,"class"), {
  comp <- new(tested.class)
  expect_is(comp, tested.class)
  
  valid.data <- portf.data
  valid.output <- tested.function(portf.data)
  
  comp <- setInputData(comp, valid.data)
  expect_equal(getInputData(comp), valid.data)
  expect_equal(isComputed(comp), FALSE)
  
  comp <- computeTransformation(comp)
  expect_equal(getOutputData(comp), valid.output)
  
  comp <- new(tested.class)
  expect_is(comp, tested.class)

})


#############################################################
#
# DaysSinceLastFlatTransformation tests
#
#############################################################

tested.class <- "DaysSinceLastFlatTransformation"
tested.class.2 <- "DaysSinceLastFlatTransformationComputation"
req.vars <- c('Date','InstrumentID','Weight')
comp.vars <- c('DaysSinceLastFlat')
outp.vars <- c(req.vars,comp.vars)
tested.function <- days_since_last_flat


test_that(paste("Canot create", tested.class ,"object with invalid argument"), {
  
  expect_error(new(tested.class))
  expect_error(new(tested.class, data.frame()))
  
  # setting new computation with  new portfolio with empty data
  expect_error(new(tested.class, new("StrategyPortfolio", 11)))
})

test_that(paste("Can create", tested.class ,"object with valid argument"), {
  
  transf <- new(tested.class, NULL)
  expect_is(transf, tested.class)
  
  transf <- new(tested.class, portf)
  expect_is(transf, tested.class)
  
  expect_equal(getPortfolio(transf), portf)
  expect_equal(getInputData(getComputation(transf)), getReferenceData(portf))
  
})


test_that(paste("Can getRequiredVariablesNames on", tested.class,"class"), {
  transf <- new(tested.class, portf)
  expect_is(transf, tested.class)
  expect_equal(getRequiredVariablesNames(transf), req.vars)
  
})


test_that(paste("Can getComputation on", tested.class,"class"), {
  transf <- new(tested.class, portf)
  expect_is(transf, tested.class)
  expect_equal(getRequiredVariablesNames(transf), req.vars)
  
  comp <- getComputation(transf)
  expect_is(comp, tested.class.2)
  
})


test_that(paste("Can setComputation on", tested.class,"class with Valid input"), {
  transf <- new(tested.class, portf)
  expect_is(transf, tested.class)
  expect_equal(getRequiredVariablesNames(transf), req.vars)
  
  comp <- getComputation(transf)
  expect_is(comp, tested.class.2)
  
  transf <- setComputation(transf, new(tested.class.2))
  comp <- getComputation(transf)
  expect_is(comp, tested.class.2)
  
  expect_equal(getInputData(comp), getReferenceData(portf))
  
})



test_that(paste("Can setComputationInput on", tested.class,"class with Valid input"), {
  transf <- new(tested.class, portf)
  expect_is(transf, tested.class)
  expect_equal(getRequiredVariablesNames(transf), req.vars)
  
  comp <- getComputation(transf)
  expect_is(comp, tested.class.2)
  
  transf <- setComputationInput(transf, getReferenceData(portf2))
  comp <- getComputation(transf)
  expect_is(comp, tested.class.2)
  
  expect_equal(getInputData(comp), getReferenceData(portf2))
  
})


test_that(paste("Cannot setComputationInput on", tested.class,"class with invalid input"), {
  transf <- new(tested.class, portf)
  expect_is(transf, tested.class)
  expect_equal(getRequiredVariablesNames(transf), req.vars)
  
  comp <- getComputation(transf)
  expect_is(comp, tested.class.2)
  
  expect_error(setComputationInput(transf, getReferenceData(portf2)[0,]))
  comp <- getComputation(transf)
  expect_is(comp, tested.class.2)
  
  expect_equal(getInputData(comp), getReferenceData(portf))
  expect_equal(getInputData(getComputation(transf)), getReferenceData(portf))
  
  expect_error(setComputationInput(transf, getReferenceData(portf2)[, match()]))
  comp <- getComputation(transf)
  expect_is(comp, tested.class.2)
  
  expect_equal(getInputData(comp), getReferenceData(portf))
  expect_equal(getInputData(getComputation(transf)), getReferenceData(portf))
  
})




test_that(paste("Can setPortfolio on", tested.class ,"object with valid argument"), {
  
  transf <- new(tested.class, portf)
  expect_is(transf, tested.class)
  
  expect_equal(getPortfolio(transf), portf)
  expect_equal(getInputData(getComputation(transf)), getReferenceData(portf))
  
  transf <- setPortfolio(transf,portf2)
  
  expect_equal(getInputData(getComputation(transf)), getReferenceData(portf2))
 
})



test_that(paste("Cannot setPortfolio on", tested.class ,"object with invalid argument"), {
  
  transf <- new(tested.class, portf)
  expect_is(transf, tested.class)
  
  expect_equal(getPortfolio(transf), portf)
  expect_equal(getInputData(getComputation(transf)), getReferenceData(portf))
  
  expect_error(setPortfolio(transf,new("StrategyPortfolio",11)))
  
  expect_equal(getInputData(getComputation(transf)), getReferenceData(portf))
  
  
})



test_that(paste("Can triggerComputation on", tested.class ), {
  
  transf <- new(tested.class, portf)
  expect_is(transf, tested.class)
  
  expect_equal(getPortfolio(transf), portf)
  expect_equal(getInputData(getComputation(transf)), getReferenceData(portf))
  
  expect_message(triggerComputation(transf), "Triggering Transformation")
  transf <- triggerComputation(transf)
  
  expect_equal(isComputed(getComputation(transf)), TRUE)
  expect_equal(isComputed(transf), TRUE)
  expect_equal(getComputationOutput(transf), tested.function(portf.data))
  
  expect_message(triggerComputation(transf), "Skipping")
  transf <- triggerComputation(transf)
  
  expect_equal(isComputed(getComputation(transf)), TRUE)
  expect_equal(isComputed(transf), TRUE)
  expect_equal(getComputationOutput(transf), tested.function(portf.data))
  
  expect_message(triggerComputation(transf, TRUE), "Triggering Transformation")
  transf <- triggerComputation(transf, TRUE)
  
  expect_equal(isComputed(getComputation(transf)), TRUE)
  expect_equal(isComputed(transf), TRUE)
  expect_equal(getComputationOutput(transf), tested.function(portf.data))
  
  expect_equal(isComputed(getComputation(transf)), TRUE)
  expect_equal(isComputed(transf), TRUE)
  expect_equal(getComputationOutput(transf), tested.function(portf.data))
  
  expect_message(triggerComputation(transf, FALSE), "Skipping")
  transf <- triggerComputation(transf, FALSE)
  
  expect_equal(isComputed(getComputation(transf)), TRUE)
  expect_equal(isComputed(transf), TRUE)
  expect_equal(getComputationOutput(transf), tested.function(portf.data))

  
})
