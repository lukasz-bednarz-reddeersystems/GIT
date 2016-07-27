context("Testing Transformation")

#########################
#
# Portfolio Tests
#
#########################

test_that("Canot create Portfolio object", {
  expect_error(new("Portfolio"))
})

test_that("Cannot create StrategyPortfolio object without trader_id", {
  expect_error(new("StrategyPortfolio"))
})

test_that("Can create StrategyPortfolio object", {
  expect_is(new("StrategyPortfolio",11), "Portfolio")
})

test_that("Can create StrategyPortfolio object", {
  expect_is(new("StrategyPortfolio",11), "StrategyPortfolio")
})


#########################
# Set/GetReferenceData
#########################

test_that("Can Set/GetReferenceData with Valid input", {
  portf <- new("StrategyPortfolio",11)

  expect_is(portf , "StrategyPortfolio")

  data <- data.frame(Strategy = "X_HEDGE",
                     TraderID = 11,
                     InstrumentID = 4454,
                     Date = "2016-05-05",
                     Weight = 1
                     )

  portf <- setReferenceData(portf, data)

  expect_equivalent(data, getReferenceData(portf))

})

test_that("Cannot affect underlying data", {
  portf <- new("StrategyPortfolio",11)

  expect_is(portf , "StrategyPortfolio")

  valid_data <- data.frame(Strategy = "X_HEDGE",
                     TraderID = 11,
                     InstrumentID = 4454,
                     Date = "2016-05-05",
                     Weight = 1
  )

  portf <- setReferenceData(portf, valid_data)

  expect_equivalent(valid_data, getReferenceData(portf))

  invalid_data <- data.frame(Strategy = "X_HEDGE",
                     TraderID = 11,
                     Date = "2016-05-05",
                     Weight = 1)
  portf@data <- invalid_data
  expect_error(validObject(portf))

})


test_that("Cannnot SetReferenceData with invalid input", {
  portf <- new("StrategyPortfolio",11)

  expect_is(portf , "StrategyPortfolio")

  invalid_data <- data.frame(Strategy = "X_HEDGE",
                     InstrumentID = 4454,
                     Date = "2016-05-05",
                     Weight = 1
  )

  expect_error(setReferenceData(portf, invalid_data))

  valid_data <- data.frame(Strategy = "X_HEDGE",
                     TraderID = 11,
                     InstrumentID = 4454,
                     Date = "2016-05-05",
                     Weight = 1)

  portf <- setReferenceData(portf, valid_data)
  expect_error(setReferenceData(portf, invalid_data))

  expect_equivalent(valid_data, getReferenceData(portf))

  invalid_data <- data.frame(Strategy = character(),
                             TraderID = numeric(),
                             InstrumentID = numeric(),
                             Date = as.Date(character()),
                             Weight = numeric())

  portf <- setReferenceData(portf, valid_data)
  expect_error(setReferenceData(portf, invalid_data))

  expect_equivalent(valid_data, getReferenceData(portf))


})


#########################
# updateVariables
#########################

test_that("Can updateVariables with valid input", {
  portf <- new("StrategyPortfolio",11)

  expect_is(portf , "StrategyPortfolio")

  valid_data <- data.frame(Strategy = "X_HEDGE",
                           TraderID = 11,
                           InstrumentID = 4454,
                           Date = "2016-05-05",
                           Weight = 1)

  portf <- setReferenceData(portf, valid_data)

  new_data <- data.frame(Strategy = "X_HEDGE",
                           TraderID = 11,
                           InstrumentID = 647,
                           Date = "2016-05-05",
                           Weight = 1,
                           MarketValue = 1999)

  portf <- updateVariables(portf, new_data, "InstrumentID")
  expect_equivalent(new_data$InstrumentID, getReferenceData(portf)$InstrumentID)

})

test_that("Can updateVariables with invalid input", {
  portf <- new("StrategyPortfolio",11)

  expect_is(portf , "StrategyPortfolio")

  valid_data <- data.frame(Strategy = "X_HEDGE",
                           TraderID = 11,
                           InstrumentID = 4454,
                           Date = "2016-05-05",
                           Weight = 1)

  portf <- setReferenceData(portf, valid_data)

  invalid_data <- data.frame(Strategy = "X_HEDGE",
                         TraderID = 11,
                         Date = "2016-05-05",
                         Weight = 1,
                         MarketValue = 1999)

  expect_error(portf <- updateVariables(portf, invalid_data, "InstrumentID"))
  expect_equivalent(valid_data$InstrumentID, getReferenceData(portf)$InstrumentID)

})

#########################
# appendVariable
#########################

test_that("Can appendVariable with valid input", {
  portf <- new("StrategyPortfolio",11)

  expect_is(portf , "StrategyPortfolio")

  valid_data <- data.frame(Strategy = "X_HEDGE",
                           TraderID = 11,
                           InstrumentID = 4454,
                           Date = "2016-05-05",
                           Weight = 1)

  portf <- setReferenceData(portf, valid_data)

  new_data <- data.frame(Strategy = "X_HEDGE",
                         TraderID = 11,
                         InstrumentID = 647,
                         Date = "2016-05-05",
                         Weight = 1,
                         MarketValue = 1999)

  portf <- appendVariables(portf, new_data, "MarketValue")
  expect_equivalent(new_data$MarketValue, getReferenceData(portf)$MarketValue)

  new_data <- data.frame(Strategy = "X_HEDGE",
                         TraderID = 11,
                         InstrumentID = 647,
                         Date = "2016-05-05",
                         Weight = 1,
                         MarketValue = 2500)

  portf <- appendVariables(portf, new_data, "MarketValue")
  expect_equivalent(new_data$MarketValue, getReferenceData(portf)$MarketValue)

})

test_that("Cannot appendVariable with invalid input", {
  portf <- new("StrategyPortfolio",11)

  expect_is(portf , "StrategyPortfolio")

  valid_data <- data.frame(Strategy = "X_HEDGE",
                           TraderID = 11,
                           InstrumentID = 4454,
                           Date = "2016-05-05",
                           Weight = 1)

  portf <- setReferenceData(portf, valid_data)

  new_data <- data.frame(Strategy = "X_HEDGE",
                         TraderID = 11,
                         InstrumentID = 647,
                         Date = "2016-05-05",
                         Weight = 1,
                         MarketValue = 1999)

  portf <- appendVariables(portf, new_data, "MarketValue")
  expect_equivalent(new_data$MarketValue, getReferenceData(portf)$MarketValue)

  invalid_data <- data.frame(Strategy = "X_HEDGE",
                         TraderID = 11,
                         InstrumentID = 647,
                         Date = "2016-05-05",
                         Weight = 1)

  expect_error(portf <- appendVariables(portf, invalid_data, "MarketValue"))
  expect_equivalent(new_data$MarketValue, getReferenceData(portf)$MarketValue)

  invalid_data <- data.frame(Strategy = character(),
                             TraderID = numeric(),
                             InstrumentID = numeric(),
                             Date = as.Date(character()),
                             Weight = numeric(),
                             MarketValue = numeric())

  expect_error(portf <- appendVariables(portf, invalid_data, "MarketValue"))
  expect_equivalent(new_data$MarketValue, getReferenceData(portf)$MarketValue)

})



#########################
# buildPortfolioHistory
#########################

test_that("Can buildPortfolioHistory", {

  skip_if_not(getOption("RunLongTests"))

  portf <- new("StrategyPortfolio", trader = 11)
  expect_is(portf, "StrategyPortfolio")
  expect_equal(getTraderID(portf),11)

  start <- as.Date("2016-04-01")
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

#########################
# attachTransformations
#########################

if (getOption("RunLongTests")) {
  portf <- new("StrategyPortfolio", trader = 11)
  start <- as.Date("2016-04-01")
  end <- as.Date("2016-05-04")
  portf <- buildPortfolioHistory(portf, start, end)
  portf.data <- getReferenceData(portf)
  tested.transf <- "DaysSinceLastFlatTransformation"
}

test_that("Can attachTransformations", {

  skip_if_not(getOption("RunLongTests"))

  portf <- new("StrategyPortfolio", trader = 11)
  expect_is(portf, "StrategyPortfolio")
  expect_equal(getTraderID(portf),11)
  portf <- setReferenceData(portf, portf.data)
  expect_equal(getReferenceData(portf), portf.data)

  transf <- new(tested.transf, NULL)

  prev.data <- getReferenceData(portf)
  portf <- attachTransformation(portf, transf)

  expect_equal(isTransformationAttached(portf, transf), TRUE)

  required.vars <- getRequiredVariablesNames(portf)
  stored.vars <- getStoredVariablesNames(portf)
  transf.vars <- getOutputVariablesNames(transf)

  expect_length(intersect(stored.vars,transf.vars), length(transf.vars) )
  expect_equal(getStoredNRows(portf), nrow(portf.data))
  expect_equal(getReferenceData(portf), days_since_last_flat(prev.data))
  expect_equal(colnames(getReferenceData(portf)), colnames(days_since_last_flat(prev.data)))

})

