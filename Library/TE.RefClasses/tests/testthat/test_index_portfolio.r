context("Testing IndexPortfolio")

#########################
#
# Portfolio Tests
#
#########################
tested.class <- "IndexPortfolio.BE500"
valid.ticker <- "BE500 Index"



test_that(paste("Can create", tested.class, "object"), {
  expect_is(new(tested.class), tested.class)
})

test_that("Can retrieve valid ticker", {
  object <- new(tested.class)

  expect_equal(getIndexTicker(object), valid.ticker)

})

#########################
# Set/GetReferenceData
#########################

test_that("Can Set/GetReferenceData with Valid input", {
  object <- new(tested.class)

  expect_is(object , tested.class)

  data <- data.frame(InstrumentID = 4454,
                     Date = "2016-05-05",
                     InstrumentName = "LUN DC Equity",
                     Weight = 1
                     )

  object <- setReferenceData(object, data)

  expect_equivalent(data, getReferenceData(object))

})


test_that("Cannnot setReferenceData with invalid input", {
  object <- new(tested.class)

  expect_is(object , tested.class)

  invalid_data <- data.frame(Date = "2016-05-05",
                     InstrumentName = "LUN DC Equity",
                     Weight = 1
  )

  expect_error(setReferenceData(object, invalid_data))

  valid_data <- data.frame(InstrumentID = 4454,
                     Date = "2016-05-05",
                     InstrumentName = "LUN DC Equity",
                     Weight = 1
  )

  object <- setReferenceData(object, valid_data)
  expect_error(setReferenceData(object, invalid_data))

  expect_equivalent(valid_data, getReferenceData(object))

  invalid_data <- data.frame(InstrumentID = integer(),
                             Date = as.Date(character()),
                             InstrumentName = character(),
                             Weight = numeric()
  )

  object <- setReferenceData(object, valid_data)
  expect_error(setReferenceData(object, invalid_data))

  expect_equivalent(valid_data, getReferenceData(object))


})


#########################
# updateVariables
#########################

test_that("Can updateVariables with valid input", {
  object <- new(tested.class)

  expect_is(object , tested.class)

  valid_data <- data.frame(InstrumentID = 4454,
                           Date = "2016-05-05",
                           InstrumentName = "LUN DC Equity",
                           Weight = 1)


  object <- setReferenceData(object, valid_data)

  new_data <- data.frame(InstrumentID = 44541,
                         Date = "2016-05-05",
                         InstrumentName = "LUN DC Equity",
                         Weight = 1,
                         MarketValue = 1999)

  object <- updateVariables(object, new_data, "InstrumentID")
  expect_equivalent(new_data$InstrumentID, getReferenceData(object)$InstrumentID)

})


test_that("Cannot updateVariables with invalid input", {
  object <- new(tested.class)

  expect_is(object , tested.class)

  valid_data <- data.frame(InstrumentID = 4454,
                           Date = "2016-05-05",
                           InstrumentName = "LUN DC Equity",
                           Weight = 1)

  object <- setReferenceData(object, valid_data)

  invalid_data <- data.frame(Date = "2016-05-05",
                                InstrumentName = "LUN DC Equity",
                                Weight = 1,
                                MarketValue = 1999)

  expect_error(object <- updateVariables(object, invalid_data, "InstrumentID"))
  expect_equivalent(valid_data$InstrumentID, getReferenceData(object)$InstrumentID)

})

#########################
# appendVariable
#########################

test_that("Can appendVariable with valid input", {
  object <- new(tested.class)

  expect_is(object , tested.class)

  valid_data <- data.frame(InstrumentID = 4454,
                           Date = "2016-05-05",
                           InstrumentName = "LUN DC Equity",
                           Weight = 1)

  object <- setReferenceData(object, valid_data)

  new_data <- data.frame(InstrumentID = 4454,
                         Date = "2016-05-05",
                         InstrumentName = "LUN DC Equity",
                         Weight = 1,
                          MarketValue = 1999)

  object <- appendVariables(object, new_data, "MarketValue")
  expect_equivalent(new_data$MarketValue, getReferenceData(object)$MarketValue)

  new_data <- data.frame(InstrumentID = 4454,
                         Date = "2016-05-05",
                         InstrumentName = "LUN DC Equity",
                         Weight = 1,
                         MarketValue = 2500)

  object <- appendVariables(object, new_data, "MarketValue")
  expect_equivalent(new_data$MarketValue, getReferenceData(object)$MarketValue)

})

test_that("Cannot appendVariable with invalid input", {
  object <- new(tested.class)

  expect_is(object , tested.class)

  valid_data <- data.frame(InstrumentID = 4454,
                           Date = "2016-05-05",
                           InstrumentName = "LUN DC Equity",
                           Weight = 1)

  object <- setReferenceData(object, valid_data)

  new_data <- data.frame(InstrumentID = 4454,
                         Date = "2016-05-05",
                         InstrumentName = "LUN DC Equity",
                         Weight = 1,
                         MarketValue = 1999)

  object <- appendVariables(object, new_data, "MarketValue")
  expect_equivalent(new_data$MarketValue, getReferenceData(object)$MarketValue)

  invalid_data <- data.frame(InstrumentID = 4454,
                             Date = "2016-05-05",
                             InstrumentName = "LUN DC Equity",
                             Weight = 1)

  expect_error(object <- appendVariables(object, invalid_data, "MarketValue"))
  expect_equivalent(new_data$MarketValue, getReferenceData(object)$MarketValue)

  invalid_data <- data.frame(InstrumentID = integer(),
                             Date = as.Date(character()),
                             InstrumentName = character(),
                             Weight = numeric(),
                             MarketValue = numeric())

  expect_error(object <- appendVariables(object, invalid_data, "MarketValue"))
  expect_equivalent(new_data$MarketValue, getReferenceData(object)$MarketValue)

})



#########################
# dataRequest
#########################

test_that("Cannot dataRequest with invalid data", {

  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)
  expect_is(object, tested.class)

  start <- as.Date("2016-05-01")
  end <- as.Date("2016-05-04")

  invalid.keys <- data.frame(IndexTicker = "SPX 50", start, end)

  expect_error(dataRequest(object, invalid.keys), regexp = "Invalid query passed to dataRequest()")

})

test_that("Can dataRequest", {

  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)
  expect_is(object, tested.class)

  start <- as.Date("2016-05-01")
  end <- as.Date("2016-05-04")

  valid.keys <- data.frame(IndexTicker = getIndexTicker(object), start, end)

  object <- dataRequest(object, valid.keys)

  expect_equal(getStartDate(object), start)
  expect_equal(getEndDate(object), end)

  required_colnms <- getRequiredVariablesNames(object)
  stored_colnms <- getStoredVariablesNames(object)

  expect_length(stored_colnms[(stored_colnms %in% required_colnms)], length(required_colnms))
  expect_gt(nrow(getReferenceData(object)),0)
  expect_true (min(getReferenceData(object)$Date) >= start)
  expect_true(max(getReferenceData(object)$Date) <= end)

})



#########################
# buildPortfolioHistory
#########################

test_that("Can buildPortfolioHistory", {

  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)
  expect_is(object, tested.class)

  start <- as.Date("2016-05-01")
  end <- as.Date("2016-05-04")

  object <- buildPortfolioHistory(object, start, end)
  expect_equal(getStartDate(object), start)
  expect_equal(getEndDate(object), end)

  required_colnms <- getRequiredVariablesNames(object)
  stored_colnms <- getStoredVariablesNames(object)

  expect_length(stored_colnms[(stored_colnms %in% required_colnms)], length(required_colnms))
  expect_gt(nrow(getReferenceData(object)),0)
  expect_true (min(getReferenceData(object)$Date) >= start)
  expect_true(max(getReferenceData(object)$Date) <= end)

})

