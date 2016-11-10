

context("Testing ImpliedFactorReturnsState")

#############################
#
# ImpliedFactorReturnsState Tests
#
#############################

tested.class          <-  "ImpliedFactorReturnsState"
valid.component       <- "ImpliedFactorReturns"
valid.risk_model      <- "RiskModel.DevelopedEuropePrototype150.1.1"
valid.model_prefix    <- "developed_europe_prototype"
valid.risk_model_obj  <- new(valid.risk_model)
valid.model_factors   <- getRiskModelFactorNames(valid.risk_model_obj)
valid.model_prefix    <- getRiskModelPrefix(valid.risk_model_obj)
valid.lookback        <- getRiskModelLookback(valid.risk_model_obj)

valid.key_cols        <- c(risk_model_objectstore_keys)
valid.values          <- c("Date",
                           valid.model_factors)
valid.required_colnms <- c('Date',
                           valid.model_factors)
valid.column_name_map <- hash("dtDateTime"    = "Date",
                              "dtDate"        = "Date",
                              "lInstrumentID" = "InstrumentID",
                              "sFactorName"   = "FactorName",
                              "Instrument"    = "InstrumentID",
                              "dblLogReturn"  = "Return")
init.key_values       <-  data.frame(Date = as.Date(character()))




test_that(paste("Can create", tested.class, "object"), {
  expect_is(new(tested.class), tested.class)
})

test_that(paste("Can use basic accessors of ", tested.class, "object"), {

  object <- new(tested.class)
  expect_is(object, tested.class)

  expect_equal(getRiskModelComponentName(object), valid.component)

  expect_is(getRiskModelObject(object), valid.risk_model)

  expect_equal(getRiskModelName(object), valid.model_prefix)

  expect_equal(getRiskModelLookback(object), valid.lookback)

  expect_equal(getDataSourceQueryKeyColumnNames(object), valid.key_cols)

  expect_equal(getDataSourceReturnColumnNames(object), valid.values)

  expect_equal(getDataSourceQueryKeyValues(object), init.key_values)

  expect_equal(getDataSourceClientColumnNameMap(object), valid.column_name_map)


})


test_that("Cannot .setDataSourceQueryKeyValues with invalid data", {

  object <- new(tested.class)

  expect_error(TE.RefClasses:::.setDataSourceQueryKeyValues(object, init.key_values),
               regexp = "Zero row query keys data.frame passed")



  invalid.key_values <- data.frame(lC = numeric(), dtD = as.Date(character()))

  expect_error(TE.RefClasses:::.setDataSourceQueryKeyValues(object, invalid.key_values),
               regexp = "Invalid column names of query keys passed")

  expect_equal(getDataSourceQueryKeyValues(object), init.key_values)

})


test_that("Can .setDataSourceQueryKeyValues with valid data", {

  object <- new(tested.class)

  valid.key_vals <- data.frame(Date = seq(from = as.Date('2016-06-01'),
                                          to = as.Date('2016-06-03'),
                                          by = "1 day"))

  object <- TE.RefClasses:::.setDataSourceQueryKeyValues(object, valid.key_vals)

  expect_equal(getDataSourceQueryKeyValues(object), valid.key_vals)

})


test_that("Cannot dataRequest() with invalid key_values", {

  object <- new(tested.class)


  invalid.key_values <- data.frame(Date = as.Date(character()))

  expect_error(dataRequest(object, invalid.key_values),
               regexp = "Zero row query keys data.frame passed")



  invalid.key_values <- data.frame(lC = numeric(), dtD = as.Date(character()))

  expect_error(dataRequest(object, invalid.key_values),
               regexp = "Invalid column names of query keys passed")


  expect_equal(getDataSourceQueryKeyValues(object), init.key_values)

})



#########################
# attachTransformations
#########################


test_that("Can attachTransformations()", {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))
  skip_if_not(FALSE)

  object <- new(tested.class)

  valid.key_vals <- expand.grid(Date = seq(from = as.Date('2016-10-01'),
                                           to = as.Date('2016-10-30'),
                                           by = "1 day"))

  object <- dataRequest(object, valid.key_vals)

  transf <- TE.RefClasses:::getTransformations(object)
  transf.names <- sapply(transf, class)

  object <- attachTransformations(object, transf)

})

#########################
# dataRequest
#########################


test_that("Can dataRequest()", {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  valid.key_vals <- expand.grid(Date = seq(from = as.Date('2016-01-01'),
                                           to = as.Date('2016-10-30'),
                                           by = "1 day"))

  object <- dataRequest(object, valid.key_vals)

  ret <- getReferenceData(object)

  expect_is(ret, "data.frame")

  expect_gt(getStoredNRows(ret), 0)

})

