context("Test SQLProcedureCall")

########################################
#
# Testing SQLProcedureCall class
#
########################################
tested.class <- "VirtualSQLProcedureCall"
test.class   <- "TestSQLProcedureCall"
test.parser  <- TE.SQLQuery:::parse_instrument_date_keys

valid.key_cols <- c("InstrumentID", "Date")

valid.key_vals <-data.frame(lInstrumentID = integer(),
                             dtDateTime = as.Date(character()))
test.key_vals  <- data.frame(InstrumentID = 4454, Date = today() -1)

valid.parsed.key_vals <- data.frame(InstrumentIDs = 4454,
                                    start = today() -1,
                                    end  = today() -1)


test_that(sprintf("Cannot instantiate %s class", tested.class ),{
  expect_error(new(tested.class), regexp = "trying to generate an object from a virtual class")
})


test_that(sprintf("Can inherit from %s class", tested.class),{

  setClass(
    Class     = test.class,
    prototype = list(
      db_name        = "RAIDSTAGEDB",
      db_schema      = "Razor",
      key_cols       = valid.key_cols,
      key_values     = data.frame(lInstrumentID = integer(),
                                  dtDateTime = as.Date(character())),
      query_parser   = test.parser,
      arguments    = c("@sInstrumentIDs", "@dtStart", "@dtEnd"),
      procedure    = "prFactorRisk_GetScores_TraderAnalytics_InstrumentList"
    ),
    contains  = c("VirtualSQLProcedureCall")
  )
  expect_true(isClass(test.class))

})


test_that(sprintf("Can instantiate  object of %s class", test.class),{

  object <- new(test.class)
  expect_is(object, test.class)

})


test_that(sprintf("Can use basic accessors of  %s class", test.class),{

  object <- new(test.class)

  expect_equal(getSQLQueryKeyColumnNames(object), valid.key_cols)
  expect_equal(getSQLQueryKeyValues(object), valid.key_vals)

})

test_that("Can prepareSQLQuery",{

  object <- new(test.class)
  object <- expect_is(object, test.class)

  object <- prepareSQLQuery(object, test.key_vals)

  expect_equal(getSQLQueryKeyValues(object), test.key_vals)


})


test_that("Can executeSQLQuery",{

  object <- new(test.class)
  object <- expect_is(object, test.class)

  object <- prepareSQLQuery(object, test.key_vals)

  expect_equal(getSQLQueryKeyValues(object), test.key_vals)

  ret <- executeSQLQuery(object)

  expect_is(ret, "data.frame")
  expect_gt(nrow(ret, 0))


})


