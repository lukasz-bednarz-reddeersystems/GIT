context("Test DataAccess.SQLProcedureCall.Query_HistoricalTrades_WithInstrumentIDAndOrderID class")

##############################################################################################
#
# Testing DataAccess.SQLProcedureCall.Query_HistoricalTrades_WithInstrumentIDAndOrderID class
#
##############################################################################################


tested.class     <- "DataAccess.SQLProcedureCall.Query_HistoricalTrades_WithInstrumentIDAndOrderID"


valid.key_cols   <- c("TraderID", "DateStart", "DateEnd")
valid.key_values <- data.frame(TraderID  = 101L,
                               DateStart = as.Date("2016-09-28"),
                               DateEnd   = as.Date("2016-09-28") )

test_that(sprintf("Can instantiate %s class witht parameters", tested.class),{

  object <- new(tested.class)

  expect_is(object, tested.class)
})


test_that(sprintf("Can prepareSQLQuery on  %s class", tested.class),{

  object <- new(tested.class)

  expect_is(object, tested.class)

  object <- prepareSQLQuery(object, valid.key_values)

  expect_equal(getSQLQueryKeyValues(object), valid.key_values)
  expect_is(object, tested.class)
})

test_that(sprintf("Can executeSQLQuery on  %s class", tested.class),{

  object <- new(tested.class)

  expect_is(object, tested.class)

  ret <- executeSQLQuery(object, valid.key_values)

  expect_is(ret, "data.frame")

  expect_gt(nrow(ret), 0)
})


context("Test DataAccess.SQLProcedureCall.InstrumentHistoryRequired_QueryPriceHistoryFromTQA class")

##############################################################################################
#
# Testing DataAccess.SQLProcedureCall.InstrumentHistoryRequired_QueryPriceHistoryFromTQA class
#
##############################################################################################


tested.class     <- "DataAccess.SQLProcedureCall.InstrumentHistoryRequired_QueryPriceHistoryFromTQA"


valid.key_cols   <- c("InstrumentID", "DateStart", "DateEnd")
valid.key_values <- data.frame(InstrumentID  = 4454L,
                               DateStart = as.Date("2016-09-01"),
                               DateEnd   = as.Date("2016-09-28") )

test_that(sprintf("Can instantiate %s class witht parameters", tested.class),{

  object <- new(tested.class)

  expect_is(object, tested.class)
})


test_that(sprintf("Can prepareSQLQuery on  %s class", tested.class),{

  object <- new(tested.class)

  expect_is(object, tested.class)

  object <- prepareSQLQuery(object, valid.key_values)

  expect_equal(getSQLQueryKeyValues(object), valid.key_values)
  expect_is(object, tested.class)
})

test_that(sprintf("Can executeSQLQuery on  %s class", tested.class),{

  object <- new(tested.class)

  expect_is(object, tested.class)

  ret <- executeSQLQuery(object, valid.key_values)

  expect_is(ret, "data.frame")

  expect_gt(nrow(ret), 0)
})
