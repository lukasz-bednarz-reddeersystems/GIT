context("Test RiskModel.VirtualSQLProcedureCall Query Classes")

#####################################################
#
# Testing RiskModel.VirtualSQLProcedureCall class
#
#####################################################

tested.class <- "RiskModel.VirtualSQLProcedureCall"
test.class   <- "Test.RiskModel.VirtualSQLProcedureCall"

test_that(sprintf("Cannot instantiate %s class", tested.class ),{
  expect_error(new(tested.class), regexp = "trying to generate an object from a virtual class")
})


test_that(sprintf("Can inherit from %s class", tested.class),{

  setClass(test.class, contains = tested.class)
  expect_true(isClass(test.class))

})


########################################################################################
#
# Testing RiskModel.SQLProcedureCall.MultiFactorRisk_FactorCorrelationByModelNameDate class
#
########################################################################################

tested.class     <- "RiskModel.SQLProcedureCall.MultiFactorRisk_FactorCorrelationByModelNameDate"
valid.risk_model <- new("RiskModel.DevelopedEuropePrototype150.1.1")
valid.model_name <- getRiskModelName(valid.risk_model)
valid.key_vals   <- data.frame(RiskModelName = valid.model_name,
                               Date = seq(as.Date("2016-01-01"),
                                          as.Date("2016-01-01"),
                                          1))


test_that(sprintf("Can instantiate %s class", tested.class),{

  object <- new(tested.class)

  expect_is(object, tested.class)
})


test_that(sprintf("Can prepareSQLQuery on  %s class", tested.class),{

  object <- new(tested.class)

  expect_is(object, tested.class)

  object <- prepareSQLQuery(object, valid.key_vals)

  expect_is(object, tested.class)

  expect_equal(getSQLQueryKeyValues(object), valid.key_vals)
})

test_that(sprintf("Can executeSQLQuery on  %s class", tested.class),{

  object <- new(tested.class)

  expect_is(object, tested.class)

  object <- new(tested.class)

  expect_is(object, tested.class)

  object <- prepareSQLQuery(object, valid.key_vals)

  expect_is(object, tested.class)

  expect_equal(getSQLQueryKeyValues(object), valid.key_vals)

  ret <- executeSQLQuery(object)

  expect_is(ret, "data.frame")
  expect_equal(nrow(ret), 43)
  expect_equal(ncol(ret), 45)
})

########################################################################################
#
# Testing RiskModel.SQLProcedureCall.MultiFactorRisk_FactorCovarianceByModelNameDate class
#
########################################################################################

tested.class     <- "RiskModel.SQLProcedureCall.MultiFactorRisk_FactorCovarianceByModelNameDate"
valid.risk_model <- new("RiskModel.DevelopedEuropePrototype150.1.1")
valid.model_name <- getRiskModelName(valid.risk_model)
valid.key_vals   <- data.frame(RiskModelName = valid.model_name,
                               Date = seq(as.Date("2016-01-01"),
                                          as.Date("2016-01-01"),
                                          1))


test_that(sprintf("Can instantiate %s class", tested.class),{

  object <- new(tested.class)

  expect_is(object, tested.class)
})


test_that(sprintf("Can prepareSQLQuery on  %s class", tested.class),{

  object <- new(tested.class)

  expect_is(object, tested.class)

  object <- prepareSQLQuery(object, valid.key_vals)

  expect_is(object, tested.class)

  expect_equal(getSQLQueryKeyValues(object), valid.key_vals)
})

test_that(sprintf("Can executeSQLQuery on  %s class", tested.class),{

  object <- new(tested.class)

  expect_is(object, tested.class)

  object <- new(tested.class)

  expect_is(object, tested.class)

  object <- prepareSQLQuery(object, valid.key_vals)

  expect_is(object, tested.class)

  expect_equal(getSQLQueryKeyValues(object), valid.key_vals)

  ret <- executeSQLQuery(object)

  expect_is(ret, "data.frame")
  expect_equal(nrow(ret), 43)
  expect_equal(ncol(ret), 45)
})


########################################################################################
#
# Testing RiskModel.SQLProcedureCall.MultiFactorRisk_FactorVarianceByModelNameDate class
#
########################################################################################

tested.class     <- "RiskModel.SQLProcedureCall.MultiFactorRisk_FactorVarianceByModelNameDate"
valid.risk_model <- new("RiskModel.DevelopedEuropePrototype150.1.1")
valid.model_name <- getRiskModelName(valid.risk_model)
valid.key_vals   <- data.frame(RiskModelName = valid.model_name,
                               Date = seq(as.Date("2016-01-01"),
                                          as.Date("2016-01-01"),
                                          1))


test_that(sprintf("Can instantiate %s class", tested.class),{

  object <- new(tested.class)

  expect_is(object, tested.class)
})


test_that(sprintf("Can prepareSQLQuery on  %s class", tested.class),{

  object <- new(tested.class)

  expect_is(object, tested.class)

  object <- prepareSQLQuery(object, valid.key_vals)

  expect_is(object, tested.class)

  expect_equal(getSQLQueryKeyValues(object), valid.key_vals)
})

test_that(sprintf("Can executeSQLQuery on  %s class", tested.class),{

  object <- new(tested.class)

  expect_is(object, tested.class)

  object <- new(tested.class)

  expect_is(object, tested.class)

  object <- prepareSQLQuery(object, valid.key_vals)

  expect_is(object, tested.class)

  expect_equal(getSQLQueryKeyValues(object), valid.key_vals)

  ret <- executeSQLQuery(object)

  expect_is(ret, "data.frame")
  expect_equal(nrow(ret), 1)
  expect_equal(ncol(ret), 44)
})


########################################################################################
#
# Testing RiskModel.SQLProcedureCall.MultiFactorRisk_MarketStyleByModelNameDate class
#
########################################################################################

tested.class     <- "RiskModel.SQLProcedureCall.MultiFactorRisk_MarketStyleByModelNameDate"
valid.risk_model <- new("RiskModel.DevelopedEuropePrototype150.1.1")
valid.model_name <- getRiskModelName(valid.risk_model)
valid.key_vals   <- data.frame(RiskModelName = valid.model_name,
                               Date = seq(as.Date("2016-01-01"),
                                          as.Date("2016-01-01"),
                                          1))


test_that(sprintf("Can instantiate %s class", tested.class),{

  object <- new(tested.class)

  expect_is(object, tested.class)
})


test_that(sprintf("Can prepareSQLQuery on  %s class", tested.class),{

  object <- new(tested.class)

  expect_is(object, tested.class)

  object <- prepareSQLQuery(object, valid.key_vals)

  expect_is(object, tested.class)

  expect_equal(getSQLQueryKeyValues(object), valid.key_vals)
})

test_that(sprintf("Can executeSQLQuery on  %s class", tested.class),{

  object <- new(tested.class)

  expect_is(object, tested.class)

  object <- new(tested.class)

  expect_is(object, tested.class)

  object <- prepareSQLQuery(object, valid.key_vals)

  expect_is(object, tested.class)

  expect_equal(getSQLQueryKeyValues(object), valid.key_vals)

  ret <- executeSQLQuery(object)

  expect_is(ret, "data.frame")
  expect_equal(nrow(ret), 1)
  expect_equal(ncol(ret), 44)
})

########################################################################################
#
# Testing RiskModel.SQLProcedureCall.MultiFactorRisk_ImpliedFactorReturnsByModelNameDate class
#
########################################################################################

tested.class     <- "RiskModel.SQLProcedureCall.MultiFactorRisk_ImpliedFactorReturnsByModelNameDate"
valid.risk_model <- new("RiskModel.DevelopedEuropePrototype150.1.1")
valid.model_name <- getRiskModelName(valid.risk_model)
valid.key_vals   <- data.frame(RiskModelName = valid.model_name,
                               Date = seq(as.Date("2016-01-01"),
                                          as.Date("2016-01-01"),
                                          1))


test_that(sprintf("Can instantiate %s class", tested.class),{

  object <- new(tested.class)

  expect_is(object, tested.class)
})


test_that(sprintf("Can prepareSQLQuery on  %s class", tested.class),{

  object <- new(tested.class)

  expect_is(object, tested.class)

  object <- prepareSQLQuery(object, valid.key_vals)

  expect_is(object, tested.class)

  expect_equal(getSQLQueryKeyValues(object), valid.key_vals)
})

test_that(sprintf("Can executeSQLQuery on  %s class", tested.class),{

  object <- new(tested.class)

  expect_is(object, tested.class)

  object <- new(tested.class)

  expect_is(object, tested.class)

  object <- prepareSQLQuery(object, valid.key_vals)

  expect_is(object, tested.class)

  expect_equal(getSQLQueryKeyValues(object), valid.key_vals)

  ret <- executeSQLQuery(object)

  expect_is(ret, "data.frame")
  expect_equal(nrow(ret), 1)
  expect_equal(ncol(ret), 44)
})


########################################################################################
#
# Testing RiskModel.SQLProcedureCall.MultiFactorRisk_InstrumentBetasByInstrumentIDModelNameDate class
#
########################################################################################

tested.class     <- "RiskModel.SQLProcedureCall.MultiFactorRisk_InstrumentBetasByInstrumentIDModelNameDate"
valid.risk_model <- new("RiskModel.DevelopedEuropePrototype150.1.1")
valid.model_name <- getRiskModelName(valid.risk_model)
valid.key_vals   <- data.frame(RiskModelName = valid.model_name,
                               InstrumentID = c(5004, 5793, 6496, 7703, 8038, 5826, 5687, 6002, 6203),
                               Date = seq(as.Date("2016-01-01"),
                                          as.Date("2016-01-01"),
                                          1))


test_that(sprintf("Can instantiate %s class", tested.class),{

  object <- new(tested.class)

  expect_is(object, tested.class)
})


test_that(sprintf("Can prepareSQLQuery on  %s class", tested.class),{

  object <- new(tested.class)

  expect_is(object, tested.class)

  object <- prepareSQLQuery(object, valid.key_vals)

  expect_is(object, tested.class)

  expect_equal(getSQLQueryKeyValues(object), valid.key_vals)
})

test_that(sprintf("Can executeSQLQuery on  %s class", tested.class),{

  object <- new(tested.class)

  expect_is(object, tested.class)

  object <- new(tested.class)

  expect_is(object, tested.class)

  object <- prepareSQLQuery(object, valid.key_vals)

  expect_is(object, tested.class)

  expect_equal(getSQLQueryKeyValues(object), valid.key_vals)

  ret <- executeSQLQuery(object)

  expect_is(ret, "data.frame")
  expect_equal(nrow(ret), 9)
  expect_equal(ncol(ret), 45)
})


########################################################################################
#
# Testing RiskModel.SQLProcedureCall.MultiFactorRisk_ResidualReturnsByInstrumentIDModelNameDate class
#
########################################################################################

tested.class     <- "RiskModel.SQLProcedureCall.MultiFactorRisk_ResidualReturnsByInstrumentIDModelNameDate"
valid.risk_model <- new("RiskModel.DevelopedEuropePrototype150.1.1")
valid.model_name <- getRiskModelName(valid.risk_model)
valid.key_vals   <- data.frame(RiskModelName = valid.model_name,
                               InstrumentID = c(5004, 5793, 6496, 7703, 8038, 5826, 5687, 6002, 6203),
                               Date = seq(as.Date("2016-01-04"),
                                          as.Date("2016-01-04"),
                                          1))


test_that(sprintf("Can instantiate %s class", tested.class),{

  object <- new(tested.class)

  expect_is(object, tested.class)
})


test_that(sprintf("Can prepareSQLQuery on  %s class", tested.class),{

  object <- new(tested.class)

  expect_is(object, tested.class)

  object <- prepareSQLQuery(object, valid.key_vals)

  expect_is(object, tested.class)

  expect_equal(getSQLQueryKeyValues(object), valid.key_vals)
})

test_that(sprintf("Can executeSQLQuery on  %s class", tested.class),{

  object <- new(tested.class)

  expect_is(object, tested.class)

  object <- new(tested.class)

  expect_is(object, tested.class)

  object <- prepareSQLQuery(object, valid.key_vals)

  expect_is(object, tested.class)

  expect_equal(getSQLQueryKeyValues(object), valid.key_vals)

  ret <- executeSQLQuery(object)

  expect_is(ret, "data.frame")
  expect_equal(nrow(ret), 9)
  expect_equal(ncol(ret), 4)
})
