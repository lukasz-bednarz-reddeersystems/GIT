context("Testing FactorExposureData")

#########################
#
# FactorExposureData Tests
#
#########################
tested.class          <- "FactorExposureData"
valid.db_name         <- TE.RefClasses:::RISK_MODEL_DB()
valid.key_cols        <- c("InstrumentID", "Date")
valid.values          <- c("lInstrumentID","dtDateTime","lFactorRiskInstrumentID", "sFactorName",
                             "dblZScore", "dblValue")
valid.required_colnms <- c("FactorName", "Date", "InstrumentID",
                           "ZScore", "Value")
valid.column_name_map <- hash(c("lFactorRiskInstrumentID", "sFactorName", "dtDateTime", "lInstrumentID",
                                "dblZScore", "dblValue"),
                              c("FactorRiskInstrumentID", "FactorName", "Date", "InstrumentID",
                                "ZScore", "Value"))
init.key_values       <-  data.frame(InstrumentID = integer(),
                                     Date = as.Date(character()))




test_that(paste("Can create", tested.class, "object"), {
  expect_is(new(tested.class), tested.class)
})

test_that(paste("Can use basic accessors of ", tested.class, "object"), {

  object <- new(tested.class)
  expect_is(object, tested.class)

  expect_equal(getDataSourceQueryKeyColumnNames(object), valid.key_cols)

  expect_equal(getDataSourceReturnColumnNames(object), valid.values)

  expect_equal(getDataSourceQueryKeyValues(object), init.key_values)

  expect_equal(getDataSourceClientColumnNameMap(object), valid.column_name_map)


})


test_that("Cannot .setDataSourceQueryKeyValues with invalid data", {

  object <- new(tested.class)

  invalid.key_values <- data.frame(InstrumentID = integer(),
                                   Date = as.Date(character()))

  expect_error(TE.RefClasses:::.setDataSourceQueryKeyValues(object, invalid.key_values),
               regexp = "Zero row query keys data.frame passed")



  invalid.key_values <- data.frame(lC = numeric(), dtD = as.Date(character()))

  expect_error(TE.RefClasses:::.setDataSourceQueryKeyValues(object, invalid.key_values),
               regexp = "Invalid column names of query keys passed")

  expect_equal(getDataSourceQueryKeyValues(object), init.key_values)

})


test_that("Can .setDataSourceQueryKeyValues with valid data", {

  object <- new(tested.class)

  valid.key_vals <- data.frame(InstrumentID = 4454,
                               Date = seq(from = as.Date('2016-06-01'),
                                                 to = as.Date('2016-06-03'),
                                                 by = "1 day"))

  object <- TE.RefClasses:::.setDataSourceQueryKeyValues(object, valid.key_vals)

  expect_equal(getDataSourceQueryKeyValues(object), valid.key_vals)

})


test_that("Cannot dataRequest() with invalid key_values", {

  object <- new(tested.class)


  invalid.key_values <- data.frame(InstrumentID = integer(),
                                   Date = as.Date(character()))

  expect_error(dataRequest(object, invalid.key_values),
               regexp = "Zero row query keys data.frame passed")



  invalid.key_values <- data.frame(lC = numeric(), dtD = as.Date(character()))

  expect_error(dataRequest(object, invalid.key_values),
               regexp = "Invalid column names of query keys passed")


  expect_equal(getDataSourceQueryKeyValues(object), init.key_values)

})


test_that("Generates empty data.frame when dataRequest() with nonexistent key_values", {

  object <- new(tested.class)

  nexist.key_vals <- data.frame(InstrumentID = 1984,
                                Date = seq(from = as.Date('2016-06-01'),
                                                  to = as.Date('2016-06-03'),
                                                  by = "1 day"))
  cols <- TE.RefClasses:::.translateDataSourceColumnNames(object, valid.values)

  diff <- setdiff(cols ,valid.key_cols)

  valid.ret_data <- cbind(nexist.key_vals,data.frame(t(rep(NA,length(diff)))))

  colnames(valid.ret_data) <- cols

  object <- dataRequest(object, nexist.key_vals)

  var_names <- intersect(getRequiredVariablesNames(object), valid.required_colnms)

  expect_equal(var_names , valid.required_colnms)

  ret_data <- getReferenceData(object)

  valid.ret_data <- valid.ret_data[colnames(ret_data)]

  class_names <- Map(class, ret_data)

  cols <- colnames(valid.ret_data)
  valid.ret_data <- as.data.frame(lapply(seq(length(class_names)),
                                         function(x) {as(valid.ret_data[,x], class_names[[x]])}), stringsAsFactors = FALSE)

  colnames(valid.ret_data) <- cols

  expect_equivalent(ret_data, valid.ret_data)

})




test_that("Can dataRequest() with valid key_values", {

  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  # instruments with large ammount of events for these days
  # 5004 5793 6496 7703 8038 5826 5687 6002 6203
  # 6    6    7    7    7    8   11   11   12
  valid.key_vals <- expand.grid(InstrumentID = c(5004, 5793, 6496, 7703, 8038, 5826, 5687, 6002, 6203),
                                Date = seq(from = as.Date('2016-06-01'),
                                               to = as.Date('2016-06-03'),
                                               by = "1 day"))
  values <- getDataSourceReturnColumnNames(object)

  # create valid return data.frame
  proc_name      <- TE.SQLQuery:::.getSQLProcedureName(TE.RefClasses:::.getSQLQueryObject(object))
  proc_args      <- TE.SQLQuery:::.getSQLProcedureArgumentNames(TE.RefClasses:::.getSQLQueryObject(object))
  query_key_vals <- TE.RefClasses:::parse_instrument_date_keys(valid.key_vals)
  query_string   <- TE.SQLQuery:::generate_procedure_call_strings(proc_name, proc_args, query_key_vals)
  valid.ret_data <- TE.SQLQuery:::execute_sql_query(query_string, valid.db_name, "Razor")
  valid.ret_data <- TE.RefClasses:::convert_column_class(valid.ret_data)
  valid.ret_data <- unique(valid.ret_data)
  valid.ret_data <- valid.ret_data[values]
  colnames(valid.ret_data) <- values(valid.column_name_map[values])[values]
  valid.ret_data <- valid.ret_data[with(valid.ret_data, order(Date, InstrumentID, FactorRiskInstrumentID)),]

  rownames(valid.ret_data) <- seq(nrow(valid.ret_data))

  object <- dataRequest(object, valid.key_vals)

  expect_equal(getDataSourceQueryKeyColumnNames(object), colnames(valid.key_vals))
  expect_equivalent(getDataSourceQueryKeyValues(object), valid.key_vals)

  ret_data <- getReferenceData(object)
  ret_data <- ret_data[with(ret_data, order(Date, InstrumentID, FactorRiskInstrumentID)),]
  rownames(ret_data) <- seq(nrow(ret_data))

  valid.ret_data <- valid.ret_data[colnames(ret_data)]

  # class_names <- Map(class, ret_data)
  #
  # cols <- colnames(valid.ret_data)
  #
  # valid.ret_data <- as.data.frame(lapply(seq(length(class_names)),
  #                                        function(x) {as(valid.ret_data[,x], class_names[[x]])}), stringsAsFactors = FALSE)
  # colnames(valid.ret_data) <- cols

  expect_equal(unlist(Map(class, ret_data)), unlist(Map(class, valid.ret_data)))

  expect_equal(ret_data, valid.ret_data)

})


