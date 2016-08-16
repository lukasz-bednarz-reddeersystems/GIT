context("Testing EventData")

library(plyr)


#########################
#
# EventData Tests
#
#########################
tested.class          <-  "EventData"
valid.key_cols        <- c("InstrumentID","Date")
valid.values          <- c("lInstrumentID","dtDateTime", "sEventType")
valid.required_colnms <- c("InstrumentID","Date","EventType")
valid.factor_cols     <- c("EventType")
valid.factor_keys     <- c("Date", "InstrumentID")
valid.non_na_cols     <- character()
valid.column_name_map <- hash(c("dtDateTime","lInstrumentID","sEventType"),
                              c("Date","InstrumentID","EventType"))
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

  expect_equal(getNonNAColumnNames(object), valid.non_na_cols)

  expect_equal(getFactorColumnNames(object), valid.factor_cols)

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

  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  nexist.key_vals <- data.frame(InstrumentID = 1984,
                                Date = seq(from = as.Date('2016-06-01'),
                                                  to = as.Date('2016-06-03'),
                                                  by = "1 day"))
  diff <- setdiff(valid.values,valid.key_cols)

  valid.ret_data <- cbind(nexist.key_vals,data.frame(t(rep(NA,length(diff)))))

  cols <- values(valid.column_name_map[valid.values])[valid.values]

  colnames(valid.ret_data) <- cols

  object <- dataRequest(object, nexist.key_vals)

  var_names <- intersect(getRequiredVariablesNames(object), valid.required_colnms)

  valid.ret_data <- valid.ret_data[setdiff(cols, valid.factor_cols)]

  expect_equal(var_names , setdiff(valid.required_colnms, valid.factor_cols))

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
  datastore <- 'event_datastore'

  # create valid return data.frame
  # create valid return data.frame
  proc_name      <- TE.RefClasses:::.getSQLProcedureName(TE.RefClasses:::.getSQLQueryObject(object))
  proc_args      <- TE.RefClasses:::.getSQLProcedureArgumentNames(TE.RefClasses:::.getSQLQueryObject(object))
  query_key_vals <- TE.RefClasses:::parse_instrument_date_keys(valid.key_vals)
  query_string   <- TE.RefClasses:::generate_procedure_call_strings(proc_name, proc_args, query_key_vals)
  valid.ret_data <- TE.RefClasses:::execute_sql_query(query_string)
  valid.ret_data <- TE.RefClasses:::convert_column_class(valid.ret_data)
  valid.ret_data <- unique(valid.ret_data)
  valid.ret_data <- valid.ret_data[values]
  colnames(valid.ret_data) <- values(valid.column_name_map[values])[values]

  valid.fact_transf <- TE.RefClasses:::factor_transform(valid.ret_data[c(valid.factor_keys,valid.factor_cols)], valid.factor_keys, valid.factor_cols)

  valid.ret_data <- merge(valid.ret_data[setdiff(colnames(valid.ret_data), valid.factor_cols)],
                          valid.fact_transf, by = c(valid.factor_keys), all.x = TRUE)

  object <- dataRequest(object, valid.key_vals)

  expect_equal(getDataSourceQueryKeyColumnNames(object), colnames(valid.key_vals))
  expect_equivalent(getDataSourceQueryKeyValues(object), valid.key_vals)

  ret_data <- getReferenceData(object)

  valid.ret_data <- valid.ret_data[colnames(ret_data)]
  valid.ret_data <- unique(valid.ret_data)

  rownames(valid.ret_data) <- seq(nrow(valid.ret_data))

  class_names <- Map(class, ret_data)

  cols <- colnames(valid.ret_data)

  valid.ret_data <- as.data.frame(lapply(seq(length(class_names)),
                                         function(x) {as(valid.ret_data[,x], class_names[[x]])}), stringsAsFactors = FALSE)
  colnames(valid.ret_data) <- cols

  expect_equal(Map(class, ret_data), Map(class, valid.ret_data))

  expect_equal(ret_data, valid.ret_data)

})


