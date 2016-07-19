sourceTo("../common/price_data/price_data.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(testthat)
library(plyr)


#########################
#
# EventData Tests
#
#########################
tested.class          <-  "PriceData"
valid.key_cols        <- c("lInstrumentID", "dtDateTime")
valid.values          <- c("lInstrumentID", "dtDateTime","dblClosePrice","dblPreviousClosePrice",
                           "dblVolume", "lOutstandingShares","dbl30DayAvgVol" )
valid.required_colnms <- c('InstrumentID','Date','ClosePrice')
valid.column_name_map <- hash(c("lInstrumentID", "dtDateTime","dblClosePrice","dblPreviousClosePrice",
                                "dblVolume", "lOutstandingShares","dbl30DayAvgVol" ), 
                              c('InstrumentID','Date','ClosePrice',"PreviousClosePrice",
                                "Volume", "OutstandingShares","AvgVol30Day"))
init.key_values       <-  data.frame(lInstrumentID = integer(), 
                                     dtDateTime = as.Date(character()))




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
  
  invalid.key_values <- data.frame(lInstrumentID = integer(), 
                                   dtDateTime = as.Date(character()))
  
  expect_error(.setDataSourceQueryKeyValues(object, invalid.key_values),
               regexp = "Zero row query keys data.frame passed")
  
  
  
  invalid.key_values <- data.frame(lC = numeric(), dtD = as.Date(character()))
  
  expect_error(.setDataSourceQueryKeyValues(object, invalid.key_values),
               regexp = "Invalid column names of query keys passed")
  
  expect_equal(getDataSourceQueryKeyValues(object), init.key_values)
  
})


test_that("Can .setDataSourceQueryKeyValues with valid data", {
  
  object <- new(tested.class)
  
  valid.key_vals <- data.frame(lInstrumentID = 4454, 
                               dtDateTime = seq(from = as.Date('2016-06-01'), 
                                                 to = as.Date('2016-06-03'),
                                                 by = "1 day"))
  
  object <- .setDataSourceQueryKeyValues(object, valid.key_vals)
  
  expect_equal(getDataSourceQueryKeyValues(object), valid.key_vals)
  
})


test_that("Cannot dataRequest() with invalid key_values", {
  
  object <- new(tested.class)
  
  
  invalid.key_values <- data.frame(lInstrumentID = integer(), 
                                   dtDateTime = as.Date(character()))
  
  expect_error(dataRequest(object, invalid.key_values),
               regexp = "Zero row query keys data.frame passed")
  
  
  
  invalid.key_values <- data.frame(lC = numeric(), dtD = as.Date(character()))
  
  expect_error(dataRequest(object, invalid.key_values),
               regexp = "Invalid column names of query keys passed")
  
  
  expect_equal(getDataSourceQueryKeyValues(object), init.key_values)
  
})


test_that("Generates empty data.frame when dataRequest() with nonexistent key_values", {
  
  object <- new(tested.class)
  
  nexist.key_vals <- data.frame(lInstrumentID = 1984, 
                                dtDateTime = seq(from = as.Date('2016-06-01'), 
                                                  to = as.Date('2016-06-03'),
                                                  by = "1 day"))
  diff <- setdiff(valid.values,valid.key_cols)
  
  valid.ret_data <- cbind(nexist.key_vals,data.frame(t(rep(NA,length(diff)))))
  
  cols <- values(valid.column_name_map[valid.values])[valid.values]
  
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
  
  object <- new(tested.class)
  
  # instruments with large ammount of events for these days
  # 5004 5793 6496 7703 8038 5826 5687 6002 6203 
  # 6    6    7    7    7    8   11   11   12 
  valid.key_vals <- expand.grid(lInstrumentID = c(5004, 5793, 6496, 7703, 8038, 5826, 5687, 6002, 6203), 
                                dtDateTime = seq(from = as.Date('2016-06-01'), 
                                               to = as.Date('2016-06-03'),
                                               by = "1 day"))
  values <- getDataSourceReturnColumnNames(object)

  # create valid return data.frame
  proc_name      <- .getSQLProcedureName(.getSQLQueryObject(object))
  proc_args       <- .getSQLProcedureArgumentNames(.getSQLQueryObject(object))
  query_key_vals <- parse_instrument_date_keys(valid.key_vals)
  query_string   <- generate_procedure_call_strings(proc_name, proc_args, query_key_vals)
  valid.ret_data <- execute_sql_query(query_string)
  valid.ret_data <- convert_column_class(valid.ret_data)
  valid.ret_data <- unique(valid.ret_data)
  valid.ret_data <- valid.ret_data[values]
  colnames(valid.ret_data) <- values(valid.column_name_map[values])[values]
  rownames(valid.ret_data) <- seq(nrow(valid.ret_data))
  
  object <- dataRequest(object, valid.key_vals)
  
  expect_equal(getDataSourceQueryKeyColumnNames(object), colnames(valid.key_vals))
  expect_equal(getDataSourceQueryKeyValues(object), valid.key_vals)
  
  ret_data <- getReferenceData(object)
  
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


