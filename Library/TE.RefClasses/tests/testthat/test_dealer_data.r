context("Testing DealerData")

library(plyr)

#########################
#
# DealerData Tests
#
#########################
tested.class          <-  "DealerData"
valid.datastore_name  <- "dealing_datastore"
valid.key_cols        <- c("lTraderID", "dtTradeDate")
valid.factor_cols     <- c("InputDirection")
valid.factor_keys     <- c("Date", "InstrumentID")
valid.non_na_cols     <- character()
valid.values          <- c("lTraderID","dtTradeDate","lInstrumentID","sTradeRationale","sInputDirection")
valid.required_colnms <- c("Date","InstrumentID","Rationale","InputDirection")
valid.column_name_map <- hash(c("lTraderID","dtTradeDate","lInstrumentID","sTradeRationale","sInputDirection"),
                              c("TraderID","Date","InstrumentID","Rationale","InputDirection"))
init.key_values       <-  data.frame(lTraderID = integer(),
                                     dtTradeDate = as.Date(character()))




test_that(paste("Can create", tested.class, "object"), {
  expect_is(new(tested.class), tested.class)
})

test_that(paste("Can use basic accessors of ", tested.class, "object"), {

  object <- new(tested.class)
  expect_is(object, tested.class)

  expect_equal(getDataStoreName(object), valid.datastore_name)

  expect_equal(getDataSourceQueryKeyColumnNames(object), valid.key_cols)

  expect_equal(getDataSourceReturnColumnNames(object), valid.values)

  expect_equal(getDataSourceQueryKeyValues(object), init.key_values)

  expect_equal(getNonNAColumnNames(object), valid.non_na_cols)

  expect_equal(getFactorColumnNames(object), valid.factor_cols)

  expect_equal(getDataSourceClientColumnNameMap(object), valid.column_name_map)


})


test_that("Cannot .setDataSourceQueryKeyValues with invalid data", {

  object <- new(tested.class)

  invalid.key_values <- data.frame(lTraderID = integer(),
                                   dtTradeDate = as.Date(character()))

  expect_error(TE.RefClasses:::.setDataSourceQueryKeyValues(object, invalid.key_values),
               regexp = "Zero row query keys data.frame passed")



  invalid.key_values <- data.frame(lC = numeric(), dtD = as.Date(character()))

  expect_error(TE.RefClasses:::.setDataSourceQueryKeyValues(object, invalid.key_values),
               regexp = "Invalid column names of query keys passed")

  expect_equal(getDataSourceQueryKeyValues(object), init.key_values)

})


test_that("Can .setDataSourceQueryKeyValues with valid data", {

  object <- new(tested.class)

  valid.key_vals <- data.frame(lTraderID = 11,
                               dtTradeDate = seq(from = as.Date('2016-06-01'),
                                                 to = as.Date('2016-06-03'),
                                                 by = "1 day"))

  object <- TE.RefClasses:::.setDataSourceQueryKeyValues(object, valid.key_vals)

  expect_equal(getDataSourceQueryKeyValues(object), valid.key_vals)

})


test_that("Cannot dataRequest() with invalid key_values", {

  object <- new(tested.class)


  invalid.key_values <- data.frame(lTraderID = integer(),
                                   dtTradeDate = as.Date(character()))

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

  nexist.key_vals <- data.frame(lTraderID = 1984,
                                dtTradeDate = seq(from = as.Date('2016-06-01'),
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

  valid.key_vals <- data.frame(lTraderID = 11,
                               dtTradeDate = seq(from = as.Date('2016-06-01'),
                                                 to = as.Date('2016-06-03'),
                                                 by = "1 day"))

  values <- getDataSourceReturnColumnNames(object)
  datastore <- getDataStoreName(object)

  # create valid return data.frame
  valid.ret_data <- data_request(datastore ,valid.key_vals,values)
  valid.ret_data <- data_request(datastore ,valid.key_vals,values)
  valid.ret_data <- getData(valid.ret_data)
  valid.ret_data <- unique(valid.ret_data)
  valid.ret_data <- valid.ret_data[values]
  colnames(valid.ret_data) <- values(valid.column_name_map[values])[values]
  valid.ret_data <- valid.ret_data[valid.required_colnms]
  rownames(valid.ret_data) <- seq(nrow(valid.ret_data))


  valid.fact_transf <- TE.RefClasses:::factor_transform(valid.ret_data[c(valid.factor_keys,valid.factor_cols)], valid.factor_keys, valid.factor_cols)

  valid.ret_data <- merge(valid.ret_data[setdiff(colnames(valid.ret_data), valid.factor_cols)],
                          valid.fact_transf, by = c(valid.factor_keys), all = TRUE)

  object <- dataRequest(object, valid.key_vals)

  expect_equal(getDataSourceQueryKeyColumnNames(object), colnames(valid.key_vals))
  expect_equal(getDataSourceQueryKeyValues(object), valid.key_vals)

  ret_data <- getReferenceData(object)

  valid.ret_data <- valid.ret_data[colnames(ret_data)]

  class_names <- Map(class, ret_data)

  cols <- colnames(valid.ret_data)

  valid.ret_data <- as.data.frame(lapply(seq(length(class_names)),
                                         function(x) {as(valid.ret_data[,x], class_names[[x]])}), stringsAsFactors = FALSE)
  colnames(valid.ret_data) <- cols

  expect_equal(Map(class, ret_data), Map(class, valid.ret_data))

  expect_equal(arrange(ret_data, Date, InstrumentID, Rationale), plyr:::arrange(valid.ret_data, Date, InstrumentID, Rationale))
  expect_equal(ret_data[with(ret_data, order(Date, InstrumentID, Rationale)),],
               valid.ret_data[with(valid.ret_data, order(Date, InstrumentID, Rationale)),])

  for(col in colnames(ret_data)) {
    expect_equal(arrange(ret_data, Date, InstrumentID, Rationale)[col], arrange(valid.ret_data, Date, InstrumentID, Rationale)[col])
  }


  expect_equal(ret_data, valid.ret_data)

})


