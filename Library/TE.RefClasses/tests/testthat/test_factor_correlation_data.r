context("Testing Factor Correlation Data")

#############################
#
# FactorCorrelationData Tests
#
#############################

tested.class          <-  "FactorCorrelationData"
valid.component       <- "FactorCorrelation"
valid.risk_model      <- "RiskModel.DevelopedEuropePrototype150"
valid.model_prefix    <- "developed_europe_prototype"
valid.lookback        <- 150
valid.key_cols        <- c(risk_model_objectstore_keys)
valid.values          <- c("Date",
                           risk_model_market_factors,
                           risk_model_currency_factors,
                           risk_model_commodity_factors,
                           risk_model_sector_factors)
valid.required_colnms <- c('Date',
                           risk_model_market_factors,
                           risk_model_currency_factors,
                           risk_model_commodity_factors,
                           risk_model_sector_factors)
valid.column_name_map <- hash()
init.key_values       <-  data.frame(Date = as.Date(character()))




test_that(paste("Can create", tested.class, "object"), {
  expect_is(new(tested.class), tested.class)
})

test_that(paste("Can use basic accessors of ", tested.class, "object"), {

  object <- new(tested.class)
  expect_is(object, tested.class)

  expect_equal(getRiskModelObjectstoreComponentName(object), valid.component)

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


test_that("Generates empty data.frame when dataRequest() with nonexistent key_values", {

  object <- new(tested.class)

  nexist.key_vals <- data.frame(Date = seq(from = as.Date('2016-06-01'),
                                                  to = as.Date('2016-06-03'),
                                                  by = "1 day"))
  diff <- setdiff(valid.required_colnms,valid.key_cols)

  valid.ret_data <- cbind(nexist.key_vals,data.frame(t(rep(NA,length(diff)))))

  colnames(valid.ret_data) <- c(colnames(nexist.key_vals), diff)


  object <- dataRequest(object, nexist.key_vals)

  var_names <- intersect(getRequiredVariablesNames(object), valid.required_colnms)

  expect_equal(var_names , valid.required_colnms)

  ret_data <- getReferenceData(object)

  valid.ret_data <- valid.ret_data[colnames(ret_data)]

  class_names <- Map(class, ret_data)

  setAs("numeric", "Date", function(from){as.Date(from)})

  cols <- colnames(valid.ret_data)
  valid.ret_data <- as.data.frame(lapply(seq(length(class_names)),
                                         function(x) {as(valid.ret_data[,x], class_names[[x]])}), stringsAsFactors = FALSE)

  colnames(valid.ret_data) <- cols

  expect_equivalent(ret_data, valid.ret_data)

})




test_that("Can dataRequest() with valid key_values", {

  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- new(tested.class)

  valid.key_vals <- expand.grid(Date = seq(from = as.Date('2016-06-20'),
                                               to = as.Date('2016-06-23'),
                                               by = "1 day"))

  values <- getDataSourceReturnColumnNames(object)

  # create valid return data.frame
  start        <- min(valid.key_vals$Date)
  end          <- max(valid.key_vals$Date)
  rm_str       <- get_most_recent_model_objectstore(valid.model_prefix, end, valid.lookback)
  name         <- getID(rm_str)
  query_data   <- queryDailyRiskModelObjectStore(rm_str,name,valid.lookback,valid.component)
  query_data   <- getData(query_data)
  query_data   <- query_data[query_data$Date >= start & query_data$Date <= end, ]

  merge_keys   <- valid.key_vals

  colnames(merge_keys) <- TE.RefClasses:::.translateDataSourceColumnNames(object, colnames(merge_keys))

  query_data$Index <- seq(nrow(query_data))

  valid.ret_data <- merge(query_data,
                        merge_keys[merge_keys$Date >= start & merge_keys$Date <= end,, drop = FALSE],
                        all.y = TRUE)

  query_data <- query_data[order(query_data$Index), setdiff(colnames(query_data), "Index")]

  colnames(valid.ret_data) <- TE.RefClasses:::.translateDataSourceColumnNames(object, colnames(valid.ret_data))

  rownames(valid.ret_data) <- seq(nrow(valid.ret_data))

  object <- dataRequest(object, valid.key_vals)

  expect_true(setequal(getDataSourceQueryKeyColumnNames(object), colnames(valid.key_vals)))
  expect_true(setequal(getDataSourceQueryKeyValues(object), valid.key_vals))

  ret_data <- getReferenceData(object)

  valid.ret_data <- valid.ret_data[colnames(ret_data)]

  expect_equal(unlist(Map(class, ret_data)), unlist(Map(class, valid.ret_data)))

  expect_equal(ret_data, valid.ret_data)

})


