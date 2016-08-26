context("Testing TradeDataSimple")

#########################
#
# TradeDataSimple Tests
#
#########################
tested.class          <-  "TradeDataSimple"
valid.key_cols        <- c("id", "start", "end")
valid.values          <- c("TradeDate", "TradeID", "Long", "Instrument", "Trader", "ValueUSD", "Strategy",
                           outcome_price_features, context_price_features )
valid.required_colnms <- c("Date", "TradeID", "Long", "InstrumentID", "TraderName", "ValueUSD", "Strategy",
                           outcome_price_features, context_price_features )
valid.column_name_map <- hash(c("TradeID", "TradeDate", "Long", "Instrument", "Trader", "ValueUSD", "Strategy",
                                outcome_price_features, context_price_features ),
                              c("TradeID", "Date", "Long", "InstrumentID", "TraderName", "ValueUSD","Strategy",
                                outcome_price_features, context_price_features ))
init.key_values       <-  data.frame(id = integer(),
                                     start = as.Date(character()),
                                     end = as.Date(character()))

valid.model_class     <- "TradeHistorySimple"


test_that(paste("Can create", tested.class, "object"), {
  expect_is(new(tested.class), tested.class)
})

test_that(paste("Can use basic accessors of ", tested.class, "object"), {

  object <- new(tested.class)
  expect_is(object, tested.class)

  expect_equal(getDataSourceQueryKeyColumnNames(object), valid.key_cols)

  expect_equal(getDataSourceReturnColumnNames(object), valid.values)

  expect_equal(getRequiredVariablesNames(object), valid.required_colnms)

  expect_equal(getDataSourceQueryKeyValues(object), init.key_values)

  expect_equal(getDataSourceClientColumnNameMap(object), valid.column_name_map)

  expect_equal(getPPModelClass(object), valid.model_class)

})


test_that("Cannot .setDataSourceQueryKeyValues with invalid data", {

  object <- new(tested.class)

  invalid.key_values <- data.frame(id = integer(),
                                   start = as.Date(character()),
                                   end = as.Date(character()))

  expect_error(TE.RefClasses:::.setDataSourceQueryKeyValues(object, invalid.key_values),
               regexp = "Zero row query keys data.frame passed")



  invalid.key_values <- data.frame(lC = numeric(), dtD = as.Date(character()))

  expect_error(TE.RefClasses:::.setDataSourceQueryKeyValues(object, invalid.key_values),
               regexp = "Invalid column names of query keys passed")

  expect_equal(getDataSourceQueryKeyValues(object), init.key_values)

})


test_that("Can .setDataSourceQueryKeyValues with valid data", {

  object <- new(tested.class)

  valid.key_vals <- data.frame(id = 11,
                                  start = "2016-03-29",
                                  end = "2016-04-01")

  object <- TE.RefClasses:::.setDataSourceQueryKeyValues(object, valid.key_vals)

  expect_equal(getDataSourceQueryKeyValues(object), valid.key_vals)

})


test_that("Cannot dataRequest() with invalid key_values", {

  object <- new(tested.class)


  invalid.key_values <- data.frame(id = integer(),
                                   start = as.Date(character()),
                                   end = as.Date(character()))

  expect_error(dataRequest(object, invalid.key_values),
               regexp = "Zero row query keys data.frame passed")

  invalid.key_values <- data.frame(lC = numeric(), dtD = as.Date(character()))

  expect_error(dataRequest(object, invalid.key_values),
               regexp = "Invalid column names of query keys passed")


  expect_equal(getDataSourceQueryKeyValues(object), init.key_values)

})


test_that("Generates empty data.frame when dataRequest() with nonexistent key_values", {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))
  # create new object
  object <- new(tested.class)

  # create test vectors
  nexist.key_vals <- data.frame(id = 1984,
                                start = as.Date("2016-03-29"),
                                end = as.Date("2016-04-01"))



  diff <- setdiff(valid.values, c("Date"))

  valid.ret_data <- cbind(data.frame(Date = seq(from = nexist.key_vals$start,
                                                to = nexist.key_vals$end,
                                                by = 1)),
                          data.frame(t(rep(NA,length(diff))))
                          )

  cols <- c("Date", values(valid.column_name_map[diff])[diff])

  colnames(valid.ret_data) <- cols

  # request data
  object <- dataRequest(object, nexist.key_vals)

  # make sure column classes are the same
  var_names <- intersect(getRequiredVariablesNames(object), valid.required_colnms)


  expect_true(setequal(var_names , valid.required_colnms))

  # retrieve stored values
  ret_data <- getReferenceData(object)

  valid.ret_data <- valid.ret_data[colnames(ret_data)]

  # compare return and expected data
  expect_equivalent(ret_data, valid.ret_data)

})




test_that("Can dataRequest() with valid key_values", {
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  # create new instance of class
  object <- new(tested.class)
  valid.key_vals <- data.frame(id = 11,
                               start = as.Date("2016-03-29"),
                               end = as.Date("2016-04-01"))

  valid.store_keys <- data.frame(model_class = "TradeHistorySimple",
                                 id = valid.key_vals$id,
                                 start = min(valid.key_vals$start),
                                 end = max(valid.key_vals$end))

  # create valid return data.frame
  valid.ids <- get_ppmodel_objectstore_name(valid.store_keys)
  ppm_store <- ppmodel_objectstore_factory(valid.ids)

  ppm <- queryPPModelStore(ppm_store, valid.store_keys)
  expect_true(!is.null(ppm))

  valid.ret_data <- getData(ppm@modeldata)
  valid.ret_data[setdiff(valid.values, colnames(valid.ret_data))] <- NA
  valid.ret_data <- valid.ret_data[valid.values]
  colnames(valid.ret_data) <- TE.RefClasses:::.translateDataSourceColumnNames(object, valid.values)

  valid.ret_data <- unique(valid.ret_data)
  rownames(valid.ret_data) <- seq(nrow(valid.ret_data))

  # request data
  object <- dataRequest(object, valid.key_vals)

  # compare basic query internals
  expect_equal(getDataSourceQueryKeyColumnNames(object), colnames(valid.key_vals))
  expect_equal(getDataSourceQueryKeyValues(object), valid.key_vals)

  # retrieve stored data
  ret_data <- getReferenceData(object)

  valid.ret_data <- valid.ret_data[colnames(ret_data)]

  expect_equal(ret_data, valid.ret_data)

})


