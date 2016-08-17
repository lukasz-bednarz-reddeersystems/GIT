context("Testing PositionData")

#########################
#
# PositionData Tests
#
#########################
tested.class          <-  "PositionData"
valid.key_cols        <- c("id", "start", "end")
valid.values          <- c("UserID", "Date", "StrategyID", "InstrumentID", "Active", "Trader", "FundGroup",
                           "Alias", "Group", "Type", "Direction", "Description", "AliasID",
                           "GroupID", "TypeID", "MarketValue", "TodayPL", "Strategy",
                           "Quantity", "Age",  "PsnReturn")
valid.required_colnms <- c("Date", "StrategyID", "InstrumentID", "TraderID", "Direction", "MarketValue", "TodayPL",
                           "Quantity", "Age",  "PsnReturn")
valid.column_name_map <- hash(c("Date", "StrategyID", "InstrumentID", "Active", "Trader", "FundGroup",
                                "UserID", "Alias", "Group", "Type", "Direction", "Description", "AliasID",
                                "GroupID", "TypeID", "MarketValue", "TodayPL", "Strategy",
                                "Quantity", "Age",  "PsnReturn"),
                              c("Date", "StrategyID", "InstrumentID", "Active", "TraderName", "FundGroup",
                                "TraderID", "Alias", "Group", "Type", "Direction", "Description", "AliasID",
                                "GroupID", "TypeID", "MarketValue", "TodayPL", "Strategy",
                                "Quantity", "Age",  "PsnReturn"))
init.key_values       <-  data.frame(id = integer(),
                                     start = as.Date(character()),
                                     end = as.Date(character()))


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
  # create new object
  object <- new(tested.class)

  # create test vectors
  nexist.key_vals <- data.frame(id = 1984,
                                start = as.Date("2016-03-29"),
                                end = as.Date("2016-04-01"))



  diff <- setdiff(valid.values, c("TraderID", "Date"))

  valid.ret_data <- cbind(data.frame(TraderID = 1984,
                                     Date = seq(from = nexist.key_vals$start,
                                                to = nexist.key_vals$end,
                                                by = 1)),
                          data.frame(t(rep(NA,length(diff))))
                          )

  cols <- c("TraderID", "Date", values(valid.column_name_map[diff])[diff])

  colnames(valid.ret_data) <- cols

  # request data
  object <- dataRequest(object, nexist.key_vals)

  # make sure column classes are the same
  var_names <- intersect(getRequiredVariablesNames(object), valid.required_colnms)

  valid.ret_data <- valid.ret_data[cols]

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
                                  start = as.Date("2015-07-31"),
                                  end = as.Date("2015-08-31"))

  # valid.key_vals <- dated_twelve_monthly_lookback(101, '2016-07-01')

  # create valid return data.frame
  #wh_keys <- dated_whole_year_lookback(unique(valid.key_vals$id), max(valid.key_vals$end))
  wh_keys <- valid.key_vals

  wh_str_name <- warehouse_name_from_key(wh_keys)
  wh_str <- warehouse_objectstore_factory(wh_str_name)
  wh_str <- queryWarehouseStore(wh_str, wh_keys$id,
                                        wh_keys$start,
                                        wh_keys$end )

  wh <- getWarehouseFromStore(wh_str, wh_keys$id,
                                      wh_keys$start,
                                      wh_keys$end )

  valid.ret_data <- getData(getRawPositionData(wh))
  valid.ret_data <- valid.ret_data[valid.values]
  valid.ret_data <- valid.ret_data[valid.ret_data$Date >= valid.key_vals$start &
                                     valid.ret_data$Date <= valid.key_vals$end, ]
  colnames(valid.ret_data) <- TE.RefClasses:::.translateDataSourceColumnNames(object, valid.values)
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


