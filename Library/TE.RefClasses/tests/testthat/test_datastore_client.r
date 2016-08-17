context("Testing of DataStoreClient")

#########################
#
# DataStoreClient Tests
#
#########################


test_that("Canot create VirtualDataStoreClient object", {
  expect_error(new("VirtualDataStoreClient"),
               regexp = "trying to generate an object from a virtual class")
})

test_that("Can create TestDataStoreClient object", {
  expect_is(new("TestDataStoreClient"), "TestDataStoreClient")
})

test_that("Can use basic accessors of  TestDataStoreClient object", {

  dsc <- new("TestDataStoreClient")
  expect_is(dsc, "TestDataStoreClient")

  expect_equal(getDataStoreName(dsc), "test_datastore")

  expect_equal(getDataSourceQueryKeyColumnNames(dsc), c("lA", "dtB"))

  expect_equal(getDataSourceReturnColumnNames(dsc), c("lA","dtB","lC","sD","sE"))

  expect_equal(getDataSourceQueryKeyValues(dsc), data.frame(lA = numeric(), dtB = as.Date(character())))

  expect_equal(getDataSourceClientColumnNameMap(dsc), hash(c("lA","dtB","lC","sD","sE"),
                                                    c("A","B","C","D","E")))


})


test_that("Cannot .setDataSourceQueryKeyValues with invalid data", {

  dsc <- new("TestDataStoreClient")
  expect_error(.setDataSourceQueryKeyValues(dsc,
                                          data.frame(lA = numeric(), dtB = as.Date(character()))),
               regexp = "Zero row query keys data.frame passed")

  expect_error(.setDataSourceQueryKeyValues(dsc,
                                          data.frame(lC = numeric(), dtD = as.Date(character()))),
               regexp = "Invalid column names of query keys passed")

  expect_equal(getDataSourceQueryKeyValues(dsc), data.frame(lA = numeric(), dtB = as.Date(character())))

})


test_that("Can .setDataSourceQueryKeyValues with valid data", {

  dsc <- new("TestDataStoreClient")

  key_vals <- data.frame(lA = 1:2, dtB = c(today() -1, today()))

  dsc <- .setDataSourceQueryKeyValues(dsc, key_vals)

  expect_equal(getDataSourceQueryKeyValues(dsc), key_vals)

})


test_that("Cannot dataRequest() with invalid key_values", {

  dsc <- new("TestDataStoreClient")


  key_vals <- data.frame(lA = numeric(), dtB = as.Date(character()))

  expect_error(dataRequest(dsc, key_vals),
               regexp = "Zero row query keys data.frame passed")



  key_vals <- data.frame(lC = numeric(), dtD = as.Date(character()))

  expect_error(dataRequest(dsc, key_vals),
               regexp = "Invalid column names of query keys passed")



  key_vals <- data.frame(lA = 1:2, dtB = c(today() -1, today()))

  expect_error(dataRequest(dsc, key_vals),
               regexp = "Query sent to test_datastore returned zero row data.frame")


  expect_equal(getDataSourceQueryKeyValues(dsc), data.frame(lA = numeric(), dtB = as.Date(character())))

})


test_that("Can dataRequest() with valid key_values", {

  dsc <- new("TestDataStoreClient")
  key_names <- getDataSourceQueryKeyColumnNames(dsc)
  factor_cols <- "sD"

  key_vals <- data.frame(lA = 1:2, dtB = as.Date(c("2016-05-01", "2016-05-02")))

  dsc <- dataRequest(dsc, key_vals)

  expect_equal(key_names, colnames(key_vals))



  res_data <- merge(dsc@test_df, key_vals, key_names)
  tr_data <- TE.RefClasses:::factor_transform(res_data[c(key_names, factor_cols)], key_names, factor_cols )
  res_data <- cbind(res_data[setdiff(colnames(res_data), factor_cols)],
                    tr_data[setdiff(colnames(tr_data), colnames(res_data))])

  colnames(res_data)[1:4] <- LETTERS[c(1,2,3,5)]

  ret_data <- getReferenceData(dsc)

  res_data <- res_data[colnames(ret_data)]

  expect_equal(getDataSourceQueryKeyValues(dsc), key_vals)

  expect_equal(getReferenceData(dsc), res_data)


  dsc <- new("TestDataStoreClient")
  key_vals <- data.frame(lA = 1:10,
                         dtB = seq(from = as.Date("2016-05-01"), to = as.Date("2016-05-10"), by = "1 day"))

  dsc <- dataRequest(dsc, key_vals)

  expect_equal(getDataSourceQueryKeyColumnNames(dsc), colnames(key_vals))

  res_data <- dsc@test_df
  tr_data <- TE.RefClasses:::factor_transform(res_data[c(key_names, factor_cols)], key_names, factor_cols )
  res_data <- cbind(res_data[setdiff(colnames(res_data), factor_cols)],
                    tr_data[setdiff(colnames(tr_data), colnames(res_data))])
  #res_data <- res_data[!is.na(res_data['sD']),]
  rownames(res_data) <- seq(nrow(res_data))

  colnames(res_data)[1:4] <- LETTERS[c(1,2,3,5)]

  ret_data <- getReferenceData(dsc)

  res_data <- res_data[colnames(ret_data)]

  expect_equal(getDataSourceQueryKeyValues(dsc), key_vals)

  expect_equal(getReferenceData(dsc), res_data)

})


