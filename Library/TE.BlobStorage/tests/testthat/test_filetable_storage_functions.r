context("Test blob storage functions")

########################################
#
# Testing blob storage helper functions
#
########################################

# test vectors
valid.db            <- TE.BlobStorage:::.__DEFAULT_ODBC_DB_NAME__.
valid.schema        <- TE.BlobStorage:::.__DEFAULT_FILE_DB_SCHEMA__.
valid.path          <- "\\\\RAIDSTAGEDB\\MSSQLSERVER_STAGEDB\\FileTableDB\\BlobTest"
valid.table         <- "ftMultiFactorRiskBlobTest"
valid.testfilename  <- "temp.txt"
valid.tempfile     <- tempfile("temp", fileext = ".txt")
valid.tempfilename <- basename(valid.tempfile)
file.create(valid.tempfile)

test_that("Created temporary file.", {

  expect_true(file.exists(valid.tempfile))

})

test_that("Can get_filetable_path", {

  ret.path <- get_filetable_path(valid.table, valid.db, valid.schema)

  expect_equal(ret.path, valid.path)

})

test_that("Can call check_file_exists()", {
  expect_true(check_file_exists(valid.testfilename, valid.table, valid.db, valid.schema))
  expect_false(check_file_exists(valid.tempfilename, valid.table, valid.db, valid.schema))
})

test_that("Can call check_file_stored()", {
  expect_true(check_file_stored(valid.testfilename, valid.table, valid.db, valid.schema))
  expect_false(check_file_stored(valid.tempfilename, valid.table, valid.db, valid.schema))
})


test_that("Can store_file_in_filetable()", {

  # store new file
  ret <- store_file_in_filetable(valid.tempfile,
                                 valid.table,
                                 valid.db,
                                 valid.schema)

  expect_equal(ret, 0)
  expect_true(check_file_exists(valid.tempfilename, valid.table, valid.db, valid.schema))

  # try to store the same file without overwrite set
  ret <- store_file_in_filetable(valid.tempfile,
                                 valid.table,
                                 valid.db,
                                 valid.schema)

  expect_equal(ret, -1)
  expect_true(check_file_exists(valid.tempfilename, valid.table, valid.db, valid.schema))


  # try to store the same file with overwrite set
  ret <- store_file_in_filetable(valid.tempfile,
                                 valid.table,
                                 valid.db,
                                 valid.schema,
                                 overwrite = TRUE)
  expect_equal(ret, 1)
  expect_true(check_file_exists(valid.tempfilename, valid.table, valid.db, valid.schema))

})


test_that("Can remove_file_from_filetable()", {

  # remove file
  ret <- remove_file_from_filetable(valid.tempfilename,
                                    valid.table,
                                    valid.db,
                                    valid.schema)

  expect_equal(ret, 0)
  expect_false(check_file_exists(valid.tempfilename, valid.table, valid.db, valid.schema))

  # try to remove removed file
  ret <- remove_file_from_filetable(valid.tempfilename,
                                    valid.table,
                                    valid.db,
                                    valid.schema)

  expect_equal(ret, -1)


})
