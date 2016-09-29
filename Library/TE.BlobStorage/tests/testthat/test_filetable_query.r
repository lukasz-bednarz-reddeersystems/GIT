context("Test FileTable Query Classes")

###########################################
#
# Testing BlobStorage.VirtualSQLQuery class
#
###########################################

tested.class <- "BlobStorage.VirtualSQLQuery"
test.class   <- "TestFileTableSQLQuery"

test_that(sprintf("Cannot instantiate %s class", tested.class ),{
  expect_error(new(tested.class), regexp = "trying to generate an object from a virtual class")
})


test_that(sprintf("Can inherit from %s class", tested.class),{

  setClass(test.class, contains = tested.class)
  expect_true(isClass(test.class))

})


######################################################
#
# Testing BlobStorage.SQLQuery.FileTableRootPath class
#
######################################################

tested.class  <- "BlobStorage.SQLQuery.FileTableRootPath"
valid.tb_name <- "ftMultiFactorRiskBlobTest"
valid.db      <- TE.BlobStorage:::.__DEFAULT_ODBC_DB_NAME__.
valid.schema  <- TE.BlobStorage:::.__DEFAULT_FILE_DB_SCHEMA__.
valid.ret     <- data.frame(Path = "\\\\RAIDSTAGEDB\\MSSQLSERVER_STAGEDB\\FileTableDB\\BlobTest",
                            stringsAsFactors = FALSE)

test_that(sprintf("Cannot instantiate %s class without parameters", tested.class),{
  expect_error(new(tested.class), regexp = 'argument "db_name" is missing, with no default')
})


test_that(sprintf("Can instantiate %s class witht parameters", tested.class),{

  object <- new(tested.class, db_name = valid.db,
                              db_schema = valid.schema,
                              tb_name = valid.tb_name)

  expect_is(object, tested.class)
})

test_that(sprintf("Can executeSQLQuery on  %s class", tested.class),{

  object <- new(tested.class, db_name = valid.db,
                db_schema = valid.schema,
                tb_name = valid.tb_name)

  expect_is(object, tested.class)

  ret <- executeSQLQuery(object)

  expect_equal(ret, valid.ret)
})



##########################################
#
# Testing BlobStorage.SQLQuery.FileStoredInFileTable class
#
##########################################

tested.class        <- "BlobStorage.SQLQuery.FileStoredInFileTable"
valid.tb_name       <- "ftMultiFactorRiskBlobTest"
valid.db            <- TE.BlobStorage:::.__DEFAULT_ODBC_DB_NAME__.
valid.schema        <- TE.BlobStorage:::.__DEFAULT_FILE_DB_SCHEMA__.
valid.testfilename  <- "temp.txt"
valid.ret           <- data.frame(name = valid.testfilename,
                                  stringsAsFactors = FALSE)

test_that(sprintf("Cannot instantiate %s class without parameters", tested.class),{
  expect_error(new(tested.class), regexp = 'argument "db_name" is missing, with no default')
})


test_that(sprintf("Can instantiate %s class witht parameters", tested.class),{

  object <- new(tested.class, db_name = valid.db,
                db_schema = valid.schema,
                tb_name = valid.tb_name,
                filename = valid.testfilename)

  expect_is(object, tested.class)
})

test_that(sprintf("Can executeSQLQuery on  %s class", tested.class),{

  object <- new(tested.class, db_name = valid.db,
                db_schema = valid.schema,
                tb_name = valid.tb_name,
                filename = valid.testfilename)

  expect_is(object, tested.class)

  ret <- executeSQLQuery(object)

  expect_equal(ret, valid.ret)
})

