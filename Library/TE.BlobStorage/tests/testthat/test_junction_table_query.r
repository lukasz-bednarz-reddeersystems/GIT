context("Test JunctionTable Query Classes")

#####################################################
#
# Testing BlobStorage.VirtualSQLProcedureCall class
#
#####################################################

tested.class <- "BlobStorage.VirtualSQLProcedureCall"
test.class   <- "TestFileTableSQLQuery"

test_that(sprintf("Cannot instantiate %s class", tested.class ),{
  expect_error(new(tested.class), regexp = "trying to generate an object from a virtual class")
})


test_that(sprintf("Can inherit from %s class", tested.class),{

  setClass(test.class, contains = tested.class)
  expect_true(isClass(test.class))

})


########################################################################################
#
# Testing BlobStorage.SQLProcedureCall.ReferencedFileTable_SelectByParentTableName class
#
########################################################################################

tested.class  <- "BlobStorage.SQLProcedureCall.ReferencedFileTable_SelectByParentTableName"
valid.tb_name <- "tMultiFactorRiskBlobTest"
valid.db      <- TE.BlobStorage:::.__DEFAULT_ODBC_DB_NAME__.
valid.schema  <- TE.BlobStorage:::.__DEFAULT_FILE_DB_SCHEMA__.
valid.ret     <- data.frame(ReferencedTableName = "ftMultiFactorRiskBlobTest",
                            ParentColName = "hPathLocator",
                            ReferencedColName = "path_locator",
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


########################################################################################
#
# Testing BlobStorage.SQLProcedureCall.JointFileTable_QueryByTbNameTraderIDStartDateEndDate class
#
########################################################################################



tested.class  <- "BlobStorage.SQLProcedureCall.JointFileTable_QueryByHashID"
valid.tb_name <- "tMultiFactorRiskBlobTest"
valid.db      <- TE.BlobStorage:::.__DEFAULT_ODBC_DB_NAME__.
valid.schema  <- TE.BlobStorage:::.__DEFAULT_FILE_DB_SCHEMA__.
valid.keys    <- data.frame(TraderID  = 11L,
                            StartDate = as.Date("2016-01-01"),
                            EndDate   = as.Date("2016-01-01"))

valid.hash    <- TE.BlobStorage:::hash_data_frame(valid.keys)

valid.keys    <- data.frame(HashID = valid.hash)

valid.ret     <- data.frame(sHashID       = valid.hash,
                            dtCreatedDate = as.Date("2016-09-25"),
                            sCreatedByUserID = "lukasz.bednarz",
                            sFileName  = "temp.txt",
                            stringsAsFactors = FALSE)

test_that(sprintf("Cannot instantiate %s class without parameters", tested.class),{
  expect_error(new(tested.class), regexp = 'argument "db_name" is missing, with no default')
})


test_that(sprintf("Can instantiate %s class without keys parameter", tested.class),{

  object <- new(tested.class, db_name = valid.db,
                db_schema = valid.schema,
                tb_name = valid.tb_name)

  expect_is(object, tested.class)
})

test_that(sprintf("Can instantiate %s class with keys parameter", tested.class),{

  object <- new(tested.class, db_name = valid.db,
                db_schema = valid.schema,
                tb_name = valid.tb_name,
                keys    = valid.keys)

  expect_is(object, tested.class)
})


test_that(sprintf("Can prepareSQLQuery on  %s", tested.class),{

  object <- new(tested.class, db_name = valid.db,
                db_schema = valid.schema,
                tb_name = valid.tb_name)

  expect_is(object, tested.class)

  keys <- cbind(data.frame(TableName = valid.tb_name), valid.keys)

  object <- prepareSQLQuery(object, keys)

  expect_equal(getSQLQueryKeyValues(object), keys)

})



test_that(sprintf("Can executeSQLQuery on  %s class without calling prepareSQLQuery()",
                  tested.class),{

  object <- new(tested.class, db_name = valid.db,
                db_schema = valid.schema,
                tb_name = valid.tb_name,
                keys    = valid.keys)

  expect_is(object, tested.class)

  ret <- executeSQLQuery(object)

  expect_equal(ret, valid.ret)
})




test_that(sprintf("Can executeSQLQuery on  %s class with calling prepareSQLQuery()",
                  tested.class),{

  object <- new(tested.class, db_name = valid.db,
                db_schema = valid.schema,
                tb_name = valid.tb_name)

  expect_is(object, tested.class)

  keys <- cbind(data.frame(TableName = valid.tb_name), valid.keys)

  object <- prepareSQLQuery(object, keys)

  expect_equal(getSQLQueryKeyValues(object), keys)

  ret <- executeSQLQuery(object)

  expect_equal(ret, valid.ret)

})



########################################################################################
#
# Testing BlobStorage.SQLProcedureCall.JointFileTable_UpdateByTbNameTraderIDInstrumentIDLegStartDateLegEndDate class
#
########################################################################################

tested.class  <- "BlobStorage.SQLProcedureCall.JointFileTable_UpdateByTbNameTraderIDInstrumentIDLegStartDateLegEndDate"
valid.tb_name <- "tRDTE_TradesObjectstore"
valid.db      <- TE.BlobStorage:::.__DEFAULT_ODBC_DB_NAME__.
valid.schema  <- TE.BlobStorage:::.__DEFAULT_FILE_DB_SCHEMA__.
valid.keys    <- data.frame(TraderID     = 1984L,
                            InstrumentID = 4454L,
                            Direction    = "Buy",
                            Strategy     = "LB_TEST",
                            LegStartDate = as.Date("2016-03-29"),
                            LegEndDate   = as.Date("2016-04-01")
                            )

valid.hash    <- TE.BlobStorage:::hash_data_frame(valid.keys)

valid.keys    <- cbind(data.frame(HashID      = valid.hash),
                       valid.keys,
                       data.frame(LegStatus   = "Closed",
                                  CreatedDate = as.Date("2016-10-12"),
                                  CreatedByUserID = "lukasz.bednarz",
                                  FileName    = "trade_store_1984_4454_Buy_LB_TEST_2016-03-29_objectstore_2016-10-12_lukasz.bednarz.rds"))

valid.ret     <- data.frame(KeyAlreadyStored  = 1,
                            KeyInserted = 0,
                            stringsAsFactors = FALSE)

test_that(sprintf("Cannot instantiate %s class without parameters", tested.class),{
  expect_error(new(tested.class), regexp = 'argument "db_name" is missing, with no default')
})


test_that(sprintf("Can instantiate %s class without keys parameter", tested.class),{

  object <- new(tested.class, db_name = valid.db,
                db_schema = valid.schema,
                tb_name = valid.tb_name)

  expect_is(object, tested.class)
})

test_that(sprintf("Can instantiate %s class with keys parameter", tested.class),{

  object <- new(tested.class, db_name = valid.db,
                db_schema = valid.schema,
                tb_name = valid.tb_name,
                keys    = valid.keys)

  expect_is(object, tested.class)
})


test_that(sprintf("Can prepareSQLQuery on  %s", tested.class),{

  object <- new(tested.class, db_name = valid.db,
                db_schema = valid.schema,
                tb_name = valid.tb_name)

  expect_is(object, tested.class)

  keys <- cbind(data.frame(TableName = valid.tb_name), valid.keys)

  object <- prepareSQLQuery(object, keys)

  expect_equal(getSQLQueryKeyValues(object), keys)

})



test_that(sprintf("Can executeSQLQuery on  %s class without calling prepareSQLQuery()",
                  tested.class),{

                    object <- new(tested.class, db_name = valid.db,
                                  db_schema = valid.schema,
                                  tb_name = valid.tb_name,
                                  keys    = valid.keys)

                    expect_is(object, tested.class)

                    ret <- executeSQLQuery(object)

                    expect_equal(ret, valid.ret)
})




test_that(sprintf("Can executeSQLQuery on  %s class with calling prepareSQLQuery()",
                  tested.class),{

                    object <- new(tested.class, db_name = valid.db,
                                  db_schema = valid.schema,
                                  tb_name = valid.tb_name)

                    expect_is(object, tested.class)

                    keys <- cbind(data.frame(TableName = valid.tb_name), valid.keys)

                    object <- prepareSQLQuery(object, keys)

                    expect_equal(getSQLQueryKeyValues(object), keys)

                    ret <- executeSQLQuery(object)

                    expect_equal(ret, valid.ret)

})



########################################################################################
#
# Testing BlobStorage.SQLProcedureCall.JointFileTable_QueryByTbNameTraderIDInstrumentIDLegStartDateLegEndDate class
#
########################################################################################



tested.class  <- "BlobStorage.SQLProcedureCall.JointFileTable_QueryByTbNameTraderIDInstrumentIDLegStartDateLegEndDate"
valid.tb_name <- "tRDTE_TradesObjectstore"
valid.db      <- TE.BlobStorage:::.__DEFAULT_ODBC_DB_NAME__.
valid.schema  <- TE.BlobStorage:::.__DEFAULT_FILE_DB_SCHEMA__.
valid.keys    <- data.frame(TraderID     = 1984L,
                            InstrumentID = 4454L,
                            Direction    = "Buy",
                            Strategy     = "LB_TEST",
                            LegStartDate = as.Date("2016-03-29"),
                            LegEndDate   = as.Date("2016-04-01"))

valid.ret     <- data.frame(sHashID    = valid.hash,
                            lTraderID  = 1984L,
                            lInstrumentID = 4454L,
                            sDirection    = "Buy",
                            sStrategy     = "LB_TEST",
                            dtLegStartDate = as.Date("2016-03-29"),
                            dtLegEndDate   = as.Date("2016-04-01"),
                            sLegStatus     = "Closed",
                            dtCreatedDate = as.Date("2016-10-04"),
                            sCreatedByUserID = "lukasz.bednarz",
                            sFileName  = "trade_store_1984_4454_Buy_LB_TEST_2016-03-29_objectstore_2016-10-04_lukasz.bednarz.rds",
                            stringsAsFactors = FALSE)

test_that(sprintf("Cannot instantiate %s class without parameters", tested.class),{
  expect_error(new(tested.class), regexp = 'argument "db_name" is missing, with no default')
})


test_that(sprintf("Can instantiate %s class without keys parameter", tested.class),{

  object <- new(tested.class, db_name = valid.db,
                db_schema = valid.schema,
                tb_name = valid.tb_name)

  expect_is(object, tested.class)
})

test_that(sprintf("Can instantiate %s class with keys parameter", tested.class),{

  object <- new(tested.class, db_name = valid.db,
                db_schema = valid.schema,
                tb_name = valid.tb_name,
                keys    = valid.keys)

  expect_is(object, tested.class)
})


test_that(sprintf("Can prepareSQLQuery on  %s", tested.class),{

  object <- new(tested.class, db_name = valid.db,
                db_schema = valid.schema,
                tb_name = valid.tb_name)

  expect_is(object, tested.class)

  keys <- cbind(data.frame(TableName = valid.tb_name), valid.keys)

  object <- prepareSQLQuery(object, keys)

  expect_equal(getSQLQueryKeyValues(object), keys)

})



test_that(sprintf("Can executeSQLQuery on  %s class without calling prepareSQLQuery()",
                  tested.class),{

  object <- new(tested.class, db_name = valid.db,
                db_schema = valid.schema,
                tb_name = valid.tb_name,
                keys    = valid.keys)

  expect_is(object, tested.class)

  ret <- executeSQLQuery(object)

  expect_equal(ret, valid.ret)
})




test_that(sprintf("Can executeSQLQuery on  %s class with calling prepareSQLQuery()",
                  tested.class),{

  object <- new(tested.class, db_name = valid.db,
                db_schema = valid.schema,
                tb_name = valid.tb_name)

  expect_is(object, tested.class)

  keys <- cbind(data.frame(TableName = valid.tb_name), valid.keys)

  object <- prepareSQLQuery(object, keys)

  expect_equal(getSQLQueryKeyValues(object), keys)

  ret <- executeSQLQuery(object)

  expect_equal(ret, valid.ret)

})



########################################################################################
#
# Testing BlobStorage.SQLProcedureCall.JointFileTable_UpdateByTbNameTraderIDStartDateEndDate class
#
########################################################################################

# tested.class  <- "BlobStorage.SQLProcedureCall.JointFileTable_UpdateByTbNameTraderIDStartDateEndDate"
# valid.tb_name <- "tMultiFactorRiskBlobTest"
# valid.db      <- TE.BlobStorage:::.__DEFAULT_ODBC_DB_NAME__.
# valid.schema  <- TE.BlobStorage:::.__DEFAULT_FILE_DB_SCHEMA__.
# valid.keys    <- data.frame(TraderID  = 11,
#                             StartDate = as.Date("2016-01-01"),
#                             EndDate   = as.Date("2016-01-01"),
#                             CreatedDate = as.Date("2016-08-01"),
#                             CreatedBy   = "Lukasz.Bednarz",
#                             FileName    = "temp.txt")
# valid.ret     <- data.frame(KeyAlreadyStored  = 1,
#                             KeyInserted = 0,
#                             stringsAsFactors = FALSE)
#
# test_that(sprintf("Cannot instantiate %s class without parameters", tested.class),{
#   expect_error(new(tested.class), regexp = 'argument "db_name" is missing, with no default')
# })
#
#
# test_that(sprintf("Can instantiate %s class without keys parameter", tested.class),{
#
#   object <- new(tested.class, db_name = valid.db,
#                 db_schema = valid.schema,
#                 tb_name = valid.tb_name)
#
#   expect_is(object, tested.class)
# })
#
# test_that(sprintf("Can instantiate %s class with keys parameter", tested.class),{
#
#   object <- new(tested.class, db_name = valid.db,
#                 db_schema = valid.schema,
#                 tb_name = valid.tb_name,
#                 keys    = valid.keys)
#
#   expect_is(object, tested.class)
# })
#
#
# test_that(sprintf("Can prepareSQLQuery on  %s", tested.class),{
#
#   object <- new(tested.class, db_name = valid.db,
#                 db_schema = valid.schema,
#                 tb_name = valid.tb_name)
#
#   expect_is(object, tested.class)
#
#   keys <- cbind(data.frame(TableName = valid.tb_name), valid.keys)
#
#   object <- prepareSQLQuery(object, keys)
#
#   expect_equal(getSQLQueryKeyValues(object), keys)
#
# })
#
#
#
# test_that(sprintf("Can executeSQLQuery on  %s class without calling prepareSQLQuery()",
#                   tested.class),{
#
#                     object <- new(tested.class, db_name = valid.db,
#                                   db_schema = valid.schema,
#                                   tb_name = valid.tb_name,
#                                   keys    = valid.keys)
#
#                     expect_is(object, tested.class)
#
#                     ret <- executeSQLQuery(object)
#
#                     expect_equal(ret, valid.ret)
# })
#
#
#
#
# test_that(sprintf("Can executeSQLQuery on  %s class with calling prepareSQLQuery()",
#                   tested.class),{
#
#                     object <- new(tested.class, db_name = valid.db,
#                                   db_schema = valid.schema,
#                                   tb_name = valid.tb_name)
#
#                     expect_is(object, tested.class)
#
#                     keys <- cbind(data.frame(TableName = valid.tb_name), valid.keys)
#
#                     object <- prepareSQLQuery(object, keys)
#
#                     expect_equal(getSQLQueryKeyValues(object), keys)
#
#                     ret <- executeSQLQuery(object)
#
#                     expect_equal(ret, valid.ret)
#
# })





########################################################################################
#
# Testing BlobStorage.SQLProcedureCall.JointFileTable_QueryByTbNameTraderIDStartDateEndDate class
#
########################################################################################



# tested.class  <- "BlobStorage.SQLProcedureCall.JointFileTable_QueryByTbNameTraderIDStartDateEndDate"
# valid.tb_name <- "tMultiFactorRiskBlobTest"
# valid.db      <- TE.BlobStorage:::.__DEFAULT_ODBC_DB_NAME__.
# valid.schema  <- TE.BlobStorage:::.__DEFAULT_FILE_DB_SCHEMA__.
# valid.keys    <- data.frame(TraderID  = 11,
#                             StartDate = as.Date("2016-01-01"),
#                             EndDate   = as.Date("2016-01-01"))
# valid.ret     <- data.frame(lTraderID  = 11L,
#                             dtStartDate = as.Date("2016-01-01"),
#                             dtEndDate   = as.Date("2016-01-01"),
#                             dtCreatedDate = as.Date("2016-07-31"),
#                             sCreatedByUserID = "Lukasz.Bednarz",
#                             sFileName  = "temp.txt",
#                             stringsAsFactors = FALSE)
#
# test_that(sprintf("Cannot instantiate %s class without parameters", tested.class),{
#   expect_error(new(tested.class), regexp = 'argument "db_name" is missing, with no default')
# })
#
#
# test_that(sprintf("Can instantiate %s class without keys parameter", tested.class),{
#
#   object <- new(tested.class, db_name = valid.db,
#                 db_schema = valid.schema,
#                 tb_name = valid.tb_name)
#
#   expect_is(object, tested.class)
# })
#
# test_that(sprintf("Can instantiate %s class with keys parameter", tested.class),{
#
#   object <- new(tested.class, db_name = valid.db,
#                 db_schema = valid.schema,
#                 tb_name = valid.tb_name,
#                 keys    = valid.keys)
#
#   expect_is(object, tested.class)
# })
#
#
# test_that(sprintf("Can prepareSQLQuery on  %s", tested.class),{
#
#   object <- new(tested.class, db_name = valid.db,
#                 db_schema = valid.schema,
#                 tb_name = valid.tb_name)
#
#   expect_is(object, tested.class)
#
#   keys <- cbind(data.frame(TableName = valid.tb_name), valid.keys)
#
#   object <- prepareSQLQuery(object, keys)
#
#   expect_equal(getSQLQueryKeyValues(object), keys)
#
# })
#
#
#
# test_that(sprintf("Can executeSQLQuery on  %s class without calling prepareSQLQuery()",
#                   tested.class),{
#
#   object <- new(tested.class, db_name = valid.db,
#                 db_schema = valid.schema,
#                 tb_name = valid.tb_name,
#                 keys    = valid.keys)
#
#   expect_is(object, tested.class)
#
#   ret <- executeSQLQuery(object)
#
#   expect_equal(ret, valid.ret)
# })
#
#
#
#
# test_that(sprintf("Can executeSQLQuery on  %s class with calling prepareSQLQuery()",
#                   tested.class),{
#
#   object <- new(tested.class, db_name = valid.db,
#                 db_schema = valid.schema,
#                 tb_name = valid.tb_name)
#
#   expect_is(object, tested.class)
#
#   keys <- cbind(data.frame(TableName = valid.tb_name), valid.keys)
#
#   object <- prepareSQLQuery(object, keys)
#
#   expect_equal(getSQLQueryKeyValues(object), keys)
#
#   ret <- executeSQLQuery(object)
#
#   expect_equal(ret, valid.ret)
#
# })



