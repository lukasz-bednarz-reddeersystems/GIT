context("Test PPMOdel Objectstore")

#############################
#
# Test WarehouseObjectStore
#
#############################
tested.class <- "PPModeleObjectStore"
valid.name <- "TradeHistorySimpleWithSummary_11_2016-03-29_2016-04-01"
valid.key  <- TE.DataAccess:::key_from_ppmodel_objectstore_name(valid.name)


test_that("Can move local objectstore files to Blob Objectstore", {

  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))
  skip_if_not(FALSE)

  object <- update_warehouse_remote_storage()

})


test_that("Can call warehouse_objectstore_factory() with locally existing file", {

  object <- warehouse_objectstore_factory(valid.name)

  expect_is(object, tested.class)

})

test_that("Can check for keys in remote store() ", {

  object <- warehouse_objectstore_factory(valid.name)

  expect_is(object, tested.class)
  local.key <- TE.DataAccess:::generateKey(object,
                                     valid.key$id,
                                     valid.key$start,
                                     valid.key$end)


  query <- getObjectStoreQuery(object)
  expect_is(query, "RemoteWarehouseQuery")



  is_known <- TE.DataAccess:::isKeyKnown(query, local.key)
  expect_true(is_known)

  is_known <- TE.DataAccess:::isKeyKnownInLocalStore(query, local.key)
  expect_true(is_known)

  is_known <- TE.DataAccess:::isKeyKnownInRemoteStore(query, valid.key)
  expect_true(is_known)

})


test_that("Can check for keys in remote store() ", {

  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  local.key <- TE.DataAccess:::generateKey(object,
                                            valid.key$id,
                                            valid.key$start,
                                            valid.key$end)

  object <- warehouse_objectstore_factory(valid.name)

  expect_is(object, tested.class)

  query <- getObjectStoreQuery(object)
  expect_is(query, "RemoteWarehouseQuery")



  is_known <- TE.DataAccess:::isKeyKnown(query, local.key)
  expect_true(is_known)

  is_known <- TE.DataAccess:::isKeyKnownInLocalStore(query, local.key)
  expect_true(is_known)

  ret <- TE.DataAccess:::removeObjectFromRemoteStore(object)
  expect_true(ret %in% c(-1,0))

  is_known <- TE.DataAccess:::isKeyKnownInRemoteStore(query, valid.key)
  expect_false(is_known)

  ret <- TE.DataAccess:::saveObjectInRemoteStore(object)
  expect_true(ret)

  is_known <- TE.DataAccess:::isKeyKnownInRemoteStore(query, valid.key)
  expect_true(is_known)

})


test_that("Can load warehouse from remote store() ", {

  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  local.key <- TE.DataAccess:::generateKey(object,
                                           valid.key$id,
                                           valid.key$start,
                                           valid.key$end)

  object <- new(tested.class, valid.name)
  expect_is(object, tested.class)

  valid.path <- TE.DataAccess:::getPath(object)

  expect_true(file.exists(valid.path))

  expect_true(file.remove(valid.path))

  expect_false(file.exists(valid.path))

  object <- warehouse_objectstore_factory(valid.name)

  expect_is(object, tested.class)

  query <- getObjectStoreQuery(object)
  expect_is(query, "RemoteWarehouseQuery")



  is_known <- TE.DataAccess:::isKeyKnown(query, local.key)
  expect_true(is_known)

  is_known <- TE.DataAccess:::isKeyKnownInLocalStore(query, local.key)
  expect_true(is_known)

  is_known <- TE.DataAccess:::isKeyKnownInRemoteStore(query, valid.key)
  expect_true(is_known)


  ret <- TE.DataAccess:::removeObjectFromRemoteStore(object)
  expect_true(ret %in% c(-1,0))

  is_known <- TE.DataAccess:::isKeyKnownInRemoteStore(query, valid.key)
  expect_false(is_known)

  ret <- TE.DataAccess:::saveObjectInRemoteStore(object)
  expect_true(ret)

  is_known <- TE.DataAccess:::isKeyKnownInRemoteStore(query, valid.key)
  expect_true(is_known)

})


