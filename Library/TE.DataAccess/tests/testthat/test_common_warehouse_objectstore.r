context("Test Trade Warehouse Objectstore")

#############################
#
# Test WarehouseObjectStore
#
#############################
tested.class <- "WarehouseObjectStore"
valid.name <- "11_2016-06-30_2016-07-31"
valid.key  <- TE.DataAccess:::key_from_name(valid.name)

test_that("Can move local objectstore files to Blob Objectstore", {

  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- update_warehouse_remote_storage()

})


test_that("Can call warehouse_objectstore_factory() with locally existing file", {

  object <- warehouse_objectstore_factory(valid.name)

  expect_is(object, tested.class)

})

test_that("Can check for keys in remote store() ", {

  object <- warehouse_objectstore_factory(valid.name)

  expect_is(object, tested.class)
  key <- TE.DataAccess:::generateKey(object,
                                     valid.key$id,
                                     valid.key$start,
                                     valid.key$end)


  query <- getObjectStoreQuery(object)
  expect_is(query, "RemoteWarehouseQuery")



  is_known <- TE.DataAccess:::isKeyKnown(query, key)
  expect_true(is_known)

  is_known <- TE.DataAccess:::isKeyKnownInLocalStore(query, key)
  expect_true(is_known)

  is_known <- TE.DataAccess:::isKeyKnownInRemoteStore(query, key)
  expect_false(is_known)

})


test_that("Can check for keys in remote store() ", {

  object <- warehouse_objectstore_factory(valid.name)

  expect_is(object, tested.class)

  is_known <- TE.DataAccess:::isKeyKnown(object, valid.key)
  expect_true(is_known)

  is_known <- TE.DataAccess:::isKeyKnownInLocalStore(object, valid.key)
  expect_true(is_known)

  is_known <- TE.DataAccess:::isKeyKnownInRemoteStore(object, valid.key)
  expect_false(is_known)



  object <- TE.DataAccess:::saveObjectInRemoteStore(object)
  expect_is(object, tested.class)

})
