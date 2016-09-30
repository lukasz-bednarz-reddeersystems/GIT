context("Test Trade Objectstore")

#############################
#
# Test TradeObjectStore
#
#############################
tested.class <- "TradeObjectStore"

valid.key  <- data.frame(id          = 1984L,
                         instrument  = 4454L,
                         buysell     = "Buy",
                         strategy    = "LB_TEST",
                         start       = as.Date("2016-03-29"),
                         end         = as.Date("2016-04-01"))

test_that("Can create new trade") {
  valid.trade <<- new("Trade",
                      leg_start = valid.key$start,
                      leg_end = valid.key$start,
                      long = TE.DataAccess:::test_long(valid.key$buysell),
                      value_usd =1000,
                      strategy = valid.key$strategy,
                      trader = valid.key$id,
                      instrument = valid.key$instrument,
                      consolidation = data.frame(TradeDate = seq(valid.key$start,
                                                                 valid.key$end,
                                                                 1),
                                                 ValueUSD  = sample(1000 + seq(100000)/100, 4),
                                                 Strategy  = valid.key$strategy,
                                                 OrderID   = sort(sample(50000 + seq(100), 4))
                                                 ),

                      status        = "Closed")

  expect_is(valid.trade, "Trade")
}



valid.name <- get_trade_objectstore_name(valid.key)
valid.key2  <- TE.DataAccess:::key_from_trade_objectstore_name(valid.name)

test_that("Key generators are working properly", {
  expect_equivalent(valid.key, valid.key2)
})


test_that("Can move local objectstore files to Blob Objectstore", {

  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))
  skip_if_not(FALSE)

  object <- TE.DataAccess:::update_trade_remote_storage()

})


test_that("Can call trade_objectstore_factory() with locally existing file", {

  object <- trade_objectstore_factory(valid.name)

  expect_is(object, tested.class)

})

test_that("Can check for keys in remote store() ", {

  object <- trade_objectstore_factory(valid.name)

  expect_is(object, tested.class)

  query <- getObjectStoreQuery(object)
  expect_is(query, "RemoteTradeQuery")

  local.key <- TE.DataAccess:::hashKey(query,valid.key)


  is_known <- TE.DataAccess:::isKeyKnown(query, local.key)
  expect_true(is_known)

  is_known <- TE.DataAccess:::isKeyKnownInLocalStore(query, local.key)
  expect_true(is_known)

  is_known <- TE.DataAccess:::isKeyKnownInRemoteStore(query, valid.key)
  expect_true(is_known)

})


test_that("Can check for keys in remote store() ", {

  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  object <- trade_objectstore_factory(valid.name)

  expect_is(object, tested.class)

  query <- getObjectStoreQuery(object)
  expect_is(query, "RemoteTradeQuery")

  local.key <- TE.DataAccess:::hashKey(query,valid.key)


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

  object <- new(tested.class, valid.name)
  expect_is(object, tested.class)

  query <- getObjectStoreQuery(object)
  expect_is(query, "RemoteTradeQuery")

  local.key <- TE.DataAccess:::hashKey(query,valid.key)

  valid.path <- TE.DataAccess:::getPath(object)

  expect_true(file.exists(valid.path))

  expect_true(file.remove(valid.path))

  expect_false(file.exists(valid.path))

  object <- trade_objectstore_factory(valid.name)

  expect_is(object, tested.class)

  query <- getObjectStoreQuery(object)
  expect_is(query, "RemoteTradeQuery")

  local.key <- TE.DataAccess:::hashKey(query,valid.key)


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


