context("Test Trade Objectstore")

#############################
#
# Test TradeObjectStore
#
#############################
tested.class <- "TradeObjectStore"

valid.key  <- data.frame(id          = "1984",
                         instrument  = 4454L,
                         buysell     = "Buy",
                         strategy    = "LB_TEST",
                         leg_start   = as.Date("2016-03-29"),
                         leg_end     = as.Date("2016-04-01"),
                         status      = "Closed",
                         stringsAsFactors = FALSE)

valid.name <- get_trade_objectstore_name(valid.key)


test_that("Can create new Trade", {
  set.seed(1)
  valid.trade <<- new("Trade",
                      order_id = 1L,
                      trader_id = as.integer(valid.key$id),
                      leg_start = valid.key$leg_start,
                      leg_end = valid.key$leg_end,
                      long = TE.DataAccess:::test_long(valid.key$buysell),
                      buysell = valid.key$buysell,
                      value_usd =1000,
                      strategy = valid.key$strategy,
                      trader = valid.key$id,
                      instrument = valid.key$instrument,
                      consolidation = data.frame(TradeDate = seq(valid.key$leg_start,
                                                                 valid.key$leg_end,
                                                                 1),
                                                 ValueUSD  = sample(1000 + seq(100000)/100, 4),
                                                 Strategy  = valid.key$strategy,
                                                 OrderID   = sort(sample(50000 + seq(100), 4))
                                                 ),

                      status        = "Closed")

  expect_is(valid.trade, "Trade")
})

test_that("Can call trade_objectstore_factory() with locally existing file", {

  object <- trade_objectstore_factory(valid.key)

  expect_is(object, tested.class)

  object <- updateTradeStore(object, valid.trade, valid.key, TRUE)

  ret <- commitTradeStore(object)

  expect_true(ret)

  query <- getObjectStoreQuery(object)

  expect_is(query, "RemoteTradeQuery")

  is_known <- TE.DataAccess:::isKeyKnownInRemoteStore(query, valid.key)

  expect_true(is_known)

})


test_that("Can move local objectstore files to Blob Objectstore", {

  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))
  skip_if_not(FALSE)

  object <- TE.DataAccess:::update_trade_remote_storage()

})


test_that("Can check for keys in remote store() ", {

  object <- trade_objectstore_factory(valid.key)

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

  object <- trade_objectstore_factory(valid.key)

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

  object <- trade_objectstore_factory(valid.key)
  expect_is(object, tested.class)

  query <- getObjectStoreQuery(object)
  expect_is(query, "RemoteTradeQuery")

  local.key <- TE.DataAccess:::hashKey(query,valid.key)

  valid.path <- TE.DataAccess:::getPath(object)

  expect_true(file.exists(valid.path))

  expect_true(file.remove(valid.path))

  expect_false(file.exists(valid.path))

  object <- trade_objectstore_factory(valid.key)

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


