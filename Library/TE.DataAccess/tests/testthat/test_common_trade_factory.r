
context("Test Trade Warehouse")

##############################################################################################
#
# Testing TradeWarehouse
#
##############################################################################################
tested.class <- "TradeWarehouse"
valid.trader <- 11L
valid.start <- as.Date("2014-11-30")
valid.end   <- today()


test_that(sprintf("Can create instance of %s", tested.class),{

  object <- new(tested.class)

  expect_is(object, tested.class)
})


test_that(sprintf("Can build_warehouse()", tested.class),{

  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))
  object <- TE.DataAccess:::build_warehouse(valid.trader, valid.start, valid.end)

  expect_is(object, tested.class)
})


