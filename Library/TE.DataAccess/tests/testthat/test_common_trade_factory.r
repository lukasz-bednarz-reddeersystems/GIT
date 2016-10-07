
context("Test Trade Warehouse")

##############################################################################################
#
# Testing TradeWarehouse
#
##############################################################################################
tested.class <- "TradeWarehouse"
valid.trader <- 101L
valid.start <- as.Date("2016-09-28")
valid.end   <- as.Date("2016-09-28")


test_that(sprintf("Can create instance of %s", tested.class),{

  object <- new(tested.class)

  expect_is(object, tested.class)
})


test_that(sprintf("Can build_warehouse()", tested.class),{

  object <- TE.DataAccess:::build_warehouse(valid.trader, valid.start, valid.end)

  expect_is(object, tested.class)
})


