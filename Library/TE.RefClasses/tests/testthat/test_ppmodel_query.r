context("Testing PPModelObjectStore access")

#########################
#
# Test Vectors
#
#########################
tested.class  <-  "PPModelObjectStore"

if (Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")) {

  suppressMessages({
    valid.trader_id <- 11
    valid.end_date  <- "2016-06-01"
    valid.key_func <- dated_three_day_lookback
    valid.keys <- valid.key_func(valid.trader_id, valid.end_date)
    valid.ppmodel <- new('TradeHistorySimple', keys = valid.keys )

    valid.store_keys <- data.frame(model_class = "TradeHistorySimple",
                                   id = valid.trader_id,
                                   start = min(valid.keys$start),
                                   end = max(valid.keys$end))

    valid.ppmodel <- runPreProcessorModel(valid.ppmodel)
    valid.ids <- get_ppmodel_objectstore_name(valid.store_keys)
  }
  )
}

if (Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")) {
  suppressMessages({
    tested.class  <-  "PPModelObjectStore"
    valid.trader_id <- 11
    valid.end_date  <- "2016-04-01"
    valid.key_func <- dated_three_day_lookback
    valid.keys <- valid.key_func(valid.trader_id, valid.end_date)
    valid.ppmodel <- new('TradeHistorySimple', keys = valid.keys )

    valid.store_keys <- data.frame(model_class = "TradeHistorySimple",
                                   id = valid.trader_id,
                                   start = min(valid.keys$start),
                                   end = max(valid.keys$end))

    valid.ppmodel <- runPreProcessorModel(valid.ppmodel)
    valid.ids <- get_ppmodel_objectstore_name(valid.store_keys)
  }
  )
}
###########################
#
# PPModelObjectStore Tests
#
###########################

test_that(paste("Store the model in the ", tested.class) ,{
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  ppm_store <- ppmodel_objectstore_factory(valid.ids)

  ppm_store <- updatePPModelStore(ppm_store, valid.ppmodel, valid.store_keys, force = TRUE)

  ppm_store <- commitPPModelStore(ppm_store)

})

test_that(paste("Can retrieve the model of the class ", tested.class) ,{
  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  ppm_store <- ppmodel_objectstore_factory(valid.ids)

  ppm <- queryPPModelStore(ppm_store, valid.store_keys)

  expect_true(!is.null(ppm))

  expect_equal(ppm, valid.ppmodel)

  expect_equal(getData(ppm@modeldata), getData(valid.ppmodel@modeldata))

})
