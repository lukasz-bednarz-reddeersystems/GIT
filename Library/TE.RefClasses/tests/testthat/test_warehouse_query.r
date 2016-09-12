context("Testing Warehouse Query")

#########################
#
# PPModelObjectStore Tests
#
#########################

valid.trader_id <- 11
valid.end_date  <- "2016-04-01"
valid.key_func <- dated_three_day_lookback
valid.keys <- valid.key_func(valid.trader_id, valid.end_date)


test_that(paste("Can read from warehouse"), {

  skip_if_not(as.logical(Sys.getenv("R_TESTTHAT_RUN_LONG_TESTS", unset = "FALSE")))

  first <- TRUE

  for(i_row in seq(nrow(valid.keys))) {

    trader <- valid.keys$id[i_row]
    start <- valid.keys$start[i_row]
    end <- valid.keys$end[i_row]

    wh_str <- warehouse_objectstore_factory(TE.RefClasses:::warehouse_name_from_key(valid.keys[i_row,]))
    wh_str <- queryWarehouseStore(wh_str, trader, start, end)
    wh <- getWarehouseFromStore(wh_str, trader, start, end )


    if (first) {
      position_data <- getData(getRawPositionData(wh))
      first <- FALSE
    } else(
      position_data <- rbind(position_data, getData(getRawPositionData(wh)))
    )

  }

  expect_gt(nrow(position_data),0)
})
