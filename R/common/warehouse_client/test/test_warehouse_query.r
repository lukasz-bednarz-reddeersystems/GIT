sourceTo("../common/trade_factory.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/warehouse_client/warehouse_client_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/warehouse_objectstore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

library(testthat)

#########################
#
# TradeWarehouse Tests
#
#########################

tested.class  <-  "PPModelObjectStore"
valid.trader_id <- 11
valid.end_date  <- "2016-04-01"
valid.key_func <- three_year_lookback
valid.keys <- valid.key_func(valid.trader_id)

#########################
#
# PPModelObjectStore Tests
#
#########################


test_that(paste("Can read from warehouse"), {
  
  
  first <- TRUE
  
  for(i_row in seq(nrow(valid.keys))) {
    
    trader <- valid.keys$id[i_row]
    start <- valid.keys$start[i_row]
    end <- valid.keys$end[i_row]
    
    wh_str <- warehouse_objectstore_factory(warehouse_name_from_key(valid.keys[i_row,]))
    wh_str <- queryWarehouseStore(wh_str, trader, start, end)
    wh <- getWarehouseFromStore(wh_str, trader, start, end )
    
    
    if (first) {
      position_data <- getData(getRawPositionData(wh))
      first <- FALSE
    } else(
      position_data <- rbind(position_data, getData(getRawPositionData(wh)))
    )
  
  }
})
