sourceTo("../common/dataplex.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/trade_factory.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/composite_warehouse.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

trader <- as.integer(11)
start  <- as.Date("2015-10-01")
end    <- as.Date("2015-10-15")

load("C:/Development/AllRaid/TraderEnhacement/R/templates/js_q3_warehouse.RData")
composite_warehouse <- new("CompositeWarehouse")
composite_warehouse <- addWarehouse(composite_warehouse,warehouse,"JS_Q3_2015")
rm(warehouse)

trd_url_query = new("TradeHistoryURL",user_ids=trader,start=start,end=end)
trd_data <- new("URLParser",parser_type = "XMLToFrame")
trd_data <- runURLs(trd_data,c(trd_url_query@url))
  
trd_dataset <- new("TradeHistoryDataSet",trader_id=trd_url_query@user_ids,start_date=trd_url_query@start,end_date=trd_url_query@end)
trd_dataset <- setData(trd_dataset,getURLData(trd_data,1))
  
warehouse <- new("TradeWarehouse")
warehouse <- tradeFactory(warehouse,trd_dataset,fill_price=TRUE,fill_positions=TRUE)

composite_warehouse <- addWarehouse(composite_warehouse,warehouse,"JS_Oct_2015")