sourceTo("../common/trade_factory.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/trade_feature_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

trd_url_query = new("TradeHistoryURL",user_ids=as.integer(101),start=as.Date("2015-02-01"),end=as.Date("2015-02-28"))
trd_data <- new("URLParser",parser_type = "XMLToFrame")
trd_data <- runURLs(trd_data,c(trd_url_query@url))

trd_dataset <- new("TradeHistoryDataSet",trader_id=trd_url_query@user_ids,start_date=trd_url_query@start,end_date=trd_url_query@end)
trd_dataset <- setData(trd_dataset,getURLData(trd_data,1))

warehouse <- new("TradeWarehouse")
warehouse <- tradeFactory(warehouse,trd_dataset)

warehouse <- fillTradeListPrices(warehouse,5234)
getInstrumentTrades(warehouse,5234)
