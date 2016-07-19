sourceTo("../common/trade_factory.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/trade_feature_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/preprocessor_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

features = c("MarketCap",
             "CompoundReturnInto",
             "CompoundReturnOutof",
             "PnLInto",
             "PnLOutof",
             "PriceInfluence",
             "ActivityIntoTrade",
             "ActivityOutofTrade")

if(exists("warehouse")==FALSE){
  trd_url_query = new("TradeHistoryURL",user_ids=as.integer(11),start=as.Date("2015-08-05"),end=as.Date("2015-08-15"))
  trd_data <- new("URLParser",parser_type = "XMLToFrame")
  trd_data <- runURLs(trd_data,c(trd_url_query@url))

  trd_dataset <- new("TradeHistoryDataSet",trader_id=trd_url_query@user_ids,start_date=trd_url_query@start,end_date=trd_url_query@end)
  trd_dataset <- setData(trd_dataset,getURLData(trd_data,1))

  warehouse <- new("TradeWarehouse")
  warehouse <- tradeFactory(warehouse,trd_dataset,fill_price=TRUE,fill_positions=TRUE)
  
  warehouse <- attachFeatures(warehouse,features)
  
}

qtiler <- new("MktCapQuartile")
qtiler <- quartileMktCap(qtiler,warehouse,features)
qtile_dataset <- getOutPut(qtiler)

