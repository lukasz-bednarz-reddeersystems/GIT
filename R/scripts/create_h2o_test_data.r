sourceTo("../common/trade_factory.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

trader <- as.integer(70)
start  <- as.Date("2015-03-01")
end    <- as.Date("2015-07-30")

trd_url_query = new("TradeHistoryURL",user_ids=trader,start=start,end=end)
trd_data <- new("URLParser",parser_type = "XMLToFrame")
trd_data <- runURLs(trd_data,c(trd_url_query@url))
  
trd_dataset <- new("TradeHistoryDataSet",trader_id=trd_url_query@user_ids,start_date=trd_url_query@start,end_date=trd_url_query@end)
trd_dataset <- setData(trd_dataset,getURLData(trd_data,1))
  
warehouse <- new("TradeWarehouse")
warehouse <- tradeFactory(warehouse,trd_dataset,fill_price=TRUE,fill_positions=TRUE)

#fill missing dates (weekends in)
#impute resulting missing price values with the last price
#all frames should be the same size
filldates_and_index <- function(trade){
  
}

#select price data for trades within a specific strategy
#and on a specific side
trades <- listTrades(warehouse)
first <- TRUE
for(trade in trades){
  t <- getTrade(warehouse,trade)
  if(t@strategy == 'BA_LHYBRID' && t@long == TRUE){
    if(first){
      price_data <- filldates_and_index(t)    
      first <- FALSE
    }
    else{
      price_data <- merge(price_data,filldates_and_index(t),by=c('Index'))
    }
  }
}



