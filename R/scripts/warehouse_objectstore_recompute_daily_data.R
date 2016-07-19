setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
library(lubridate)
sourceTo("../common/dataplex.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/warehouse_objectstore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/trade_feature_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

commit            <- TRUE
fill_psns         <- TRUE
fill_price        <- FALSE
fill_levels       <- FALSE
update_trades     <- TRUE
fields <- c('strategy')
recompute_summary <- TRUE
trader <- as.integer(70)
period_start <- ymd('2015-01-01')
months <- 8

for(mnth in 1:months){
  start  <- as.Date(period_start %m+% months(mnth-1) %m-% days(1))
  end    <- as.Date(period_start %m+% months(mnth) %m-% days(1))
  warehouse_name <- paste(trader,"_",as.character(start),"_",as.character(end),sep="")
  wh_str <- warehouse_objectstore_factory(warehouse_name)
  wh_str <- queryWarehouseStore(wh_str,trader,start,end)
  wh <- getWarehouseFromStore(wh_str,trader,start,end)
  instruments <- listInstruments(wh)
  if(recompute_summary) wh <- fillPositionDataAndSummarise(wh)
  for(ins in instruments){
    if(fill_psns)       wh <- fillTradeListPosns(wh,ins)
    if(fill_price)      wh <- fillTradeListPrices(wh,ins)
    if(fill_levels)     wh <- fillTradeLevels(wh,ins)
  }
  if(commit){
    if(fill_psns||fill_price||fill_levels)wh_str <- pushTradeDailyData(wh_str,wh,keep_old=FALSE)
    if(update_trades)wh_str <- pushTradeFields(wh_str,wh,fields)
    if(recompute_summary) wh_str <- pushSummary(wh_str,wh)
    commitWarehouseStore(wh_str)  
  }
}








