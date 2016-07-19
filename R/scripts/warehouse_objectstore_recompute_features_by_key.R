setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../common/dataplex.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/warehouse_objectstore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/trade_feature_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

trader <- as.integer(70)
date  <- as.Date("2015-10-01")
key_func <- dated_three_monthly_lookback
recompute_features <- c("PnLOutof","PtvePnLOutof","ProfitTarget","StopLoss")

kv <- key_func(trader,date)
for(k in 1:nrow(kv)){
  warehouse_name <- paste(trader,"_",as.character(kv$start[k]),"_",as.character(kv$end[k]),sep="")
  wh_str <- warehouse_objectstore_factory(warehouse_name)
  wh_str <- queryWarehouseStore(wh_str,trader,kv$start[k],kv$end[k])
  wh <- getWarehouseFromStore(wh_str,trader,kv$start[k],kv$end[k])
  wh <- attachFeatures(wh,recompute_features)
  wh_str <- pushFeatures(wh_str,wh,keep_old=FALSE)
  commitWarehouseStore(wh_str)  
}

