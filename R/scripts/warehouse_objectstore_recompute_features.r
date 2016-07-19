setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
library(lubridate)
sourceTo("../common/dataplex.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/warehouse_objectstore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/trade_feature_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

commit <- TRUE
recompute_features <- c("DaysSinceRelLow6m","DaysSinceRelLow3m")
trader <- as.integer(101)
period_start <- ymd('2016-05-01')
months <- 1

for(mnth in 1:months){
  start  <- as.Date(period_start %m+% months(mnth-1) %m-% days(1))
  end    <- as.Date(period_start %m+% months(mnth) %m-% days(1))
  warehouse_name <- paste(trader,"_",as.character(start),"_",as.character(end),sep="")
  wh_str <- warehouse_objectstore_factory(warehouse_name)
  wh_str <- queryWarehouseStore(wh_str,trader,start,end)
  wh <- getWarehouseFromStore(wh_str,trader,start,end)
  #debug(wh@features[["PsnReturnOut"]]@computation@compute)
  wh <- attachFeatures(wh,recompute_features)
  if(commit){
    wh_str <- pushFeatures(wh_str,wh,keep_old=FALSE)
    commitWarehouseStore(wh_str)  
  }
}





