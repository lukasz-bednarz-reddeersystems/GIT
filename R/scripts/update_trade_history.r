setwd("C:/Development/AllRaid/branches/dev_lukasz.bednarz/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules_legacy/analysis_module_position_holding_period.r", modifiedOnly = TRUE, local = FALSE)

m <- history_analysis_module_builder
traders  <- c(11,70,101)
date     <- '2016-07-01'
for(trader in traders){
  key_func <- function(){dated_twelve_monthly_lookback(trader,date)}
  analysis <- createAnalysisModule(m,key_func)
  #debug(updateAnalysisModel)
  analysis <- updateAnalysisModel(analysis)
  #analysis <- runAnalysisModule(analysis)
  
  key_hash <- as.character(murmur3.32(as.character(key_func())))
  kv <- key_func()
  hrname <- paste(kv[[1,1]],"_",as.character(min(kv[['start']])),"_",as.character(max(kv[['end']])),sep="")
  analysis_store <- analysis_objectstore_factory(paste("analysis_store_",hrname,sep=""))
  analysis_store <- updateAnalysisStore(analysis_store,analysis,data.frame(key_hash=key_hash,analysis_module=class(analysis)[[1]]),force=TRUE)
  commitAnalysisStore(analysis_store)    
}



