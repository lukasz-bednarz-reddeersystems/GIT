setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules/analysis_module_snapshot_resultsday.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_module_eventdaypsn.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
#run_mods <- c(resultsday_snapshot_analysis_module_builder,resultsday_exposure_snapshot_analysis_module_builder,results_daypsn_analysis_module_builder)
run_mods <- c(results_daypsn_analysis_module_builder)

trader   <- 70
date     <- '2016-01-01'
key_func <- function(){dated_three_monthly_lookback(trader,date)}
for(m in run_mods){
  analysis <- createAnalysisModule(m,key_func)
  analysis <- updateAnalysisModel(analysis)
  analysis <- runAnalysisModule(analysis)
  
  key_hash <- as.character(murmur3.32(as.character(key_func())))
  kv <- key_func()
  hrname <- paste(kv[[1,1]],"_",as.character(min(kv[['start']])),"_",as.character(max(kv[['end']])),sep="")
  analysis_store <- analysis_objectstore_factory(paste("analysis_store_",hrname,sep=""))
  analysis_store <- updateAnalysisStore(analysis_store,analysis,data.frame(key_hash=key_hash,analysis_module=class(analysis)[[1]]),force=TRUE)
  commitAnalysisStore(analysis_store)  
}
