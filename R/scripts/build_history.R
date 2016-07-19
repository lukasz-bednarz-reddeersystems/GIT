setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules/analysis_module_position_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

trader   <- 101
date     <- '2015-04-01'
key_func <- function(){dated_three_monthly_lookback(trader,date)}
analysis <- createAnalysisModule(history_analysis_module_builder,key_func)
analysis <- updateAnalysisModel(analysis)
analysis <- runAnalysisModule(analysis)

key_hash <- as.character(murmur3.32(as.character(key_func())))
kv <- key_func()
hrname <- paste(kv[[1,1]],"_",as.character(min(kv[['start']])),"_",as.character(max(kv[['end']])),sep="")
analysis_store <- analysis_objectstore_factory(paste("analysis_store_",hrname,sep=""))
analysis_store <- updateAnalysisStore(analysis_store,analysis,data.frame(key_hash=key_hash,analysis_module=class(analysis)[[1]]))
commitAnalysisStore(analysis_store)