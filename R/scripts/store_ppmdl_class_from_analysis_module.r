sourceTo("../analysis_modules_legacy/analysis_module_position_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

m <- history_analysis_module_builder
trader   <- 101
date     <- '2016-05-01'
key_func <- function(){dated_four_monthly_lookback(trader,date)}

key_hash <- as.character(murmur3.32(as.character(key_func())))
kv <- key_func()
hrname <- paste(kv[[1,1]],"_",as.character(min(kv[['start']])),"_",as.character(max(kv[['end']])),sep="")
analysis_store <- analysis_objectstore_factory(paste("analysis_store_",hrname,sep=""))
analysis <- queryAnalysisStore(analysis_store,data.frame(key_hash=key_hash,analysis_module="TradeHistoryModule"))
data <- analysis@ppmdl
analysis_store <- updateAnalysisStore(analysis_store,data,data.frame(key_hash=key_hash,analysis_module=class(data)[[1]]),force=TRUE)
commitAnalysisStore(analysis_store)  

