sourceTo("../common/analysis_objectstore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_module_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

trader  <- 101
to_date <- Sys.Date()
run_module <- size_adjustment_ratio_analysis_module_builder
force <- FALSE

key_func <- function(){dated_three_monthly_lookback(trader,as.character(to_date))}
key_hash <- as.character(murmur3.32(as.character(key_func())))
kv <- key_func()
hrname <- paste(kv[[1,1]],"_",as.character(min(kv[['start']])),"_",as.character(max(kv[['end']])),sep="")

analysis <- createAnalysisModule(run_module,key_func)
analysis <- updateAnalysisModel(analysis)
analysis <- recomputeModelFeatures(analysis,c("TodayPL","NewPosition"),push_change=TRUE)

#results_daypsn_analysis <- resetPanels(results_daypsn_analysis,psn_visualisations)
analysis <- togglePlotNone(analysis)
analysis <- runAnalysisModule(analysis)

analysis_store <- analysis_objectstore_factory(paste("analysis_store_",hrname,sep=""))
analysis_store <- updateAnalysisStore(analysis_store,analysis,data.frame(key_hash=key_hash,analysis_module=class(analysis)[[1]]),force=force)
commitAnalysisStore(analysis_store)
