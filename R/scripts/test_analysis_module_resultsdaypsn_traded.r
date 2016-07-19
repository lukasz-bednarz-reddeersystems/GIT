sourceTo("../common/analysis_objectstore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_module_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

trader  <- 11
to_date <- Sys.Date()
run_module <- results_daypsntraded_analysis_module_builder
force <- FALSE

key_func <- function(){dated_three_monthly_lookback(trader,as.character(to_date))}
key_hash <- as.character(murmur3.32(as.character(key_func())))
kv <- key_func()
hrname <- paste(kv[[1,1]],"_",as.character(min(kv[['start']])),"_",as.character(max(kv[['end']])),sep="")

analysis <- createAnalysisModule(run_module,key_func)
analysis <- updateAnalysisModel(analysis)

analysis <- togglePlotNone(analysis)
analysis <- runAnalysisModule(analysis)

