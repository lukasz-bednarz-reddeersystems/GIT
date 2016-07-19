sourceTo("../common/analysis_objectstore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_module_resultsdaypsn.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

key_func <- function(){three_monthly_lookback(11,'2016-01-01')}
key_hash <- as.character(murmur3.32(as.character(key_func())))
kv <- key_func()
hrname <- paste(kv[[1,1]],"_",as.character(min(kv[['start']])),"_",as.character(max(kv[['end']])),sep="")

results_daypsn_analysis <- createAnalysisModule(results_daypsn_analysis_module_builder,key_func)
results_daypsn_analysis <- updateAnalysisModel(results_daypsn_analysis)

#results_daypsn_analysis <- resetPanels(results_daypsn_analysis,psn_visualisations)
results_daypsn_analysis <- togglePlotNone(results_daypsn_analysis)
results_daypsn_analysis <- runAnalysisModule(results_daypsn_analysis)

analysis_store <- analysis_objectstore_factory(paste("analysis_store_",hrname,sep=""))
analysis_store <- updateAnalysisStore(analysis_store,results_daypsn_analysis,data.frame(key_hash=key_hash,analysis_module=class(results_daypsn_analysis)[[1]]))
commitAnalysisStore(analysis_store)

analysis <- queryAnalysisStore(analysis_store,data.frame(key_hash=key_hash,analysis_module=class(results_daypsn_analysis)[[1]]))

