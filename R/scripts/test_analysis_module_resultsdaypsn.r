sourceTo("../analysis_modules/analysis_module_eventdaypsn.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

key_func <- function(){three_monthly_lookback(70)}
results_daypsn_analysis <- createAnalysisModule(results_daypsn_analysis_module_builder,key_func)

results_daypsn_analysis <- updateAnalysisModel(results_daypsn_analysis)

#results_daypsn_analysis <- recomputeModelFeatures(results_daypsn_analysis,features=c('Hit1D'),push_change=TRUE)
#results_daypsn_analysis <- resetPanels(results_daypsn_analysis,psn_visualisations)
results_daypsn_analysis <- runAnalysisModule(results_daypsn_analysis)
