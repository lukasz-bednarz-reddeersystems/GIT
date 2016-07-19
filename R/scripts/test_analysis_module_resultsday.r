sourceTo("../analysis_modules/analysis_module_resultsday.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

key_func <- function(){three_monthly_lookback(11)}
results_day_analysis <- createAnalysisModule(results_day_analysis_module_builder,key_func)

results_day_analysis <- updateAnalysisModel(results_day_analysis)

#results_day_analysis <- recomputeModelFeatures(results_day_analysis,features=c('NewPosition'),push_change=TRUE)
#results_day_analysis <- resetPanels(results_day_analysis,rd_visualisations)
results_day_analysis <- runAnalysisModule(results_day_analysis)
