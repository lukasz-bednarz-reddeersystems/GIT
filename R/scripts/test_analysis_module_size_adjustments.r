sourceTo("../analysis_modules/analysis_module_size_adjustments.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

key_func <- function(){three_monthly_lookback(11)}
size_adjustment_analysis <- createAnalysisModule(size_adjustment_analysis_module_builder,key_func)

size_adjustment_analysis <- updateAnalysisModel(size_adjustment_analysis)

#results_day_analysis <- recomputeModelFeatures(results_day_analysis,features=c('NewPosition'),push_change=TRUE)
#results_day_analysis <- resetPanels(results_day_analysis,rd_visualisations)
size_adjustment_analysis <- runAnalysisModule(size_adjustment_analysis)
