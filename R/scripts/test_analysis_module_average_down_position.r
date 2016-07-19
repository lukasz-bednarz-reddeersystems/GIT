sourceTo("../analysis_modules/analysis_module_average_down_position.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

trader <- 11
key_func <- function(){dated_three_monthly_lookback(trader,"2016-01-01")}
average_down_analysis <- createAnalysisModule(average_down_analysis_module_builder,key_func)
average_down_analysis <- updateAnalysisModel(average_down_analysis)
average_down_analysis <- runAnalysisModule(average_down_analysis)

