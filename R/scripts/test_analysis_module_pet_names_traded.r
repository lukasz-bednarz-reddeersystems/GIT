sourceTo("../analysis_modules/analysis_module_position_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

#debug(setData)

key_func <- function(){three_monthly_lookback(11)}
holding_period_analysis <- createAnalysisModule(holding_period_analysis_module_builder,key_func)

holding_period_analysis <- updateAnalysisModel(holding_period_analysis)
holding_period_analysis <- runAnalysisModule(holding_period_analysis)

