sourceTo("../analysis_modules/analysis_module_profit_targets.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

key_func <- function(){three_monthly_lookback(11)}
profit_targets_analysis <- createAnalysisModule(profit_target_analysis_module_builder,key_func)
profit_targets_analysis <- updateAnalysisModel(profit_targets_analysis)
profit_targets_analysis <- runAnalysisModule(profit_targets_analysis)