sourceTo("../analysis_modules/analysis_module_snapshot_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

#debug(setData)

key_func <- function(){three_monthly_lookback(11)}
snapshot_analysis <- createAnalysisModule(price_snapshot_analysis_module_builder,key_func)
snapshot_analysis <- updateAnalysisModel(snapshot_analysis)
snapshot_analysis <- runAnalysisModule(snapshot_analysis)

