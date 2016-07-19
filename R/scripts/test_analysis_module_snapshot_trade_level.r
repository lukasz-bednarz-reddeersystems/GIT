sourceTo("../analysis_modules/analysis_module_snapshot_trade_level.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

key_func <- function(){three_monthly_lookback(101)}
snapshot_analysis <- createAnalysisModule(level_snapshot_analysis_module_builder,key_func)
snapshot_analysis <- updateAnalysisModel(snapshot_analysis)
snapshot_analysis <- runAnalysisModule(snapshot_analysis)

