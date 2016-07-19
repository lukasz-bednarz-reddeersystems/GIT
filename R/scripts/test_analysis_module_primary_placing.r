sourceTo("../analysis_modules/analysis_module_eventdaypsn.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

key_func <- function(){three_monthly_lookback(70)}
primary_placing_analysis <- createAnalysisModule(primary_placing_psn_analysis_module_builder,key_func)

primary_placing_analysis <- updateAnalysisModel(primary_placing_analysis)
primary_placing_analysis <- runAnalysisModule(primary_placing_analysis)
