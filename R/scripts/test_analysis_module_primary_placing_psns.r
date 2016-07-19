sourceTo("../analysis_modules/analysis_module_tradedaround_eventdaypsn.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

#debug(psn_returns_around_placings)

key_func <- function(){three_monthly_lookback(70)}
primary_placingtrd_analysis <- createAnalysisModule(trd_around_prmryplacing_analysis_module_builder,key_func)

primary_placingtrd_analysis <- updateAnalysisModel(primary_placingtrd_analysis)
primary_placingtrd_analysis <- runAnalysisModule(primary_placingtrd_analysis)

