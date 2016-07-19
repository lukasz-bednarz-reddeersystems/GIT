sourceTo("../analysis_modules/analysis_module_sizing.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

key_func <- function(){three_monthly_lookback(101)}
sizing_analysis <- createAnalysisModule(sizing_analysis_module_builder,key_func)

sizing_analysis <- updateAnalysisModel(sizing_analysis)
sizing_analysis <- resetPanels(sizing_analysis,psnsz_visualisations)
sizing_analysis <- togglePlotNone(sizing_analysis)

sizing_analysis <- runAnalysisModule(sizing_analysis)

