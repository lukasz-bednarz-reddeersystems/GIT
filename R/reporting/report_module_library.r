sourceTo("../reporting/report_module.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_module_resultsday.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_module_resultsdaypsn.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_module_size_adjustments.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

setClass(
	Class = "EarningsReportModule",
	prototype = prototype(
		builders = list(results_day_analysis_module_builder,results_daypsn_analysis_module_builder)
	),
	contains = c('ReportModule')
)

setClass(
	Class = "SizeAdjustmentReportModule",
	prototype = prototype(
		builders = list(size_adjustment_analysis_module_builder)
	),
	contains = c('ReportModule')
)
