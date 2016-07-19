sourceTo("../reporting/report_module_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

key_func <- function(){three_monthly_lookback(11)}

earnings <- new("EarningsReportModule")
earnings@build_if_none <- TRUE
earnings@no_plots <- FALSE

adjustments <- new("SizeAdjustmentReportModule")
adjustments@build_if_none <- TRUE
adjustments@no_plots <- FALSE

earnings <- buildReportModule(earnings,key_func)
adjustments <- buildReportModule(adjustments,key_func)

earnings <- runReportModule(earnings)
adjustments <- runReportModule(adjustments)





