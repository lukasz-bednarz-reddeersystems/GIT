sourceTo("../common/analysis_objectstore/analysis_objectstore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/analysis_client/analysis_client.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/strategy_breakdown_aum_and_turnover/strategy_breakdown_aum_and_turnover.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

#Convention is to name the client classes with the Client suffix so that the engine
#can specify analysis modules to load by class name.

setClass(
  Class                = "StrategyBreakdownAnalysisBlockClient",
  prototype = prototype(
    analysis_class = "StrategyBreakdownAnalysisBlock"
  ),
  contains = c("VirtualAnalysisClient")
)