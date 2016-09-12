#' @include analysis_client.r


#Convention is to name the client classes with the Client suffix so that the engine
#can specify analysis modules to load by class name.

#' Clas wrapping access to StrategyBreakdownAnalysisBlock
#'
#' Implements methods to access analysis objectstore
#' for StrategyBreakdownAnalysisBlock class
#'
#' Inherits from: "VirtualAnalysisClient"
#'
#' @export

setClass(
  Class                = "StrategyBreakdownAnalysisBlockClient",
  prototype = prototype(
    analysis_class = "StrategyBreakdownAnalysisBlock"
  ),
  contains = c("VirtualAnalysisObjectstoreClient")
)
