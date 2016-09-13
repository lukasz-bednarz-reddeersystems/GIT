#' @include analysis_client.r
NULL

#Convention is to name the client classes with the Client suffix so that the engine
#can specify analysis modules to load by class name.

#' Clas wrapping access to StrategyBreakdownAnalysisBlock
#'
#' Implements methods to access analysis objectstore
#' for StrategyBreakdownAnalysisBlock class
#'
#' Inherits from: "VirtualAnalysisObjectstoreClient"
#'
#' @export

setClass(
  Class                = "StrategyBreakdownAnalysisBlockClient",
  prototype = prototype(
    analysis_class = "StrategyBreakdownAnalysisBlock"
  ),
  contains = c("VirtualAnalysisObjectstoreClient")
)


#' Clas wrapping access to IndexPortfolioFactorExposuresAnalysisBlock
#'
#' Implements methods to access analysis objectstore
#' for IndexPortfolioFactorExposuresAnalysisBlock class
#'
#' Inherits from: "VirtualAnalysisObjectstoreClient"
#'
#' @export

setClass(
  Class                = "IndexPortfolioFactorExposuresAnalysisBlockClient",
  prototype = prototype(
    analysis_class = "IndexPortfolioFactorExposuresAnalysisBlock"
  ),
  contains = c("VirtualAnalysisObjectstoreClient")
)


#' Clas wrapping access to IndexPortfolioFactorReturnsAnalysisBlock
#'
#' Implements methods to access analysis objectstore
#' for IndexPortfolioFactorReturnsAnalysisBlock class
#'
#' Inherits from: "VirtualAnalysisObjectstoreClient"
#'
#' @export

setClass(
  Class                = "IndexPortfolioFactorReturnsAnalysisBlockClient",
  prototype = prototype(
    analysis_class = "IndexPortfolioFactorReturnsAnalysisBlock"
  ),
  contains = c("VirtualAnalysisObjectstoreClient")
)


#' Clas wrapping access to IndexPortfolioVarianceDecompositionAnalysisBlock
#'
#' Implements methods to access analysis objectstore
#' for IndexPortfolioVarianceDecompositionAnalysisBlock class
#'
#' Inherits from: "VirtualAnalysisObjectstoreClient"
#'
#' @export

setClass(
  Class                = "IndexPortfolioVarianceDecompositionAnalysisBlockClient",
  prototype = prototype(
    analysis_class = "IndexPortfolioVarianceDecompositionAnalysisBlock"
  ),
  contains = c("VirtualAnalysisObjectstoreClient")
)


#' Clas wrapping access to MarketStyleFactorStatisticAnalysisBlock
#'
#' Implements methods to access analysis objectstore
#' for MarketStyleFactorStatisticAnalysisBlock class
#'
#' Inherits from: "VirtualAnalysisObjectstoreClient"
#'
#' @export

setClass(
  Class                = "MarketStyleFactorStatisticAnalysisBlockClient",
  prototype = prototype(
    analysis_class = "MarketStyleFactorStatisticAnalysisBlock"
  ),
  contains = c("VirtualAnalysisObjectstoreClient")
)

