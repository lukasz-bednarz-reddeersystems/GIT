#' @include analysis_client.r
NULL

#Convention is to name the client classes with the Client suffix so that the engine
#can specify analysis modules to load by class name.


#############################################################
#
# Strategy Portfolio / Trade Data classes
#
#############################################################

#' Strategy Portfolio classes
#'
#' Implements methods to access analysis objectstore
#' for StrategyPortfolio Risk Analysis
#'
#' Inherits from: "VirtualAnalysisObjectstoreClient"

setClass(
  Class                = "VirtualStrategyPortfolioAnalysisBlockClient",
  prototype = prototype(
    key_cols = c("analysis_class", "TraderID", "start", "end")
  ),
  contains = c("VirtualAnalysisObjectstoreClient")
)

#' Clas wrapping access to StrategyBreakdownAnalysisBlock
#'
#' Implements methods to access analysis objectstore
#' for StrategyBreakdownAnalysisBlock class
#'
#' Inherits from: "VirtualStrategyPortfolioAnalysisBlockClient"
#'
#' @export

setClass(
  Class                = "StrategyBreakdownAnalysisBlockClient",
  prototype = prototype(
    analysis_class = "StrategyBreakdownAnalysisBlock"
  ),
  contains = c("VirtualStrategyPortfolioAnalysisBlockClient")
)


#' Clas wrapping access to AverageDownTradesAnalysisBlock
#'
#' Implements methods to access analysis objectstore
#' for AverageDownTradesAnalysisBlock class
#'
#' Inherits from: "VirtualStrategyPortfolioAnalysisBlockClient"
#'
#' @export

setClass(
  Class 			   = "AverageDownTradesAnalysisBlockClient",
  prototype = prototype(
    analysis_class = "AverageDownTradesAnalysisBlock"
  ),
  contains = c("VirtualStrategyPortfolioAnalysisBlockClient")
)



#' Clas wrapping access to AverageDownTradesFocusAnalysisBlock
#'
#' Implements methods to access analysis objectstore
#' for AverageDownTradesFocusAnalysisBlock class
#'
#' Inherits from: "VirtualStrategyPortfolioAnalysisBlockClient"
#'
#' @export
setClass(
  Class 			   = "AverageDownTradesFocusAnalysisBlockClient",
  prototype = prototype(
    analysis_class = "AverageDownTradesFocusAnalysisBlock"
  ),
  contains = c("VirtualStrategyPortfolioAnalysisBlockClient")
)

#' Clas wrapping access to BuysAndSellsAnalysisBlock
#'
#' Implements methods to access analysis objectstore
#' for BuysAndSellsAnalysisBlock class
#'
#' Inherits from: "VirtualStrategyPortfolioAnalysisBlockClient"
#'
#' @export
setClass(
  Class 			   = "BuysAndSellsAnalysisBlockClient",
  prototype = prototype(
    analysis_class = "BuysAndSellsAnalysisBlock"
  ),
  contains = c("VirtualStrategyPortfolioAnalysisBlockClient")
)

#' Clas wrapping access to ExtendedTradesAnalysisBlock
#'
#' Implements methods to access analysis objectstore
#' for ExtendedTradesAnalysisBlock class
#'
#' Inherits from: "VirtualStrategyPortfolioAnalysisBlockClient"
#'
#' @export
setClass(
  Class 			   = "ExtendedTradesAnalysisBlockClient",
  prototype = prototype(
    analysis_class = "ExtendedTradesAnalysisBlock"
  ),
  contains = c("VirtualStrategyPortfolioAnalysisBlockClient")
)

#' Clas wrapping access to MarketReturnAnalysisBlock
#'
#' Implements methods to access analysis objectstore
#' for MarketReturnAnalysisBlock class
#'
#' Inherits from: "VirtualStrategyPortfolioAnalysisBlockClient"
#'
#' @export
setClass(
  Class 			   = "MarketReturnAnalysisBlockClient",
  prototype = prototype(
    analysis_class = "MarketReturnAnalysisBlock"
  ),
  contains = c("VirtualStrategyPortfolioAnalysisBlockClient")
)

#' Clas wrapping access to OffsidePositionsAnalysisBlock
#'
#' Implements methods to access analysis objectstore
#' for OffsidePositionsAnalysisBlock class
#'
#' Inherits from: "VirtualStrategyPortfolioAnalysisBlockClient"
#'
#' @export
setClass(
  Class 			   = "OffsidePositionsAnalysisBlockClient",
  prototype = prototype(
    analysis_class = "OffsidePositionsAnalysisBlock"
  ),
  contains = c("VirtualStrategyPortfolioAnalysisBlockClient")
)

#' Clas wrapping access to OffsidePositionsCumulativePnLAnalysisBlock
#'
#' Implements methods to access analysis objectstore
#' for OffsidePositionsCumulativePnLAnalysisBlock class
#'
#' Inherits from: "VirtualStrategyPortfolioAnalysisBlockClient"
#'
#' @export
setClass(
  Class          = "OffsidePositionsCumulativePnLAnalysisBlockClient",
  prototype = prototype(
    analysis_class = "OffsidePositionsCumulativePnLAnalysisBlock"
  ),
  contains = c("VirtualStrategyPortfolioAnalysisBlockClient")
)

#' Clas wrapping access to PnLTradedInLongShortHedgeAnalysisBlock
#'
#' Implements methods to access analysis objectstore
#' for PnLTradedInLongShortHedgeAnalysisBlock class
#'
#' Inherits from: "VirtualStrategyPortfolioAnalysisBlockClient"
#'
#' @export
setClass(
  Class          = "PnLTradedInLongShortHedgeAnalysisBlockClient",
  prototype = prototype(
    analysis_class = "PnLTradedInLongShortHedgeAnalysisBlock"
  ),
  contains = c("VirtualStrategyPortfolioAnalysisBlockClient")
)

#' Clas wrapping access to PortfolioFactorExposureAnalysisBlock
#'
#' Implements methods to access analysis objectstore
#' for PortfolioFactorExposureAnalysisBlock class
#'
#' Inherits from: "VirtualStrategyPortfolioAnalysisBlockClient"
#'
#' @export
setClass(
  Class          = "PortfolioFactorExposureAnalysisBlockClient",
  prototype = prototype(
    analysis_class = "PortfolioFactorExposureAnalysisBlock"
  ),
  contains = c("VirtualStrategyPortfolioAnalysisBlockClient")
)

#' Clas wrapping access to PositionRevisitsAnalysisBlockClient
#'
#' Implements methods to access analysis objectstore
#' for PositionRevisitsAnalysisBlockClient class
#'
#' Inherits from: "VirtualStrategyPortfolioAnalysisBlockClient"
#'
#' @export
setClass(
  Class          = "PositionRevisitsAnalysisBlockClient",
  prototype = prototype(
    analysis_class = "PositionRevisitsAnalysisBlock"
  ),
  contains = c("VirtualStrategyPortfolioAnalysisBlockClient")
)



#############################################################
#
# Index Portfolio classes
#
#############################################################


#' Index Portfolio classes
#'
#' Implements methods to access analysis objectstore
#' for IndexPortfolio Risk Analysis
#'
#' Inherits from: "VirtualAnalysisObjectstoreClient"
setClass(
  Class                = "VirtualIndexPortfolioAnalysisBlockClient",
  prototype = prototype(
    key_cols = c("analysis_class", "IndexTicker", "start", "end")
  ),
  contains = c("VirtualAnalysisObjectstoreClient", "VIRTUAL")
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
  contains = c("VirtualIndexPortfolioAnalysisBlockClient")
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
  contains = c("VirtualIndexPortfolioAnalysisBlockClient")
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
  contains = c("VirtualIndexPortfolioAnalysisBlockClient")
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

