#' TE.AnalysisClasses: Trading Enhancement Data access, cashing and preprocessing
#'
#' The TE.AnalysisClasses package providing classes for embedding of computation of
#' various analysis data and visualisation via ggplot
#'
#' @docType package
#' @name TE.AnalysisClasses
#'
#' @import TE.RefClasses
#' @import TE.RiskModel
#' @import methods
#' @import quantmod
#' @import lubridate
#' @import ggplot2
#' @importFrom dplyr arrange_
#' @importFrom hash hash values names.hash keys
#' @importFrom DMwR lofactor
#' @importFrom graphics barplot frame text
#' @importFrom stats aggregate cor.test fitted loess median cor reorder start na.omit var cov
#' @importFrom utils head tail stack unstack
#' @importFrom plotly plot_ly add_trace layout subplot
#' @importFrom RColorBrewer brewer.pal brewer.pal.info
#' @importClassesFrom hash hash
NULL
## NULL

#' Request data from data sources
#' @docType methods
#' @name dataRequest
#' @rdname dataRequest
#' @aliases dataRequest,VirtualAnalysisBlock,data.frame-method
#' @param object object of class derived from"VirtualAnalysisBlock".
#' @param key_values data.frame with keys specifying data query.
#' @return \code{object} object of class derived from 'VirtualAnalysisBlock'.
NULL
#> NULL



#' Set risk_model object in object slot
#'
#' Public method to set risk_model slot with "VirtualRiskModel"
#' class object
#'
#' @docType methods
#' @name setRiskModelObject
#' @rdname setRiskModelObject
#'
#' @aliases setRiskModelObject,VirtualAnalysisBlock,VirtualRiskModel-method
#' @param object object of class derived from "VirtualAnalysisBlock"
#' @param risk_model object of class derived from "VirtualRiskModel"
#' @return \code{object} object of class derived from "VirtualAnalysisBlock"
#' @export
NULL
#> NULL



#' Set portfolio object in object slot
#'
#' Public method to set portfolio slot with "StrategyPortfolio"
#' class object
#'
#' @docType methods
#' @name setPortfolioDataObject
#' @rdname setPortfolioDataObject
#'
#' @aliases setPortfolioDataObject,VirtualAnalysisBlock,VirtualPortfolio-method
#' @param object object of class derived from"VirtualAnalysisBlock"
#' @param portfolio object of class "StrategyPortfolio" or "VirtualIndexPortfolio"
#' @return \code{object} object of class derived from "VirtualAnalysisBlock"
#' @export
NULL
#> NULL


#' Set trade_data object in object slot
#'
#' Public method to set trade_data slot with object of class derirved from "VirtualTradeData"
#'
#' @docType methods
#' @name setTradeDataObject
#' @rdname setTradeDataObject
#'
#' @aliases setTradeDataObject,VirtualAnalysisBlock,VirtualTradeData-method
#' @param object object of class derived from"VirtualAnalysisBlock"
#' @param trade_data object of class derirved from "VirtualTradeData"
#' @return \code{object} object of class derived from "VirtualAnalysisBlock"
#' @export
NULL
#> NULL

#' Set ex_trade_data object in object slot
#'
#' Public method to set ex_trade_data slot with "ExtendedTradeData"
#' class or derived class object
#'
#' @docType methods
#' @name setExtendedTradeDataObject
#' @rdname setExtendedTradeDataObject
#'
#' @aliases setExtendedTradeDataObject,VirtualAnalysisBlock,ExtendedTradeData-method
#' @param object object of class derived from"VirtualAnalysisBlock"
#' @param ex_trade_data object of class "ExtendedTradeData"
#' @return \code{object} object of class derived from "VirtualAnalysisBlock"
#' @export
NULL
#> NULL


#' Set price_data object in object slot
#'
#' Public method to set price_data slot with "PriceData"
#' class object
#'
#' @docType methods
#' @name setPriceDataObject
#' @rdname setPriceDataObject
#'
#' @aliases setPriceDataObject,VirtualAnalysisBlock,PriceData-method
#' @param object object of class derived from"VirtualAnalysisBlock"
#' @param price_data object of class "PriceData"
#' @return \code{object} object of class derived from "VirtualAnalysisBlock"
#' @export
NULL
#> NULL


#' Set strategy_data object in object slot
#'
#' Public method to set strategy_data slot with "VirtualStrategyData"
#' class or derived object
#'
#' @docType methods
#' @name setStrategyDataObject
#' @rdname setStrategyDataObject
#'
#' @aliases setStrategyDataObject,VirtualAnalysisBlock,VirtualStrategyData-method
#' @param object object of class derived from"VirtualAnalysisBlock"
#' @param strategy_data object of class derived from "VirtualStrategyData"
#' @return \code{object} object of class derived from "VirtualAnalysisBlock"
#' @export
NULL
#> NULL



#' Set position_data object in object slot
#'
#' Public method to set position_data slot with
#' object of class derived from "VirtualPositionData"
#'
#' @docType methods
#' @name setPositionDataObject
#' @rdname setPositionDataObject
#'
#' @aliases setPositionDataObject,VirtualAnalysisBlock,VirtualPositionData-method
#' @param object object of class derived from"VirtualAnalysisBlock"
#' @param position_data object of class "VirtualPositionData"
#' @return \code{object} object of class derived from "VirtualAnalysisBlock"
#' @export
NULL
#> NULL



#' Set event_data object in object slot
#'
#' Public method to set event_data slot with object of class
#' derived from "EventData"
#'
#' @docType methods
#' @name setEventDataObject
#' @rdname setEventDataObject
#'
#' @aliases setEventDataObject,VirtualAnalysisBlock,EventData-method
#' @param object object of class derived from"VirtualAnalysisBlock"
#' @param event_data object of class derived from "EventData"
#' @return \code{object} object of class derived from "VirtualAnalysisBlock"
#' @export
NULL
#> NULL




#' Set instrument_betas object in object slot
#'
#' Public method to set instrument_betas slot with "InstrumentBetasData"
#' class object
#'
#' @docType methods
#' @name setInstrumentBetasDataObject
#' @rdname setInstrumentBetasDataObject
#'
#' @aliases setInstrumentBetasDataObject,VirtualAnalysisBlock,InstrumentBetasData-method
#' @param object object of class derived from"VirtualAnalysisBlock"
#' @param instrument_betas object of class "InstrumentBetasData"
#' @return \code{object} object of class derived from "VirtualAnalysisBlock"
#' @export
NULL
#> NULL



#' Set factor_correlation object in object slot
#'
#' Public method to set factor_correlation slot with "FactorCorrelationData"
#' class object
#'
#' @docType methods
#' @name setFactorCorrelationDataObject
#' @rdname setFactorCorrelationDataObject
#'
#' @aliases setFactorCorrelationDataObject,VirtualAnalysisBlock,FactorCorrelationData-method
#' @param object object of class derived from"VirtualAnalysisBlock"
#' @param factor_correlation object of class "FactorCorrelationData"
#' @return \code{object} object of class derived from "VirtualAnalysisBlock"
#' @export
NULL
#> NULL



#' Set factor_variance object in object slot
#'
#' Public method to set factor_variance slot with "FactorVarianceData"
#' class object
#'
#' @docType methods
#' @name setFactorVarianceDataObject
#' @rdname setFactorVarianceDataObject
#'
#' @aliases setFactorVarianceDataObject,VirtualAnalysisBlock,FactorVarianceData-method
#' @param object object of class derived from"VirtualAnalysisBlock"
#' @param factor_variance object of class "FactorVarianceData"
#' @return \code{object} object of class derived from "VirtualAnalysisBlock"
#' @export
NULL
#> NULL



