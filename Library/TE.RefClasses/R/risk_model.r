####################################
#
# VirtualRiskModel Class
#
####################################

#' Virtual S4 class for handling Risk Model Properties.
#'
#' Inherits from "VirtualReferenceObject"
#'
#' @slot model_prefix    "character"
#' @slot lookback        "integer"
#' @slot model_universe  "character"
#' @slot model_factors   "character"

setClass(
  Class     = "VirtualRiskModel",
  slots     = c(
    model_prefix    = "character",
    lookback        = "integer",
    model_universe  = "character",
    model_factors   = "character"
  ),
  contains  = c("VirtualReferenceObject", "VIRTUAL")
)


#' Get Risk Model Name
#'
#' Returns name of Risk Model
#'
#' @param object object of class 'VirtualRiskModel'.
#' @return \code{model_name} 'character', name of the model.
#' @export

setGeneric("getRiskModelName", function(object){standardGeneric("getRiskModelName")})

#' @describeIn getRiskModelName
#' Get Risk Model Name
#'
#' Returns name of Risk Model
#'
#' @inheritParams getRiskModelName
#' @return \code{model_name} 'character', name of the model.
#' @export
setMethod("getRiskModelName",
          signature(object = "VirtualRiskModel"),
          function(object){
            return(object@model_prefix)
          }
)


#' Get Risk Model Prefix
#'
#' Returns prefix of Risk Model used to construct model store querries
#'
#' @param object object of class 'VirtualRiskModel'.
#' @return \code{model_prefix} 'character', model prefix.
#' @export

setGeneric("getRiskModelPrefix", function(object){standardGeneric("getRiskModelPrefix")})

#' @describeIn getRiskModelPrefix
#' Get Risk Model Prefix
#'
#' Returns prefix of Risk Model used to construct model store querries
#'
#' @inheritParams getRiskModelPrefix
#' @return \code{model_prefix} 'character', model prefix.
#' @export
setMethod("getRiskModelPrefix",
          signature(object = "VirtualRiskModel"),
          function(object){
            return(object@model_prefix)
          }
)



#' Get Risk Model Lookback
#'
#' Returns model lookback value in days. Lookback is number of
#' calendar days over which model is calculated.
#'
#' @param object object of class 'VirtualRiskModel'.
#' @return \code{lookback} 'integer'
#' @export

setGeneric("getRiskModelLookback", function(object){standardGeneric("getRiskModelLookback")})

#' @describeIn getRiskModelLookback
#' Get Risk Model Lookback
#'
#' Returns model lookback value in days. Lookback is number of
#' calendar days over which model is calculated.
#'
#' @inheritParams getRiskModelLookback
#' @return \code{lookback} 'integer'
#' @export
setMethod("getRiskModelLookback",
          signature(object = "VirtualRiskModel"),
          function(object){
            return(object@lookback)
          }
)


#' Get Risk Model Universe
#'
#' Returns name of universe for which model is computed.
#'
#' @param object object of class 'VirtualRiskModel'.
#' @return \code{model_universe} 'character', name of the universe covered by the model
#' @export

setGeneric("getRiskModelUniverse", function(object){standardGeneric("getRiskModelUniverse")})

#' @describeIn getRiskModelUniverse
#' Get Risk Model Universe
#'
#' Returns name of universe for which model is computed.
#'
#' @inheritParams getRiskModelUniverse
#' @return \code{model_universe} 'character', name of the universe covered by the model
#' @export
setMethod("getRiskModelUniverse",
          signature(object = "VirtualRiskModel"),
          function(object){
            return(object@model_universe)
          }
)

#' Get Risk Model Factor Names
#'
#' Returns names of factors included in the model
#'
#' @param object object of class 'VirtualRiskModel'.
#' @return \code{model_factors} 'character', vector of factor names
#' @export

setGeneric("getRiskModelFactorNames", function(object){standardGeneric("getRiskModelFactorNames")})

#' @describeIn getRiskModelFactorNames
#' Get Risk Model Factor Names
#'
#' Returns names of factors included in the model
#'
#' @inheritParams getRiskModelFactorNames
#' @return \code{model_factors} 'character', vector of factor names
#' @export
setMethod("getRiskModelFactorNames",
          signature(object = "VirtualRiskModel"),
          function(object){
            return(object@model_factors)
          }
)

####################################
#
# RiskModel.DevelopedEurope Class
#
####################################

#' List of risk model market factors
risk_model_market_factors <- c('Earnings','Growth','PriceMomentum12M','PriceMomentum1M','Size','StreetSentiment','Strength','TrendExtension','Value','Volatility')

#' List of risk model currency factors
risk_model_currency_factors <- c('JPY','GBP','EUR','CNY','RUB','ZAR','HKD','AUD','DKK','NOK','SEK','CHF','ILS','PLN','HUF','TRY')

#' List of risk model commodity factors
risk_model_commodity_factors <- c('WTI')

#' List of risk model sector factors
risk_model_sector_factors <- c('SX3P','SX4P','SX6P','SX7P','SX86P','SX8P','SXAP','SXDP','SXEP','SXFP','SXKP','SXMP','SXNP','SXOP','SXPP','SXQP','SXRP','SXTP')

devtools::use_data(risk_model_market_factors,
                   risk_model_currency_factors,
                   risk_model_commodity_factors,
                   risk_model_sector_factors,
                   overwrite = TRUE)

#' Virtual S4 class for Developed Europe Risk Models.
#'
#' Class implementing parameters for family of Developed
#' Europe Risk Models
#'
#' Inherits from "VirtualRiskModel"

setClass(
  Class     = "RiskModel.DevelopedEurope",
  prototype = list(
    model_factors = c(risk_model_market_factors,
                      risk_model_currency_factors,
                      risk_model_commodity_factors,
                      risk_model_sector_factors),
    model_universe = "developed_europe"
  ),
  contains  = c("VirtualRiskModel", "VIRTUAL")
)


####################################
#
# RiskModel.DevelopedEuropePrototype150 Class
#
####################################

#' Concrete S4 class for Developed Europe Risk Model.
#'
#' Class implementing parameters for Prototype Developed
#' Europe Risk Model with 150 days lookback
#'
#' Inherits from "RiskModel.DevelopedEurope"
#' @export

setClass(
  Class     = "RiskModel.DevelopedEuropePrototype150",
  prototype = list(
    model_universe  = "developed_europe",
    lookback        = 150L,
    model_prefix    = "developed_europe_prototype"
  ),
  contains  = c("RiskModel.DevelopedEurope")
)
