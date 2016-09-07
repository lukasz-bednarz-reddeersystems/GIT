####################################
#
# VirtualBetaEstimator Class
#
####################################

#' Virtual S4 class for handling Beta Estimator Properties.
#'
#'
#' @slot est_method_name   "character"
#'
#' @export
setClass(
  Class     = "VirtualBetaEstimator",
  slots     = c(
    est_method_name      = "character"
  ),
  contains  = c("VIRTUAL")
)

#' Get Estimator method Name
#'
#' Returns name of Betas estimator
#'
#' @param object object of class 'VirtualRiskModel'.
#' @export

setGeneric("getRiskModelBetaEstimatorType", function(object){standardGeneric("getRiskModelBetaEstimatorType")})

#' @describeIn getRiskModellBetaEstimator
#' Get Risk Model Name
#'
#' Returns name of Risk Model
#'
#' @inheritParams getRiskModelBetaEstimator
#' @return \code{beta_model} 'VirtualBetaEstimator', object capturing beta model properties
#' @export
setMethod("getRiskModelBetaEstimatorType",
          signature(object = "VirtualBetaEstimator"),
          function(object){
            return(object@beta_model)
          }
)


#' Concrete S4 class for handling OLS computation of betas.
#'
#' Ordinary Least Squares Beta estimator
#'
#' @export
setClass(
  Class     = "BetaEstimator.OLS",
  prototype     = list(
    est_method_name      = "OLS"
  ),
  contains  = c("VirtualBetaEstimator")
)

#' Concrete S4 class for handling RLM computation of betas.
#'
#' Robust Linear Model Beta estimator
#'
#' @export
setClass(
  Class     = "BetaEstimator.RLM",
  prototype     = list(
    est_method_name      = "RLM"
  ),
  contains  = c("VirtualBetaEstimator")
)


####################################
#
# VirtualRiskModel Class
#
####################################

#' Virtual S4 class for handling Risk Model Properties.
#'
#'
#' @slot model_prefix    "character"
#' @slot lookback        "integer"
#' @slot model_universe  "character"
#' @slot model_factors   "character"
#' @slot beta_estimator  "VirtualBetaEstimator"
#'
#' @export

setClass(
  Class     = "VirtualRiskModel",
  slots     = c(
    model_prefix    = "character",
    lookback        = "integer",
    model_universe  = "character",
    model_factors   = "character",
    beta_estimator  = "VirtualBetaEstimator"
  ),
  prototype = list(
    beta_model      = new("BetaEstimator.OLS")
  ),
  contains  = c("VIRTUAL")
)


#' Get Risk Model Name
#'
#' Returns name of Risk Model
#'
#' @param object object of class 'VirtualRiskModel'.
#' @export

setGeneric("getRiskModellBetaEstimator", function(object){standardGeneric("getRiskModellBetaEstimator")})

#' @describeIn getRiskModellBetaEstimator
#' Get Risk Model Name
#'
#' Returns name of Risk Model
#'
#' @inheritParams getRiskModellBetaEstimator
#' @return \code{beta_model} 'VirtualBetaEstimator', object capturing beta model properties
#' @export
setMethod("getRiskModellBetaEstimator",
          signature(object = "VirtualRiskModel"),
          function(object){
            return(object@beta_model)
          }
)



#' Get Risk Model Name
#'
#' Returns name of Risk Model
#'
#' @param object object of class 'VirtualRiskModel'.
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

#' Get Risk Model Sector Factor Names
#'
#' Returns names of factors included in the model
#'
#' @param object object of class 'VirtualRiskModel'.
#' @export

setGeneric("getRiskModelSectorFactorNames", function(object){standardGeneric("getRiskModelSectorFactorNames")})

#' @describeIn getRiskModelSectorFactorNames
#' Get Risk Model Factor Names
#'
#' Returns names of sector factors included in the model
#'
#' @inheritParams getRiskModelSectorFactorNames
#' @return \code{model_factors} 'character', vector of factor names
#' @export
setMethod("getRiskModelSectorFactorNames",
          signature(object = "VirtualRiskModel"),
          function(object){

            factor_info <- get_factors()
            factors <- getRiskModelFactorNames(object)

            sector_factors <- factor_info$sFactorName[factor_info$sFactorTypeName == "Sector"]

            model_factors <- intersect(sector_factors, factors)

            return(model_factors)
          }
)


#' Get Risk Model Currency Factor Names
#'
#' Returns names of factors included in the model
#'
#' @param object object of class 'VirtualRiskModel'.
#' @export

setGeneric("getRiskModelCurrencyFactorNames", function(object){standardGeneric("getRiskModelCurrencyFactorNames")})

#' @describeIn getRiskModelCurrencyFactorNames
#' Get Risk Model Currency Factor Names
#'
#' Returns names of sector factors included in the model
#'
#' @inheritParams getRiskModelCurrencyFactorNames
#' @return \code{model_factors} 'character', vector of factor names
#' @export
setMethod("getRiskModelCurrencyFactorNames",
          signature(object = "VirtualRiskModel"),
          function(object){

            factor_info <- get_factors()
            factors <- getRiskModelFactorNames(object)

            sector_factors <- factor_info$sFactorName[factor_info$sFactorTypeName == "Currency"]

            model_factors <- intersect(sector_factors, factors)

            return(model_factors)
          }
)



#' Get Risk Model Commodity Factor Names
#'
#' Returns names of factors included in the model
#'
#' @param object object of class 'VirtualRiskModel'.
#' @export

setGeneric("getRiskModelCommodityFactorNames", function(object){standardGeneric("getRiskModelCommodityFactorNames")})

#' @describeIn getRiskModelCommodityFactorNames
#' Get Risk Model Commodity Factor Names
#'
#' Returns names of sector factors included in the model
#'
#' @inheritParams getRiskModelCommodityFactorNames
#' @return \code{model_factors} 'character', vector of factor names
#' @export
setMethod("getRiskModelCommodityFactorNames",
          signature(object = "VirtualRiskModel"),
          function(object){

            factor_info <- get_factors()
            factors <- getRiskModelFactorNames(object)

            sector_factors <- factor_info$sFactorName[factor_info$sFactorTypeName == "Oil"]

            model_factors <- intersect(sector_factors, factors)

            return(model_factors)
          }
)

#' Get Risk Model Market Factor Names
#'
#' Returns names of factors included in the model
#'
#' @param object object of class 'VirtualRiskModel'.
#' @export

setGeneric("getRiskModelMarketFactorNames", function(object){standardGeneric("getRiskModelMarketFactorNames")})

#' @describeIn getRiskModelMarketFactorNames
#' Get Risk Model Market Factor Names
#'
#' Returns names of sector factors included in the model
#'
#' @inheritParams getRiskModelMarketFactorNames
#' @return \code{model_factors} 'character', vector of factor names
#' @export
setMethod("getRiskModelMarketFactorNames",
          signature(object = "VirtualRiskModel"),
          function(object){

            factor_info <- get_factors()
            factors <- getRiskModelFactorNames(object)

            sector_factors <- factor_info$sFactorName[factor_info$sFactorTypeName == "Market"]

            model_factors <- intersect(sector_factors, factors)

            return(model_factors)
          }
)


#' Get Risk Model Commodity Factor returns
#'
#' Returns data.frame with daily returns of factors included in the model
#'
#' @param object object of class 'VirtualRiskModel'.
#' @param date_start "Date" start date of requested factor returns
#' @param date_end "Date" end date of requested factor returns
#' @export

setGeneric("getRiskModelCommodityFactorReturns", function(object,
                                                          date_start,
                                                          date_end
                                                          ){standardGeneric("getRiskModelCommodityFactorReturns")})

#' @describeIn getRiskModelCommodityFactorReturns
#' Get Risk Model Commodity Factor returns
#'
#' Returns data.frame with daily returns of factors included in the model
#'
#' @inheritParams getRiskModelCommodityFactorReturns
#' @return \code{fct_returns} 'data.frame' with "Date" "FactorName" and "Return" columns
#' @export
setMethod("getRiskModelCommodityFactorReturns",
          signature(object = "VirtualRiskModel",
                    date_start = "Date",
                    date_end   = "Date"),
          function(object, date_start, date_end){

            factors <- getRiskModelCommodityFactorNames(object)

            fct_returns <- get_commodity_returns(date_start,date_end)

            fct_returns <- fct_returns[c("Date", factors)]

            return(fct_returns)
          }
)


#' Get Risk Model Currency Factor returns
#'
#' Returns data.frame with daily returns of factors included in the model
#'
#' @param object object of class 'VirtualRiskModel'.
#' @param date_start "Date" start date of requested factor returns
#' @param date_end "Date" end date of requested factor returns
#' @export

setGeneric("getRiskModelCurrencyFactorReturns", function(object,
                                                          date_start,
                                                          date_end
){standardGeneric("getRiskModelCurrencyFactorReturns")})

#' @describeIn getRiskModelCurrencyFactorReturns
#' Get Risk Model Currency Factor returns
#'
#' Returns data.frame with daily returns of factors included in the model
#'
#' @inheritParams getRiskModelCurrencyFactorReturns
#' @return \code{fct_returns} 'data.frame' with "Date" "FactorName" and "Return" columns
#' @export
setMethod("getRiskModelCurrencyFactorReturns",
          signature(object = "VirtualRiskModel",
                    date_start = "Date",
                    date_end   = "Date"),
          function(object, date_start, date_end){

            factors <- getRiskModelCurrencyFactorNames(object)

            fct_returns <- get_FX_returns(date_start,date_end)

            fct_returns <- fct_returns[c("Date", factors)]

            return(fct_returns)
          }
)


#' Get Risk Model Market Factor returns
#'
#' Returns data.frame with daily returns of factors included in the model
#'
#' @param object object of class 'VirtualRiskModel'.
#' @param date_start "Date" start date of requested factor returns
#' @param date_end "Date" end date of requested factor returns
#' @export

setGeneric("getRiskModelMarketFactorReturns", function(object,
                                                          date_start,
                                                          date_end
){standardGeneric("getRiskModelMarketFactorReturns")})

#' @describeIn getRiskModelMarketFactorReturns
#' Get Risk Model Market Factor returns
#'
#' Returns data.frame with daily returns of factors included in the model
#'
#' @inheritParams getRiskModelMarketFactorReturns
#' @return \code{fct_returns} 'data.frame' with "Date" "FactorName" and "Return" columns
#' @export
setMethod("getRiskModelMarketFactorReturns",
          signature(object = "VirtualRiskModel",
                    date_start = "Date",
                    date_end   = "Date"),
          function(object, date_start, date_end){

            factors <- getRiskModelMarketFactorNames(object)

            fct_returns <- get_risk_factor_returns(date_start,date_end)

            fct_returns <- pivot_frame(fct_returns,'FactorName','Return','Date')

            fct_returns <- fct_returns[c("Date", factors)]

            return(fct_returns)
          }
)

#' Get Risk Model Sector Factor returns
#'
#' Returns data.frame with daily returns of factors included in the model
#'
#' @param object object of class 'VirtualRiskModel'.
#' @param date_start "Date" start date of requested factor returns
#' @param date_end "Date" end date of requested factor returns
#' @export

setGeneric("getRiskModelSectorFactorReturns", function(object,
                                                       date_start,
                                                       date_end
){standardGeneric("getRiskModelSectorFactorReturns")})

#' @describeIn getRiskModelSectorFactorReturns
#' Get Risk Model Sector Factor returns
#'
#' Returns data.frame with daily returns of factors included in the model
#'
#' @inheritParams getRiskModelSectorFactorReturns
#' @return \code{fct_returns} 'data.frame' with "Date" "FactorName" and "Return" columns
#' @export
setMethod("getRiskModelSectorFactorReturns",
          signature(object = "VirtualRiskModel",
                    date_start = "Date",
                    date_end   = "Date"),
          function(object, date_start, date_end){

            factors <- getRiskModelSectorFactorNames(object)

            fct_returns <- unique(get_sector_returns(date_start,date_end))

            fct_returns <- pivot_frame(fct_returns,'FactorName','Return','Date')

            fct_returns <- fct_returns[c("Date", factors)]

            return(fct_returns)
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


#' Virtual S4 class for Developed Europe Risk Models.
#'
#' Class implementing parameters for family of Developed
#' Europe Risk Models version 1.1 excluding HKD and DKK
#'
#' Inherits from "VirtualRiskModel"

setClass(
  Class     = "RiskModel.DevelopedEurope.1.1",
  prototype = list(
    model_factors = c(risk_model_market_factors,
                      setdiff(risk_model_currency_factors, c("HKD", "DKK")),
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
    lookback        = 150L,
    model_prefix    = "developed_europe_prototype"
  ),
  contains  = c("RiskModel.DevelopedEurope")
)

#' Concrete S4 class for Developed Europe Risk Model.
#'
#' Class implementing parameters for Prototype Developed
#' Europe Risk Model with 150 days lookback
#'
#' Inherits from "RiskModel.DevelopedEurope.1.1"
#' @export

setClass(
  Class     = "RiskModel.DevelopedEuropePrototype150.1.1",
  prototype = list(
    lookback        = 150L,
    model_prefix    = "developed_europe_prototype.1.1"
  ),
  contains  = c("RiskModel.DevelopedEurope.1.1")
)
