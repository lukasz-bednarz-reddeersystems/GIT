sourceTo("../lib/referencedata/referenceobject.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

####################################
#
# VirtualRiskModel Class
#
####################################

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

if (!isGenericS4("getRiskModelName")) {
  setGeneric("getRiskModelName", function(object,...){standardGeneric("getRiskModelName")})
}
# Returns model_name of class "VirtualRiskModel"
#
# Args:
#   object : object of type "VirtualRiskModel"
# Returns:
#   model_name : name of the model

setMethod("getRiskModelName",  
          signature(object = "VirtualRiskModel"),
          function(object){
            return(object@model_prefix)
          }
)

if (!isGenericS4("getRiskModelPrefix")) {
  setGeneric("getRiskModelPrefix", function(object,...){standardGeneric("getRiskModelPrefix")})
}
# Returns model_prefix of class "VirtualRiskModel"
#
# Args:
#   object : object of type "VirtualRiskModel"
# Returns:
#   model_prefix : model prefix used for querries

setMethod("getRiskModelPrefix",  
          signature(object = "VirtualRiskModel"),
          function(object){
            return(object@model_prefix)
          }
)

if (!isGenericS4("getRiskModelLookback")) {
  setGeneric("getRiskModelLookback", function(object,...){standardGeneric("getRiskModelLookback")})
}
# Returns model_prefix of class "VirtualRiskModel"
#
# Args:
#   object : object of type "VirtualRiskModel"
# Returns:
#   model_prefix : model prefix used for querries

setMethod("getRiskModelLookback",  
          signature(object = "VirtualRiskModel"),
          function(object){
            return(object@lookback)
          }
)

if (!isGenericS4("getRiskModelUniverse")) {
  setGeneric("getRiskModelUniverse", function(object,...){standardGeneric("getRiskModelUniverse")})
}
# Returns model_universe of class "VirtualRiskModel"
#
# Args:
#   object : object of type "VirtualRiskModel"
# Returns:
#   model_universe : name of the universe covered by the model

setMethod("getRiskModelUniverse",  
          signature(object = "VirtualRiskModel"),
          function(object){
            return(object@model_universe)
          }
)

if (!isGenericS4("getRiskModelFactorNames")) {
  setGeneric("getRiskModelFactorNames", function(object,...){standardGeneric("getRiskModelFactorNames")})
}
# Returns risk model factors vector of names of class "VirtualRiskModel"
#
# Args:
#   object : object of type "VirtualRiskModel"
# Returns:
#   model_factors : character vector of factor names

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
risk_model_market_factors <- c('Earnings','Growth','PriceMomentum12M','PriceMomentum1M','Size','StreetSentiment','Strength','TrendExtension','Value','Volatility')
risk_model_currency_factors <- c('JPY','GBP','EUR','CNY','RUB','ZAR','HKD','AUD','DKK','NOK','SEK','CHF','ILS','PLN','HUF','TRY')
risk_model_commodity_factors <- c('WTI')
risk_model_sector_factors <- c('SX3P','SX4P','SX6P','SX7P','SX86P','SX8P','SXAP','SXDP','SXEP','SXFP','SXKP','SXMP','SXNP','SXOP','SXPP','SXQP','SXRP','SXTP')


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
setClass(
  Class     = "RiskModel.DevelopedEuropePrototype150",
  prototype = list(
    model_universe  = "developed_europe",
    lookback        = 150L,
    model_prefix    = "developed_europe_prototype"
  ),
  contains  = c("RiskModel.DevelopedEurope")
)