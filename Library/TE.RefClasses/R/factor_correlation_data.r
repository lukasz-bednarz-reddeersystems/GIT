#' @include referencedata.r
#' @include risk_model_objectstore_client.r
NULL

####################################
#
# VirtualFactorCorrelationData Class
#
####################################

#' Virtual S4 class handling Factor Correlation data.
#'
#' Implements storage for Factor Correlation
#'
#' Inherits from "VirtualReferenceData"

setClass(
  Class                = "VirtualFactorCorrelationData",
  prototype = list(
    required_colnms = c('Date',
                        risk_model_market_factors,
                        risk_model_currency_factors,
                        risk_model_commodity_factors,
                        risk_model_sector_factors)
  ),
  contains = c("VirtualReferenceData", "VIRTUAL")
)

#' Concrtete S4 class handling Factor Correlation data.
#'
#' Implements storage for Factor Correlation and access
#' to Factor Correlation data via Risk Model Objectstore
#'
#' Inherits from "VirtualFactorCorrelationData" and "VirtualRiskModelObjectstoreClient"
#' @export

setClass(
  Class             = "FactorCorrelationData",
  prototype      = list(
    component          = "FactorCorrelation", # name of component
    key_cols        = c(risk_model_objectstore_keys),
    key_values      = data.frame(Date = as.Date(character())),
    values             = c("Date",
                           risk_model_market_factors,
                           risk_model_currency_factors,
                           risk_model_commodity_factors,
                           risk_model_sector_factors), # columns that neeed to be returned from datastore
    column_name_map = hash()

    ),

  contains = c("VirtualFactorCorrelationData", "VirtualRiskModelObjectstoreClient")
)

