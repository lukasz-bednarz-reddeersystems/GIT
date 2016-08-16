#' @include referencedata.r
#' @include risk_model_objectstore_client.r
NULL

####################################
#
# VirtualImpliedFactorReturnsData Class
#
####################################

#' Virtual S4 class handling Implied Factor Returns data.
#'
#' Implements storage for Implied Factor Returns
#'
#' Inherits from "VirtualReferenceData

setClass(
  Class                = "VirtualImpliedFactorReturnsData",
  prototype = list(
    required_colnms = c('Date',
                        risk_model_market_factors,
                        risk_model_currency_factors,
                        risk_model_commodity_factors,
                        risk_model_sector_factors)
  ),
  contains = c("VirtualReferenceData", "VIRTUAL")
)


#' Concrtete S4 class handling Implied Factor Returns data.
#'
#' Implements storage for Implied Factor Returns and access
#' to Implied Factor Returns data via Risk Model Objectstore
#'
#' Inherits from "VirtualFactorVarianceData" and "VirtualRiskModelObjectstoreClient"
#' @export

setClass(
  Class             = "ImpliedFactorReturnsData",
  prototype      = list(
    component          = "ImpliedFactorReturns", # name of component
    key_cols        = c(risk_model_objectstore_keys),
    key_values      = data.frame(Date = as.Date(character())),
    values             = c("Date",
                           risk_model_market_factors,
                           risk_model_currency_factors,
                           risk_model_commodity_factors,
                           risk_model_sector_factors), # columns that neeed to be returned from datastore
    column_name_map = hash()

    ),

  contains = c("VirtualImpliedFactorReturnsData", "VirtualRiskModelObjectstoreClient")
)

