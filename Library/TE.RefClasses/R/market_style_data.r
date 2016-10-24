#' @include referencedata.r
#' @include risk_model_rodbc_client.r
NULL

####################################
#
# VirtualMarketStyleData Class
#
####################################

#' Virtual S4 class handling Market Style data.
#'
#' Implements storage for Market Style
#'
#' Inherits from "VirtualReferenceData

setClass(
  Class                = "VirtualMarketStyleData",
  prototype = list(
    required_colnms = c('Date')
  ),
  contains = c("VirtualReferenceData", "VIRTUAL")
)


#' Concrtete S4 class handling Market Style data.
#'
#' Implements storage for Market Style and access
#' to Market Style data via Risk Model Objectstore
#'
#' Inherits from "VirtualFactorVarianceData" and "VirtualRiskModelClientPicker"
#' @export

setClass(
  Class             = "MarketStyleData",
  prototype      = list(
    component          = "MarketStyle", # name of component
    key_cols        = c(risk_model_objectstore_keys),
    key_values      = data.frame(Date = as.Date(character())),
    values             = c("Date") # columns that neeed to be returned from datastore
    ),

  contains = c("VirtualMarketStyleData", "VirtualRiskModelClientPicker")
)

