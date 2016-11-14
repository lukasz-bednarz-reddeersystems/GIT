#' @include referencedata.r
#' @include risk_model_rodbc_client.r
NULL

####################################
#
# VirtualFactorVarianceData Class
#
####################################

#' Virtual S4 class handling Factor Variance data.
#'
#' Implements storage for Factor Variance
#'
#' Inherits from "VirtualReferenceData

setClass(
  Class                = "VirtualFactorVarianceData",
  prototype = list(
    required_colnms = c('Date')
  ),
  contains = c("VirtualReferenceData", "VIRTUAL")
)

#' Concrtete S4 class handling Factor Variance data.
#'
#' Implements storage for Factor Variance and access
#' to Factor Variance data via Risk Model Objectstore
#'
#' Inherits from "VirtualFactorVarianceData" and "VirtualRiskModelFactorDependentClientSelector"
#' @export

setClass(
  Class             = "FactorVarianceData",
  prototype      = list(
    component          = "FactorVariance", # name of component
    key_cols        = c(risk_model_objectstore_keys),
    key_values      = data.frame(Date = as.Date(character())),
    values             = c("Date") # columns that neeed to be returned from datastore
    ),

  contains = c("VirtualFactorVarianceData", "VirtualRiskModelFactorDependentClientSelector")
)

