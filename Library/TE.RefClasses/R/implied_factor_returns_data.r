#' @include referencedata.r
#' @include risk_model_rodbc_client.r
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
    required_colnms = c('Date')
  ),
  contains = c("VirtualReferenceData", "VIRTUAL")
)

#' Concrtete S4 class handling Implied Factor Returns data.
#'
#' Implements storage for Implied Factor Returns and access
#' to Implied Factor Returns data via Risk Model Objectstore
#'
#' Inherits from "VirtualImpliedFactorReturnsData" and "VirtualRiskModelFactorDependentClientSelector"
#' @export

setClass(
  Class             = "ImpliedFactorReturnsData",
  prototype      = list(
    component          = "ImpliedFactorReturns", # name of component
    key_cols        = c(risk_model_objectstore_keys),
    key_values      = data.frame(Date = as.Date(character())),
    values             = c("Date") # columns that neeed to be returned from datastore
    ),

  contains = c("VirtualImpliedFactorReturnsData", "VirtualRiskModelFactorDependentClientSelector")
)

