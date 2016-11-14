#' @include referencedata.r
#' @include risk_model_rodbc_client.r
NULL

####################################
#
# InstrumentResidualReturnsData Class
#
####################################

#' Virtual S4 class handling Instrument Betas data.
#'
#' Implements storage for Factor Correlation
#'
#' Inherits from "VirtualReferenceData"

setClass(
  Class                = "VirtualInstrumentResidualReturnsData",
  prototype = list(
    required_colnms = c('Date','InstrumentID', 'Return')
  ),
  contains = c("VirtualReferenceData", "VIRTUAL")
)


#' Concrtete S4 class handling Instrument Betas data.
#'
#' Implements storage for Instrument Betas and access
#' to Factor Correlation data via Risk Model Objectstore
#'
#' Inherits from "VirtualInstrumentResidualReturnsData" and "VirtualRiskModelFactorDependentClientSelector"
#' @export

setClass(
  Class             = "InstrumentResidualReturnsData",
  prototype      = list(
    component          = "ResidualReturns", # name of component
    required_colnms = c('Date','InstrumentID', 'Return'),
    key_cols        = c(risk_model_objectstore_keys, "InstrumentID"),
    key_values      = data.frame(Date = as.Date(character()),
                                 InstrumentID = integer()),
    values             = c("Date", "InstrumentID", "Return") # columns that neeed to be returned from datastore
    ),

  contains = c("VirtualInstrumentResidualReturnsData",
               "VirtualRiskModelNonFactorDependentClientSelector")
)

