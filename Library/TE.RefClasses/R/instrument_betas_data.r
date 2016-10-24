#' @include referencedata.r
#' @include risk_model_rodbc_client.r
NULL

####################################
#
# InstrumentBetasData Class
#
####################################

#' Virtual S4 class handling Instrument Betas data.
#'
#' Implements storage for Factor Correlation
#'
#' Inherits from "VirtualReferenceData"

setClass(
  Class                = "VirtualInstrumentBetasData",
  prototype = list(
    required_colnms = c('Date','InstrumentID')
  ),
  contains = c("VirtualReferenceData", "VIRTUAL")
)


#' Concrtete S4 class handling Instrument Betas data.
#'
#' Implements storage for Instrument Betas and access
#' to Factor Correlation data via Risk Model Objectstore
#'
#' Inherits from "VirtualInstrumentBetasData" and "VirtualRiskModelClientPicker"
#' @export

setClass(
  Class             = "InstrumentBetasData",
  prototype      = list(
    component          = "Betas", # name of component
    key_cols        = c(risk_model_objectstore_keys, "InstrumentID"),
    key_values      = data.frame(Date = as.Date(character()),
                                 InstrumentID = integer()),
    values             = c("Date", "InstrumentID") # columns that neeed to be returned from datastore
    ),

  contains = c("VirtualInstrumentBetasData", "VirtualRiskModelClientPicker")
)

