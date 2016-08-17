#' @include risk_model.r
NULL

####################################
#
# VirtualRiskModelHandlerHandler Class
#
####################################


#' Virtual S4 class for handling Risk Model class.
#'
#' This is handler class that is to be inherited
#' by other classes handling Risk Model Objects
#'
#' Inherits from "VirtualReferenceObject"
#'
#' @slot risk_model    "VirtualRiskModel", default is "RiskModel.DevelopedEuropePrototype150"

setClass(
  Class     = "VirtualRiskModelHandler",
  slots     = c(
    risk_model = "VirtualRiskModel"
  ),
  prototype = list(
    risk_model = new("RiskModel.DevelopedEuropePrototype150")
  ),
  contains  = c("VIRTUAL")
)


#' Get Risk Model Object
#'
#' Returns risk model object
#'
#' @param object object of class 'VirtualRiskModelHandler'.
#' @return \code{risk_model} object of class "VirtualRiskModel".
#' @export

setGeneric("getRiskModelObject", function(object,...){standardGeneric("getRiskModelObject")})

setMethod("getRiskModelObject",
          signature(object = "VirtualRiskModelHandler"),
          function(object){
            return(object@risk_model)
          }
)


#' Set Risk Model Object
#'
#' Private method to set risk_model object slot value
#'
#' @param object object of class 'VirtualRiskModelHandler'.
#' @param risk_model object of class "VirtualRiskModel".
#' @return \code{object} object of class 'VirtualRiskModelHandler'.
#' @export

setGeneric(".setRiskModelObject", function(object, risk_model, ...){standardGeneric(".setRiskModelObject")})
setMethod(".setRiskModelObject",
          signature(object = "VirtualRiskModelHandler",
                    risk_model = "VirtualRiskModel"),
          function(object, risk_model){
            object@risk_model <- risk_model
            return(object)
          }
)


#' Set Risk Model Object
#'
#' Public method to set risk_model object slot value.
#' Inheriting class has to implement this method in order to be
#' functional
#'
#' @param object object of class 'VirtualRiskModelHandler'.
#' @param risk_model object of class "VirtualRiskModel".
#' @return \code{object} object of class 'VirtualRiskModelHandler'.
#' @export

setGeneric("setRiskModelObject", function(object, risk_model, ...){standardGeneric("setRiskModelObject")})
setMethod("getRiskModelName",
          signature(object = "VirtualRiskModelHandler"),
          function(object){
            return(getRiskModelName(getRiskModelObject(object)))
          }
)


setMethod("getRiskModelLookback",
          signature(object = "VirtualRiskModelHandler"),
          function(object){
            return(getRiskModelLookback(getRiskModelObject(object)))
          }
)


setMethod("getRiskModelPrefix",
          signature(object = "VirtualRiskModelHandler"),
          function(object){
            return(getRiskModelPrefix(getRiskModelObject(object)))
          }
)

setMethod("getRiskModelUniverse",
          signature(object = "VirtualRiskModelHandler"),
          function(object){
            return(getRiskModelUniverse(getRiskModelObject(object)))
          }
)

setMethod("getRiskModelFactorNames",
          signature(object = "VirtualRiskModelHandler"),
          function(object){
            return(getRiskModelFactorNames(getRiskModelObject(object)))
          }
)
