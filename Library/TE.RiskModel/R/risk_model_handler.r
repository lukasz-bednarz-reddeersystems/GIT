#' @include risk_model_handler.r
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
#'
#' @export

setClass(
  Class     = "VirtualRiskModelHandler",
  slots     = c(
    risk_model = "VirtualRiskModel"
  ),
  prototype = list(
    risk_model = new("RiskModel.DevelopedEuropePrototype150.1.1")
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

setGeneric("getRiskModelObject", function(object){standardGeneric("getRiskModelObject")})

#' @describeIn getRiskModelObject
#' Get Risk Model Object
#'
#' Returns risk model object
#'
#' @inheritParams getRiskModelObject
#' @return \code{risk_model} object of class "VirtualRiskModel".
#' @export
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

setGeneric(".setRiskModelObject", function(object, risk_model){standardGeneric(".setRiskModelObject")})

#' @describeIn .setRiskModelObject
#' Set Risk Model Object
#'
#' Private method to set risk_model object slot value
#'
#' @inheritParams .setRiskModelObject
#' @return \code{object} object of class 'VirtualRiskModelHandler'.

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

setGeneric("setRiskModelObject", function(object, risk_model){standardGeneric("setRiskModelObject")})





#' Get Risk Model Name
#'
#' Returns name of Risk Model
#'
#' @param object object of class 'VirtualRiskModelHandler'.
#' @return \code{model_name} 'character', name of the model.
#' @export

setMethod("getRiskModelName",
          signature(object = "VirtualRiskModelHandler"),
          function(object){
            return(getRiskModelName(getRiskModelObject(object)))
          }
)


#' Get Risk Model Lookback
#'
#' Returns model lookback value in days. Lookback is number of
#' calendar days over which model is calculated.
#'
#' @param object object of class 'VirtualRiskModelHandler'.
#' @return \code{lookback} 'integer'
#' @export

setMethod("getRiskModelLookback",
          signature(object = "VirtualRiskModelHandler"),
          function(object){
            return(getRiskModelLookback(getRiskModelObject(object)))
          }
)


#' Get Risk Model Prefix
#'
#' Returns prefix of Risk Model used to construct model store querries
#'
#' @param object object of class 'VirtualRiskModelHandler'.
#' @return \code{model_prefix} 'character', model prefix.
#' @export
setMethod("getRiskModelPrefix",
          signature(object = "VirtualRiskModelHandler"),
          function(object){
            return(getRiskModelPrefix(getRiskModelObject(object)))
          }
)



#' Get Risk Model Universe
#'
#' Returns name of universe for which model is computed.
#'
#' @param object object of class 'VirtualRiskModelHandler'.
#' @return \code{model_universe} 'character', name of the universe covered by the model
#' @export
setMethod("getRiskModelUniverse",
          signature(object = "VirtualRiskModelHandler"),
          function(object){
            return(getRiskModelUniverse(getRiskModelObject(object)))
          }
)


#' Get Risk Model Factor Names
#'
#' Returns names of factors included in the model
#'
#' @param object object of class 'VirtualRiskModelHandler'.
#' @return \code{model_factors} 'character', vector of factor names
#' @export

setMethod("getRiskModelFactorNames",
          signature(object = "VirtualRiskModelHandler"),
          function(object){
            return(getRiskModelFactorNames(getRiskModelObject(object)))
          }
)
