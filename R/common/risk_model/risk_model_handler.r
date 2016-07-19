sourceTo("../common/risk_model/risk_model.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

####################################
#
# VirtualRiskModelHandlerHandler Class
#
####################################

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

if (!isGenericS4("getRiskModelObject")) {
  setGeneric("getRiskModelObject", function(object,...){standardGeneric("getRiskModelObject")})
}
# Returns risk model object  of class "VirtualRiskModel"
#
# Args:
#   object : object of type "VirtualRiskModelHandler"
# Returns:
#   risk_model : object of class "VirtualRiskModel"

setMethod("getRiskModelObject",  
          signature(object = "VirtualRiskModelHandler"),
          function(object){
            return(object@risk_model)
          }
)

if (!isGenericS4(".setRiskModelObject")) {
  setGeneric(".setRiskModelObject", function(object, risk_model, ...){standardGeneric(".setRiskModelObject")})
}
# private method to set risk_model object slot class "VirtualRiskModelHandler"
#
# Args:
#   object : object of type "VirtualRiskModelHandler"
#   risk_model: object of type "VirtualRiskModel"
# Returns:
#   object : object of type "VirtualRiskModelHandler"

setMethod(".setRiskModelObject",  
          signature(object = "VirtualRiskModelHandler",
                    risk_model = "VirtualRiskModel"),
          function(object, risk_model){
            object@risk_model <- risk_model
            return(object)
          }
)

if (!isGenericS4("setRiskModelObject")) {
  setGeneric("setRiskModelObject", function(object, risk_model, ...){standardGeneric("setRiskModelObject")})
}
# Public method to set risk_model object slot class "VirtualRiskModelHandler"
#
# Args:
#   object : object of type "VirtualRiskModelHandler"
#   risk_model: object of type "VirtualRiskModel"
# Returns:
#   object : object of type "VirtualRiskModelHandler"



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
