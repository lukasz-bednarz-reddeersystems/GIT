setClass("VirtualRiskModelFactorDependentComponent",
         contains = c("VirtualRiskModelHandler", "VIRTUAL"))




setGeneric(".initializeRiskModelDependentValues",
           function(object){standardGeneric(".initializeRiskModelDependentValues")})

setMethod(".initializeRiskModelDependentValues",
          "VirtualRiskModelFactorDependentComponent",
          function(object){

            # retrieve factor names from specific_risk model
            factor_names <- getRiskModelFactorNames(object)

            # make these required colnames
            req_cols <- getRequiredVariablesNames(object)

            object <- .setRequiredVariablesNames(object, c(req_cols, factor_names))

            return(object)
          }
)

#' initialize method for "VirtualRiskModelFactorDependentComponent" derived classes
#'
#' initializes required column names from the values obtained from contained risk model
#'
#' @param .Object object of class derived from "VirtualRiskModelFactorDependentComponent"
#'
#' @export

setMethod("initialize",
          "VirtualRiskModelFactorDependentComponent",
          function(.Object){
            .Object <- .initializeRiskModelDependentValues(.Object)
            return(.Object)
          }
)

#' Set risk_model object in object slot
#'
#' Public method to set trade_data slot with "VirtualRiskModel"
#' class object
#'
#' @rdname setRiskModelObject-VirtualRiskModelFactorDependentComponent-method
#' @param object object of class "VirtualRiskModelFactorDependentComponent"
#' @param risk_model object of class "VirtualRiskModel"
#' @return \code{object} object of class "VirtualRiskModelFactorDependentComponent"
#' @export

setMethod("setRiskModelObject",
          signature(object = "VirtualRiskModelFactorDependentComponent",
                    risk_model = "VirtualRiskModel"),
          function(object, risk_model){
            object <- TE.RiskModel:::.setRiskModelObject(object, risk_model)
            object <- .initializeRiskModelDependentValues(object)
            return(object)
          }
)
