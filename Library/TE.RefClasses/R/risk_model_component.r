setClass("VirtualRiskModelFactorDependentComponent",
         contains = c("VirtualRiskModelHandler", "VIRTUAL"))


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

            # retrieve factor names from specific_risk model
            factor_names <- getRiskModelFactorNames(.Object)

            # make these required colnames
            req_cols <- getRequiredVariablesNames(.Object)

            .Object <- .setRequiredVariablesNames(.Object, c(req_cols, factor_names))

          }
)


