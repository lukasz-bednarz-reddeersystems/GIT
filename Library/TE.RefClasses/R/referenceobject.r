#' Virtual S4 class implementing handling of required variables.
#'
#' Implements handling of required variables and (tbd) error
#' handling
#'
#' @slot has_error         "logical"
#' @slot error_list        "character"
#' @slot last_error        "character"
#' @slot required_colnms   "character"
#' @export

setClass(
  Class          = "VirtualReferenceObject",
  slots = c(
    has_error = "logical",
    error_list = "character",
    last_error = "character",
    required_colnms = "character"
  ),
  prototype      = list(
    has_error = FALSE,
    error_list = "",
    last_error = ""
  ),
  contains = c("VIRTUAL")
)


#' Get names of required variables stored in data
#'
#' @param object object of class 'VirtualReferenceObject'.
#' @return \code{required_colnms} character vector, list of required column names
#' @export

setGeneric("getRequiredVariablesNames", function(object){standardGeneric("getRequiredVariablesNames")})

#' @describeIn getRequiredVariablesNames
#' Get names of required variables stored in data
#'
#' @inheritParams getRequiredVariablesNames
#' @return \code{required_colnms} character vector, list of required column names
#' @export
setMethod("getRequiredVariablesNames", "VirtualReferenceObject",
          function(object){
            return(object@required_colnms)
          }
)

#' Private function to set required variable names only to be used by derived classes
#'
#' @param object object extending VirtualReferenceObject
#' @return \code{object} object extending VirtualReferenceObject
setGeneric(".setRequiredVariablesNames", function(object, names){standardGeneric(".setRequiredVariablesNames")})

setMethod(".setRequiredVariablesNames",
          signature(object = "VirtualReferenceObject", names = "character"),
          function(object, names){
            object@required_colnms <- names
            return(object)
          }
)


has_required_columns <- function(data, required_colnms) {
  columns <- colnames(data)

  if (all(required_colnms %in% columns)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
