#' @include referenceobject.r
NULL

####################################
#
# VirtualReferenceData Class
#
####################################


referencedata_validity <- function(object){
  if (has_required_columns(getReferenceData(object), getRequiredVariablesNames(object))) {
    return(TRUE)
  } else {
    paste("Operation resulted in invalid columns beeing set on the Object.")
  }
}

#' Virtual S4 class implementing handling of reference data.
#'
#' Implements handling of reference data column names
#' handling. Inherits from "VirtualReferenceObject".
#'
#' @slot column_names      "character"
#' @slot data              "data.frame"
#' @slot unique_rows       "logical"
#' @slot stored_rows       "numeric"
#' @export

setClass(
  Class          = "VirtualReferenceData",
  slots = c(
    column_names    = "character",
    data         = "data.frame",
    unique_rows  = "logical",
    stored_rows = "numeric"
  ),
  prototype      = list(
    unique_rows  = TRUE,
    stored_rows = 0
    ),
  contains = c("VirtualReferenceObject","VIRTUAL"),
  validity = referencedata_validity
)


#' Union class NullableReferenceData
#' union of "VirtualReferenceData" and "NULL"
#'
#' @title NullableReferenceData-class
#' @name NullableReferenceData-class
#' @docType class
#' @exportClass NullableReferenceData
setClassUnion("NullableReferenceData", c("VirtualReferenceData", "NULL"))


#' Are stored rows forced to be unique
#'
#' Returns logical value indicating if stored reference data
#' rows are forced to be unique.
#'
#' @param object object of class 'VirtualReferenceData'.
#' @return \code{unique_rows} logical, are stored rows unique
#' @export

setGeneric("getForceUniqueRows", function(object){standardGeneric("getForceUniqueRows")})

#' @describeIn getForceUniqueRows
#' Are stored rows forced to be unique
#'
#' Returns logical value indicating if stored reference data
#' rows are forced to be unique.
#'
#' @inheritParams getForceUniqueRows
#' @return \code{unique_rows} logical, are stored rows unique
#' @export
setMethod("getForceUniqueRows", "VirtualReferenceData",
          function(object){
            return(object@unique_rows)
          }
)

#' Get names of stored reference data variables
#'
#' Returns vector of column names of stored data
#'
#' @param object object of class 'VirtualReferenceData'.
#' @return \code{column_names} character vector, list of stored column names
#' @export

setGeneric("getStoredVariablesNames", function(object){standardGeneric("getStoredVariablesNames")})

#' @describeIn getStoredVariablesNames
#' Get names of stored reference data variables
#'
#' Returns vector of column names of stored data
#'
#' @inheritParams getStoredVariablesNames
#' @return \code{column_names} character vector, list of stored column names
#' @export
setMethod("getStoredVariablesNames", "VirtualReferenceData",
          function(object){
            return(object@column_names)
          }
)

#' Number of currently stored rows
#'
#' Returns count of currently stored rows
#'
#' @param object object of class 'VirtualReferenceData'.
#' @return \code{stored_rows} integer, list of stored column names
#' @export

setGeneric("getStoredNRows", function(object){standardGeneric("getStoredNRows")})

#' @describeIn getStoredNRows
#' Number of currently stored rows
#'
#' Returns count of currently stored rows
#'
#' @inheritParams getStoredNRows
#' @return \code{stored_rows} integer, list of stored column names
#' @export
setMethod("getStoredNRows", "VirtualReferenceData",
          function(object){
            return(object@stored_rows)
          }
)


#' Set counter of rows
#'
#' Private method to set Number of rows of stored data in given ReferenceData class instance.
#'
#' @param object object of class 'VirtualReferenceData'.
#' @param nrows integer number of stored rows.
#' @return \code{object} object of class 'VirtualReferenceData'.

setGeneric(".setStoredNRows", function(object,nrows){standardGeneric(".setStoredNRows")})
setMethod(".setStoredNRows",
          signature(object = "VirtualReferenceData", nrows = "numeric"),
          function(object, nrows){
            object@stored_rows <- nrows
            return(object)
          }
)

#' Set stored variable names
#'
#' Private method to set stored variable names only to be used by derived classes
#'
#' @param object object of class 'VirtualReferenceData'.
#' @param names character vector of new Stored Variable names.
#' @return \code{object} object of class 'VirtualReferenceData'.

setGeneric(".setStoredVariablesNames", function(object, names){standardGeneric(".setStoredVariablesNames")})
setMethod(".setStoredVariablesNames",
          signature(object = "VirtualReferenceObject", names = "character"),
          function(object, names){
            object@column_names <- names
            return(object)
          }
)

#' updates stored variable names
#'
#' Private method to update variable names only to be used by derived classes
#'
#' @param object object of class 'VirtualReferenceData'.
#' @param names character vector of new Stored Variable names.
#' @return \code{object} object of class 'VirtualReferenceData'.

setGeneric(".updateStoredVariablesNames", function(object){standardGeneric(".updateStoredVariablesNames")})
setMethod(".updateStoredVariablesNames", "VirtualReferenceData",
          function(object){
            object <- .setStoredVariablesNames(object, colnames(getReferenceData(object)))

            tryCatch ({
              validObject(object)
            }, error = function(cond){
              message(paste("Object", class(object), "became invalid after call to .updateStoredVariablesNames()", cond))
              stop("Failure when updating storedVariablesNames")
            })

            return(object)
          }
)


#' Set stored data to value provided
#'
#' Sets new values to stored data. Erases any previous data without warning
#'
#' @param object object of class 'VirtualReferenceData'.
#' @param data data.frame with new data has to comply with required variable names.
#' @return \code{object} object object of class 'VirtualReferenceData'.
#' @export

setGeneric("setReferenceData", function(object,data){standardGeneric("setReferenceData")})

#' @describeIn setReferenceData
#' Set stored data to value provided
#'
#' Sets new values to stored data. Erases any previous data without warning
#'
#' @inheritParams setReferenceData
#' @return \code{object} object object of class 'VirtualReferenceData'.
#' @export
setMethod("setReferenceData",
          signature(object = "VirtualReferenceData", data = "data.frame"),
          function(object,data){
            required.colnms <- getRequiredVariablesNames(object)
            stored.colnms <- getStoredVariablesNames(object)

            message(paste("Updating", class(object), "object."))
            if(!has_required_columns(data, required.colnms))
            {
              message(paste("Error setting data in", class(object)))
              message(paste("Columns:",paste(colnames(data),collapse=" ")))
              message(paste("Required Columns:",paste(required.colnms,collapse=" ")))
              message(paste("Missing Columns:",paste(setdiff(required.colnms, colnames(data)),collapse=" ")))
              stop("Missing required Columns")
            } else if (!has_required_columns(data, stored.colnms)) {
              message(paste("Error setting data in", class(object)))
              message(paste("Columns:",paste(colnames(data),collapse=" ")))
              message(paste("Required Columns:",paste(stored.colnms,collapse=" ")))
              stop("New data has smaller number of collumns that existing data.")
            } else if (nrow(data) == 0) {
              message(paste("Error setting data in", class(object)))
              stop("Incoming data has zero rows.")
            }

            if (getForceUniqueRows(object)) {
              data <- unique(data)
            }
            rownames(data) <- seq(nrow(data))
            object@data <- data
            object <- .setStoredNRows(object, nrow(data))

            tryCatch ({
              validObject(object)
            }, error = function(cond){
              message(paste("Object", class(object), "became invalid after call to setReferenceData()", cond))
              stop("Failure when updating setting ReferenceData")
            })
            object <- .updateStoredVariablesNames(object)

            return(object)
          }
)


#' get stored data
#'
#' Returns data.frame with stored data.
#'
#' @param object object of class 'VirtualReferenceData'.
#' @return \code{data} data.frame with stored data..
#' @export

setGeneric("getReferenceData", function(object){standardGeneric("getReferenceData")})

#' @describeIn getReferenceData
#' get stored data
#'
#' Returns data.frame with stored data.
#'
#' @inheritParams getReferenceData
#' @return \code{data} data.frame with stored data..
#' @export
setMethod("getReferenceData","VirtualReferenceData",
          function(object){
            return(object@data)
          }
)

#' Update values in specified columns
#'
#' Sets new values to specified columns in stored data.
#' The number of rows of incoming data has to be the same as number of
#' rows currently stored.
#'
#' @param object object of class 'VirtualReferenceData'.
#' @param data data.frame with new data has to comply with required variable names.
#' @param var.names column names of data in both incomming data and stored data
#' @return \code{object} object object of class 'VirtualReferenceData'.
#' @export

setGeneric("updateVariables",function(object, data, var.names){standardGeneric("updateVariables")})

#' @describeIn updateVariables
#' Update values in specified columns
#'
#' Sets new values to specified columns in stored data.
#' The number of rows of incoming data has to be the same as number of
#' rows currently stored.
#'
#' @inheritParams updateVariables
#' @return \code{object} object object of class 'VirtualReferenceData'.
#' @export
setMethod("updateVariables",
          signature(object= "VirtualReferenceData", data = "data.frame", var.names = "character"),
          function(object, data, var.names    ){
            required.colnms <- getRequiredVariablesNames(object)
            stored.colnms <- getStoredVariablesNames(object)
            stored.data <- getReferenceData(object)

            if(!has_required_columns(data, var.names    ) || !has_required_columns(stored.data, var.names) )
            {
              message(paste("Error updating data in", class(object)))
              message(paste("Variables in Portfolio:",paste(stored.colnms,collapse=" ")))
              message(paste("Variables in incoming data:",paste(colnames(data),collapse=" ")))
              message(paste("Required Variables:",paste(var.names    ,collapse=" ")))
              stop("Missing required Columns")
            } else if(nrow(data) != nrow(object@data)) {
              message(paste("Error updating data in", class(object)))
              message(paste("Number of rows in incomming data:", nrow(data)))
              message(paste("Number of rows in existing data:",nrow(object@data)))
              stop("Nonmatching number of rows")
            } else {
              tryCatch({
                # merging on Date, can be abstracted in future
                stored.data[var.names] <- data[var.names]
                object <- setReferenceData(object, stored.data)
              },error=function(cond){
                message(paste("Could not replace data in columns :",var.names    , cond))
                stop("Check validity of incomming data")
              })
            }

            tryCatch ({
              validObject(object)
            }, error = function(cond){
              message(paste("Object", class(object), "became invalid after call to updateVariables()", cond))
              stop("Failure when updating Variables")
            })

            object <- .updateStoredVariablesNames(object)

            return(object)
          }
)

#' Append new columns to stored data
#'
#' Adds new columns to stored data
#' The number of rows of incoming data has to be the same as number of
#' rows currently stored.
#'
#' @param object object of class 'VirtualReferenceData'.
#' @param data data.frame with new data
#' @param var.names column names of data in both incomming data
#' @return \code{object} object object of class 'VirtualReferenceData'.
#' @export

setGeneric("appendVariables",function(object, data, var.names    ){standardGeneric("appendVariables")})

#' @describeIn appendVariables
#' Append new columns to stored data
#'
#' Adds new columns to stored data
#' The number of rows of incoming data has to be the same as number of
#' rows currently stored.
#'
#' @inheritParams appendVariables
#' @return \code{object} object object of class 'VirtualReferenceData'.
#' @export
setMethod("appendVariables",
          signature(object= "VirtualReferenceData", data = "data.frame", var.names     = "character"),
          function(object, data, var.names    ){
            required.vars <- getRequiredVariablesNames(object)
            stored.nrows <- getStoredNRows(object)

            for (variable in var.names    ) {
              if(!has_required_columns(data, variable    ) )
              {
                stored.vars <- getStoredVariablesNames(object)

                message(paste("Error appending", variable, "data in", class(object)))
                message(paste("Variables in Portfolio:",paste(stored.vars,collapse=" ")))
                message(paste("Variables in incoming data:",paste(colnames(data),collapse=" ")))
                message(paste("Required Variable:",paste(variable    ,collapse=" ")))
                stop("Missing required Columns")

              } else if(nrow(data) != stored.nrows) {
                message(paste("Error updating data in", class(object)))
                message(paste("Number of rows in incomming data:", nrow(data)))
                message(paste("Number of rows in existing data:",stored.nrows))
                stop("Nonmatching number of rows")

              }else if(variable %in% getStoredVariablesNames(object))
              {
                message(paste("Variable already present in Portfolio :", variable))
                message(paste("Updating with new Data"))
                return(updateVariables(object,data,variable))
              } else {
                new.data <- tryCatch({

                  stored.data <- getReferenceData(object)

                  new.data <- cbind(stored.data, data[variable])

                  #colnames(new.data) <- c(getStoredVariablesNames(object), variable)

                },error=function(cond){
                  message(paste("Could not replace data in column :",variable, cond))
                  stop("Check validity of incomming data")
                })

                object <- setReferenceData(object, new.data)

              }
            }
            tryCatch ({
              validObject(object)
            }, error = function(cond){
              message(paste("Object", class(object), "became invalid after call to appendVariables()", cond))
              stop("Failure when appending Variables")
            })

            return(object)
          }
)

####################################
#
# TestReferenceData Class
#
####################################
setClass(
  Class          = "VirtualReferenceData",
  slots = c(
    column_names    = "character",
    data         = "data.frame",
    unique_rows  = "logical",
    stored_rows = "numeric"
  ),
  prototype      = list(
    unique_rows  = TRUE,
    stored_rows = 0
  ),
  contains = c("VirtualReferenceObject","VIRTUAL")
)
