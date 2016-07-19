sourceTo("../lib/referencedata/referenceobject.r", modifiedOnly = TRUE, local = FALSE)

####################################
#
# VirtualReferenceData Class
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


setClassUnion("NullableReferenceData", c("VirtualReferenceData", "NULL"))



setGeneric("getForceUniqueRows", function(object,...){standardGeneric("getForceUniqueRows")})
# Returns Names of the variables that are currently stored in given ReferenceData class instance.
#
# Args:
#   object : object of type Portfolio
# Returns:
#   storeddVariablesNames

setMethod("getForceUniqueRows", "VirtualReferenceData",
          function(object){
            return(object@unique_rows)
          }
)


setGeneric("getStoredVariablesNames", function(object,...){standardGeneric("getStoredVariablesNames")})
# Returns Names of the variables that are currently stored in given ReferenceData class instance.
#
# Args:
#   object : object of type Portfolio
# Returns:
#   storeddVariablesNames

setMethod("getStoredVariablesNames", "VirtualReferenceData",
          function(object){
            return(object@column_names)
          }
)

setGeneric("getStoredNRows", function(object,...){standardGeneric("getStoredNRows")})
# Returns Number of rows of stored data in given ReferenceData class instance.
#
# Args:
#   object : object of type Portfolio
# Returns:
#   nRows

setMethod("getStoredNRows", "VirtualReferenceData",
          function(object){
            return(object@stored_rows)
          }
)



setGeneric("setStoredNRows", function(object,nrows, ...){standardGeneric("setStoredNRows")})
# Sets Number of rows of stored data in given ReferenceData class instance.
#
# Args:
#   object : object of type ReferenceData
#   nRows
# Returns:
#   object: object of type  ReferenceData

setMethod("setStoredNRows", 
          signature(object = "VirtualReferenceData", nrows = "numeric"),
          function(object, nrows){
            object@stored_rows <- nrows
            return(object)
          }
)

setValidity("VirtualReferenceData",
            function(object){
              if (has_required_columns(getReferenceData(object), getRequiredVariablesNames(object))) {
                return(TRUE)
              } else {
                paste("Operation resulted in invalid columns beeing set on the Object.")
              }
            }
            
)

setGeneric(".setStoredVariablesNames", function(object, names, ...){standardGeneric(".setStoredVariablesNames")})
# Private function to set stored variable names only to be used by derived classes
# Args:
#   object : object extending VirtualReferenceObject
# Returns:
#   object : object extending VirtualReferenceObject

setMethod(".setStoredVariablesNames", 
          signature(object = "VirtualReferenceObject", names = "character"),
          function(object, names){
            object@column_names <- names
            return(object)
          }
)

setGeneric("updateStoredVariablesNames", function(object,...){standardGeneric("updateStoredVariablesNames")})
# updates Names of the variables that are currently stored in given ReferenceData class instance.
#
# Args:
#   object : object of type Portfolio
# Returns:
#   object : object of type Portfolio

setMethod("updateStoredVariablesNames", "VirtualReferenceData",
          function(object){
            object <- .setStoredVariablesNames(object, colnames(getReferenceData(object)))
            
            tryCatch ({
              validObject(object)
            }, error = function(cond){
              message(paste("Object", class(object), "became invalid after call to updateStoredVariablesNames()", cond))
              stop("Failure when updating storedVariablesNames")
            })

            return(object) 
          }
)




setGeneric("setReferenceData", function(object,data, ...){standardGeneric("setReferenceData")})
# Sets internal datastore to new data and performs data validity checks .
#
# Args:
#   object : object of type ReferenceData
#   data: data to store in 
# Returns:
#   Updated Reference Data object with new data.

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
            object <- setStoredNRows(object, nrow(data))
            
            tryCatch ({
              validObject(object)
            }, error = function(cond){
              message(paste("Object", class(object), "became invalid after call to setReferenceData()", cond))
              stop("Failure when updating setting ReferenceData")
            })
            object <- updateStoredVariablesNames(object)
            
            return(object)
          }
)



setGeneric("getReferenceData", function(object, ...){standardGeneric("getReferenceData")})
# returns data.frame from internal datastore.
#
# Args:
#   object : object of type ReferenceData
# Returns:
#   data.frame with internal data.
setMethod("getReferenceData","VirtualReferenceData",
          function(object){
            return(object@data)           
          }
)

setGeneric("updateVariables",function(object, data, var.names){standardGeneric("updateVariables")})
# Update values of specified Variables.
#
# Args:
#   object : object of type Portfolio
#   data : data.frame with new data
#   colnms : colnames to update
# Returns:
#   data.frame with internal data.
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
            
            object <- updateStoredVariablesNames(object)
            
            return(object)
          }
)


setGeneric("appendVariables",function(object, data, var.names    ){standardGeneric("appendVariables")})
# append new Variables to portfolio .
#
# Args:
#   object : object of type Portfolio
#   data : data.frame with new data
#   colnms : colnames to update
# Returns:
#   data.frame with internal data.
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
                message(paste("Variables in Portfolio:",paste(stored,collapse=" ")))
                message(paste("Variables in incoming data:",paste(stored.vars,collapse=" ")))
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

