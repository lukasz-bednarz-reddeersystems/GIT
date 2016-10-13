####################################
#
# VirtualDataValidator Class
#
####################################

#' Virtual S4 class defining data validation operations
#'
#' Class that is to be inherited by any objects
#' that will contain VirtualDataValidator or derived classes
#'
#' The validator applies a test to a data frame.
#' The test is in the form of a selection of data within the frame as
#' specifed by some function.
#' If the data passes this test then it is validated.
#' If the size of the data would be modified by this selection then the data
#' fails validation, in which case the data passing the validation can be returned
#' and a warning raised, or the validator can halt execution.
#' To construct the validator there needs to be a function that takes as its arguments,
#' the dataframe to be validated and an optional set of arguments that each represent tnnamed list having names matching the
#'
#' @slot validation_function "function", function applied to the data frame to be validated
#' @slot argument_list, named list linking the parameter names of the validation_function to column names
#' @slot halt, if true execution is halted on failed validation
#' @slot mssg displyed if validation is failed

setClass(
  Class  = "VirtualDataValidator",
  slots  = c(
    validation_function = "function",
    argument_list = "list",
    halt = "logical",
    mssg = "character"),
  prototype = list(
    validation_function = function(data)data,
    halt = FALSE,
    mssg = "Data validation failed."
  ),
  contains = c("VIRTUAL")
)

#' Apply validation to dataframe
#'
#' Public method to return dataframe post validation
#' or halt execution of halt is TRUE
#'
#' @param object object of class 'VirtualDataValidator'.
#' @export
setGeneric("validateData",function(object,data){standardGeneric("validateData")})

#' @describeIn validateData
#' Apply validation to dataframe
#'
#' Public method to return dataframe post validation
#' or halt execution of halt is TRUE
#'
#' @inheritParams validateData
#' @return \code{data} data.frame of validated data
#' @export
setMethod("validateData",
          signature(object = "VirtualDataValidator", data = "data.frame"),
          function(object,data){

            if(length(object@argument_list)>0){
              args <- object@argument_list
              args[["data"]] <- data
              tryCatch({
                out_data <- do.call(object@validation_function,args)
              }, error = function(cond){
                stop(paste("Validation function raised exception:",cond))
              })
            } else {
              out_data <- object@validation_function(data)
            }
            if(nrow(data)!=nrow(out_data)){
              if(object@halt){
                stop(object@mssg)
              } else {
                message(object@mssg)
              }
            }
            return(out_data)
          }
)

