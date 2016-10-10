#' @include datasource_client.r
NULL

####################################
#
# VirtualAvailableValues Class
#
####################################

#' Virtual S4 class providing a mechanism for
#' to query the possible values of a variable.
#'
#' .obtainValues implements the method by
#' which the values are determined.
#'
#' dataRequest implementation supplies the
#' values.
#'
#' Inherits from "VirtualDataSourceClient

setClass(
  Class = "VirtualAvailableValues",
  slots = c(column_name="character"),
  contains = c("VirtualDataSourceClient")
)

#' Provides the implementation that defines the
#' available values.
#'
#' Should be called within the dataRequest method.
#'
#' @rdname private_obtainValues
#' @param object object of class 'VirtualAvailableValues'.
#' @param ... any parameters passed to specific methods implemented by
#' inheriting classes
#' @return \code{values} "data.frame" with values

setGeneric(".obtainValues", function(object, ...){standardGeneric(".obtainValues")})



