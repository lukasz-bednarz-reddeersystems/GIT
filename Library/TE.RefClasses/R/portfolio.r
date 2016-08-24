#' @include referencedata.r
NULL

####################################
#
# Virtual Portfolio Class
#
####################################


#' Virtual S4 class implementing handling of portfolio data.
#'
#' Implements handling of portfolio data
#' handling. Inherits from "VirtualReferenceData".
#'
#' @slot start        "Date"
#' @slot end          "Date

setClass(
  Class          = "Portfolio",
  slots = c(
    start = "Date",
    end = "Date"
  ),
  prototype      = list(
    required_colnms = c('Date','InstrumentID','Weight'),
    start = as.Date(character()),
    end = as.Date(character())
  ),
  contains = c("VirtualReferenceData", "VIRTUAL")
)


#' Pool data required for portfolio
#'
#' Pools data from data source specific to derived class implementation
#' Also computes portfolio weights if necessary
#'
#' @param object object of class 'Portfolio'.
#' @param start Date start of data range.
#' @param end Date end of data range.
#' @return \code{object} object of class 'Portfolio'.
#' @export

setGeneric("buildPortfolioHistory", function(object, start, end){standardGeneric("buildPortfolioHistory")})


#' Get start date of portfolio timespan
#'
#' @param object object of class 'Portfolio'.
#' @return \code{start} Date start of data range.
#' @export

setGeneric("getStartDate", function(object){standardGeneric("getStartDate")})

#' @describeIn getStartDate
#' Get start date of portfolio timespan
#'
#' @inheritParams getStartDate
#' @return \code{start} Date start of data range.
#' @export
setMethod("getStartDate",
          signature(object = "Portfolio"),
          function(object){
            return(object@start)
          }
)


#' Set start date of portfolio timespan
#'
#' @param object object of class 'Portfolio'.
#' @param start Date start of data range.
#' @return \code{object} object of class 'Portfolio'.
#' @export

setGeneric("setStartDate", function(object, start){standardGeneric("setStartDate")})

#' @describeIn setStartDate
#' Set start date of portfolio timespan
#'
#' @inheritParams setStartDate
#' @return \code{object} object of class 'Portfolio'.
#' @export
setMethod("setStartDate",
          signature(object = "Portfolio", start = "Date"),
          function(object, start){
            if (is.Date(start) && length(start) == 1) {
              object@start <- start
            } else {
              stop(paste("Trying to set ivalid value in setStartDate(", start,")"))
            }
            return(object)
          }
)

#' Get end date of portfolio timespan
#'
#' @param object object of class 'Portfolio'.
#' @return \code{end} Date end of data range.
#' @export

setGeneric("getEndDate", function(object){standardGeneric("getEndDate")})

#' @describeIn getEndDate
#' Get end date of portfolio timespan
#'
#' @inheritParams getEndDate
#' @return \code{end} Date end of data range.
#' @export
setMethod("getEndDate",
          signature(object = "Portfolio"),
          function(object){
            return(object@end)
          }
)

#' Set end date of portfolio timespan
#'
#' @param object object of class 'Portfolio'.
#' @param end Date end of data range.
#' @return \code{object} object of class 'Portfolio'.
#' @export

setGeneric("setEndDate", function(object, end){standardGeneric("setEndDate")})

#' @describeIn setEndDate
#' Set end date of portfolio timespan
#'
#' @inheritParams setEndDate
#' @return \code{object} object of class 'Portfolio'.
#' @export
setMethod("setEndDate",
          signature(object = "Portfolio"),
          function(object, end){
            if (is.Date(end) && length(end) == 1) {
              object@end <- end
            } else {
              stop(paste("Trying to set ivalid value in setEndDate(", end,")"))
            }
            return(object)
          }
)
