sourceTo("../lib/referencedata/referencedata.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(lubridate)



####################################
#
# Generic Portfolio Class
#
####################################

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


setGeneric("buildPortfolioHistory", function(object, start, end, ...){standardGeneric("buildPortfolioHistory")})
# Updates Portfolio object pooling data from MiddleWare via local cache.
#
# Args:
#   object : object of type Portfolio
#   start: Date type start of portfolio time span
#   end: Date type end of portfolio time span
# Returns:
#   Updated Portfolio object with data and computed weights.




setGeneric("getStartDate", function(object,...){standardGeneric("getStartDate")})
# Returns start slot value.
#
# Args:
#   object : object of type Portfolio
# Returns:
#   startDate

setMethod("getStartDate",
          signature(object = "Portfolio"),
          function(object){
            return(object@start)
          }
)

setGeneric("setStartDate", function(object, start, ...){standardGeneric("setStartDate")})
# Returns TraderID slot value.
#
# Args:
#   object : object of type Portfolio
# Returns:
#   TraderID

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


setGeneric("getEndDate", function(object,...){standardGeneric("getEndDate")})
# Returns start slot value.
#
# Args:
#   object : object of type Portfolio
# Returns:
#   startDate

setMethod("getEndDate",
          signature(object = "Portfolio"),
          function(object){
            return(object@end)
          }
)

setGeneric("setEndDate", function(object, end, ...){standardGeneric("setEndDate")})
# Returns TraderID slot value.
#
# Args:
#   object : object of type Portfolio
# Returns:
#   TraderID

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
