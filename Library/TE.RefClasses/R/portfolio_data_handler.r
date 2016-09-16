#' @include strategy_portfolio.r
NULL

####################################
#
# VirtualPortfolioDataHandler Class
#
####################################

#' Virtual S4 class handling Portfolio objects
#'
#' Class that is to be inherited by any objects
#' that will contain Portfolio or derived classes
#'
#' @slot portfolio object of class "Portfolio"


setClass(
  Class          = "VirtualPortfolioDataHandler",
  slots = c(
    portfolio = "Portfolio"
  ),
  contains = c("VIRTUAL")
)


#' Get portfolio stored in object
#'
#' Returns portfolio object of class "Portfolio"
#'
#' @param object object of class "VirtualPortfolioDataHandler"
#' @return \code{portfolio} object of class "Portfolio"
#' @export

setGeneric("getPortfolioDataObject", function(object){standardGeneric("getPortfolioDataObject")})

#' @describeIn getPortfolioDataObject
#' Get portfolio stored in object
#'
#' Returns portfolio object of class "Portfolio"
#'
#' @inheritParams getPortfolioDataObject
#' @return \code{portfolio} object of class "Portfolio"
#' @export
setMethod("getPortfolioDataObject",
          signature(object = "VirtualPortfolioDataHandler"),
          function(object){
            return(object@portfolio)
          }
)



#' Set portfolio object in object slot
#'
#' Public method to set portfolio slot with "Portfolio"
#' class object to be implemented in derived
#  classes where we want to allow for setting data.
#'
#' @param object object of class "VirtualPortfolioDataHandler"
#' @param portfolio object of class "Portfolio"
#' @return \code{object} object of class "VirtualPortfolioDataHandler"
#' @export

setGeneric("setPortfolioDataObject", function(object,portfolio){standardGeneric("setPortfolioDataObject")})



#' Set portfolio object in object slot
#'
#' Private method to set portfolio slot with "Portfolio"
#'
#' @rdname private_setPortfolioDataObject
#' @param object object of class "VirtualPortfolioDataHandler"
#' @param portfolio object of class "Portfolio"
#' @return \code{object} object of class "VirtualPortfolioDataHandler"

setGeneric(".setPortfolioDataObject", function(object,portfolio){standardGeneric(".setPortfolioDataObject")})

setMethod(".setPortfolioDataObject",
          signature(object = "VirtualPortfolioDataHandler", portfolio = "Portfolio"),
          function(object, portfolio){
              object@portfolio <- portfolio
            return(object)
          }
)
