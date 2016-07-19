sourceTo("../common/portfolio/strategy_portfolio.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

####################################
#
# VirtualPortfolioDataHandler Class
#
####################################


setClass(
  Class          = "VirtualPortfolioDataHandler",
  slots = c(
    portfolio = "Portfolio"
  ),
  contains = c("VIRTUAL")
)


setGeneric("getPortfolioDataObject", function(object,...){standardGeneric("getPortfolioDataObject")})
# Returns portfolio object of class "StrategyPortfolio" if has been set up previously, otherwise NULL
#
# Args:
#   object : object of type "VirtualPortfolioDataHandler"
# Returns:
#   portfolio : "StrategyPortfolio" object

setMethod("getPortfolioDataObject",  
          signature(object = "VirtualPortfolioDataHandler"),
          function(object){
            return(object@portfolio)
          }
)

if (!isGenericS4("setPortfolioDataObject")){
  setGeneric("setPortfolioDataObject", function(object,portfolio, ...){standardGeneric("setPortfolioDataObject")})
}
# Public method to set portfolio slot with "StrategyPortfolio" class object to be implemented in derived
# classes where we want to allow for setting data.
#
# Args:
#   object : object of type "VirtualPortfolioDataHandler"
#   portfolio : object of class "StrategyPortfolio" 
# Returns:
#   object : object of type "VirtualPortfolioDataHandler"




setGeneric(".setPortfolioDataObject", function(object,portfolio, ...){standardGeneric(".setPortfolioDataObject")})
# Private method to set portfolio slot with "StrategyPortfolio" class object
#
# Args:
#   object : object of type "VirtualPortfolioDataHandler"
#   portfolio : object of class "StrategyPortfolio" 
# Returns:
#   object : object of type "VirtualPortfolioDataHandler"

setMethod(".setPortfolioDataObject",  
          signature(object = "VirtualPortfolioDataHandler", portfolio = "Portfolio"),
          function(object, portfolio){
              object@portfolio <- portfolio
            return(object)
          }
)
