sourceTo("../transformations/virtual_transformation.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../transformations/transformation_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

setClassUnion("PortfolioTransformationData",c("Portfolio","NULL"))


################################################
#
# Virtual Portfollio Transformation Class
#
# This is base class handling Tranformation
# on Portfolio object
################################################

setClass(
  Class          = "PortfolioTransformation",
  slots = c(
    portfolio     = "PortfolioTransformationData"
  ),
  prototype = prototype(
    required_colnms = c('Date','InstrumentID','Weight') 
  ),
  contains = c("VirtualTransformation", "VIRTUAL")
)


setMethod("initialize",
          signature(.Object = "PortfolioTransformation"),
          function(.Object, portfolio ){
          if (!is(portfolio, "Portfolio") && !is(portfolio,"NULL")){
            message(paste("Error when initializing", class(.Object)[[1]], "class."))
            message(paste("Missing or invalid portfolio argument."))
            message(paste("PortfolioTransformation and derived classes only accept \"portfolio\""))
            message(paste("argument extending Portfolio class."))
            stop("Invalid Class initialize() argument \"portfolio\"")
          }
          if (is(portfolio, "Portfolio")) {
            .Object <- setPortfolio(.Object, portfolio)
          }
            
          return(.Object)
})


setGeneric("getPortfolio",function(object,...){standardGeneric("getPortfolio")})
setMethod("getPortfolio",
          signature(object = "PortfolioTransformation"),
          function(object){
            
            return(object@portfolio)           
          }
)

setGeneric("setPortfolio",function(object, portfolio, ...){standardGeneric("setPortfolio")})
setMethod("setPortfolio",
          signature(object = "PortfolioTransformation", portfolio = "Portfolio"),
          function(object, portfolio){
            if (!is(portfolio, "Portfolio")){
              message(paste("Error when calling setPortfolio() on ", class(object)[[1]], "class."))
              message(paste("Missing or invalid portfolio argument."))
              message(paste("PortfolioTransformation and derived classes only accept \"portfolio\""))
              message(paste("argument of class extending Portfolio class."))
              stop("Invalid setPortfolio(object, portfolio) argument \"portfolio\"")
            }
            
            object <- setComputationInput(object, getReferenceData(portfolio))
            object@portfolio <- portfolio
            
            return(object)           
          }
)

################################################
#
# DaysSinceLastFlatTransformationn Class
#
################################################

setClass(
  Class = "DaysSinceLastFlatTransformationComputation",
  prototype      = list(
    required_colnms= c('Date','InstrumentID','Weight'),
    computed_colnms = c("DaysSinceLastFlat"),
    compute = days_since_last_flat
    
  ),
  contains = c( "VirtualTransformationComputation")
)

setClass(
  Class          = "DaysSinceLastFlatTransformation",
  prototype = prototype(
    required_colnms = c('Date','InstrumentID','Weight'),
    computed_colnms = c("DaysSinceLastFlat"),
    computation = new("DaysSinceLastFlatTransformationComputation")
  ),
  contains = c("PortfolioTransformation")
)
