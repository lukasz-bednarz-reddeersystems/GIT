sourceTo("../common/portfolio/portfolio.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../transformations/portfolio_transformation.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)


####################################
#
# PortfolioWithTransformations Class
#
####################################

setClass(
  Class = "PortfolioWithTransformations",
  slots = c(
    transformations = "list"
  ),
  prototype      = list(
    required_colnms = c('Date','InstrumentID','Weight')
  ),
  contains = c("Portfolio", "VIRTUAL")
)


setGeneric("getTransformations",function(object, ...){standardGeneric("getTransformations")})
# returns list of PortfolioTransformation attached to portfolio (data computed from existing portfolio data).
#
# Args:
#   object : object of type Portfolio
# Returns:
#   transformations : list with transformations attached to the object.
setMethod("getTransformations",
          signature(object = "PortfolioWithTransformations"),
          function(object){
            return(object@transformations)    
          }
)


setGeneric(".addTransformation",function(object, transformation, ...){standardGeneric(".addTransformation")})
# returns list of PortfolioTransformation attached to portfolio (data computed from existing portfolio data).
#
# Args:
#   object : object of type Portfolio
# Returns:
#   transformations : list with transformations attached to the object.
setMethod(".addTransformation",
          signature(object = "PortfolioWithTransformations", transformation = "PortfolioTransformation"),
          function(object, transformation){
            transf.name <- class(transformation)[[1]]
            transf.present <- getTransformations(object)
            object <- .setTransformations(object, union(transf.present, transf.name))
            return(object)    
          }
)

setGeneric(".setTransformations",function(object, transformations, ...){standardGeneric(".setTransformations")})
# returns list of PortfolioTransformation attached to portfolio (data computed from existing portfolio data).
#
# Args:
#   object : object of type Portfolio
# Returns:
#   transformations : list with transformations attached to the object.
setMethod(".setTransformations",
          signature(object = "PortfolioWithTransformations", transformations = "list"),
          function(object, transformations){
            object@transformations <- transformations
            return(object)    
          }
)




setGeneric("isTransformationAttached", function(object, transformation, ...){standardGeneric("isTransformationAttached")})
setMethod("isTransformationAttached",
          signature(object = "PortfolioWithTransformations", transformation = "PortfolioTransformation"),
          function(object, transformation){
            nme <- class(transformation)[[1]]
            if(nme %in% getTransformations(object)){
              return(TRUE)
            }
            else{
              return(FALSE)
            }
          }
)


setGeneric("attachTransformation",function(object,transformation, ...){standardGeneric("attachTransformation")})
# Attaches new columns with new transormations (data computed from existing portfolio data).
#
# Args:
#   object : object of type Portfolio
#   transformations: List of transformations to add.
# Returns:
#   Updated Portfolio object with transformations added.
setMethod("attachTransformation",
          signature(object = "PortfolioWithTransformations", transformation = "PortfolioTransformation"),
          function(object,transformation,replace=TRUE){
            
            if (isTransformationAttached(object, transformation) || !replace) {
              return(object)
            } else {
              
              transformation <- tryCatch({
                setPortfolio(transformation, object)
              }, error = function(cond){
                message(paste("Failed to attach", class(transformation)[[1]], "to", class(object)[[1]]))
                message(paste("In call to setPortfolio()"))
                stop(paste("Failed to attach", class(transformation)[[1]], "to", class(object)[[1]]))
              }
              )
              
              transformation <- tryCatch({
                triggerComputation(transformation)
              }, error = function(cond){
                message(paste("Failed to attach", class(transformation)[[1]], "to", class(object)[[1]]))
                message(paste("In call to triggerComputation()"))
                stop(paste("Failed to attach", class(transformation)[[1]], "to", class(object)[[1]]))
              }
              )
              
              if (isComputed(transformation)){
                object <- appendVariables(object, 
                                          getComputationOutput(transformation), 
                                          getComputedVariablesNames(transformation))
                
                object <- .addTransformation(object,transformation)
              } else {
                message(paste("Failed to attach", class(transformation)[[1]], "to", class(object)[[1]]))
                message(paste("Computation has not returned isComputed() status equal to TRUE."))
                stop(paste("Failed to attach", class(transformation)[[1]], "to", class(object)[[1]]))
              }
              
              return(object)
              
            }
          }
)

setGeneric("attachTransformations",function(object,transformations, ...){standardGeneric("attachTransformations")})
# Attaches new columns with new transormations (data computed from existing portfolio data).
#
# Args:
#   object : object of type Portfolio
#   transformations: List of transformations to add.
# Returns:
#   Updated Portfolio object with transformations added.
setMethod("attachTransformations",
          signature(object = "PortfolioWithTransformations", transformations = "list"),
          function(object,transformations,replace=TRUE){
            for(transf in transformations){ 
              object <- attachTransformation(object,transf, replace)
            }
            return(object)    
          }
)




