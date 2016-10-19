#' @include dataset.r
#' @include common_dataplex.r
#' @include features_virtual_feature.r
NULL

# @exportClass TradeFeatureData
setClassUnion("TradeFeatureData",c("DataSet","NULL"))
setClass(
  Class          = "TradeFeature",
  representation = representation(
    data_set     = "TradeFeatureData",
    data_columns = "character", #Need to match the data columns on the incoming DataSet
    key_columns  = "character",
    data_store   = "character"
  ),
  prototype = prototype(
    key_columns = c('lInstrumentID','dtDateTime') #Need to match the key columns on the incoming DataSet
  ),
  contains = c("VirtualFeature")
)

#Perhaps add ability to pad data
setGeneric("updateData",function(object,trade_dates,instrument){standardGeneric("updateData")})
setMethod("updateData","TradeFeature",
          function(object,trade_dates,instrument){
            get_keys <- data.frame(a=c(instrument),b=trade_dates)
            colnames(get_keys) <- object@key_columns
            message(paste("Requesting feature data from store",object@data_store))
            object@data_set <- data_request(object@data_store,get_keys,object@data_columns)
            return(object)
          }
)

setGeneric("tearDownTradeFeature",function(object){standardGeneric("tearDownTradeFeature")})
setMethod("tearDownTradeFeature","TradeFeature",
          function(object){
            object <- tearDown(object)
            object@data_set <- NULL
            object@computation@compute <- NULL
            return(object)
          }
)

pass_thru <- function(compute_object){
  output <- getFeatureComputationInput(compute_object)
  compute_object <- tryCatch({
    .setFeatureComputationOutput(compute_object, input)
  }, error = function(cond) {

  })
  return(compute_object)
}

setClass(
  Class     = "PassThruComputation",
  prototype = prototype(
    compute = pass_thru
  ),
  contains = c("FeatureComputation")
)

setClass(
  Class    = "ContextFeature",
  contains = c("TradeFeature")
)

setClass(
  Class    = "OutcomeFeature",
  contains = c("TradeFeature")
)

setClass(
  Class    = "ControlFeature",
  contains = c("TradeFeature")
)
