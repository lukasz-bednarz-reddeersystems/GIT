#' @include dataset.r
#' @include common_trade_factory.r
#' @include features_virtual_aggregate_feature.r
NULL

# Leg position impact
# Get leg position impact from the trade object

ftr_unit <- function(compute_object){
	compute_object@output <- compute_object@input
	return(compute_object)
}

setClass(
	Class = "PassThroughComputation",
	prototype = prototype(
		compute = ftr_unit
	),
	contains = c("FeatureComputation")
)

setClass(
	Class = "PositionImpact",
	prototype = prototype(
		feature_mrg_on = list(TradeID="TradeID"),
		gather_method  = getTradeInformation,
		feature_method = getTradeFeatures,
		computation    = new("PassThroughComputation")
		),
	contains = c('Preprocessor')
)

setGeneric("gatherFeatures",function(object,warehouse,features){standardGeneric("gatherFeatures")})
setMethod("gatherFeatures","FeatureGatherer",
	function(object,warehouse,features){
			object@features <- features
			object <- gatherData(object,warehouse)
			object <- updateCompute(object)
			return(object)
		}
)
