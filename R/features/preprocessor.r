sourceTo("../features/virtual_aggregate_feature.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../lib/dataset.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/trade_factory.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/preprocessor_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

setClassUnion("PreprocessorOutput",c("DataSet","NULL"))
#Preprocessors should return a DataSet.
setClassUnion("PreprocessorInput",c("data.frame"))
setClass(
  Class      = "PreprocessorComputation",
  representation = representation(
    input    = "PreprocessorInput",
    compute  = "function",
    output   = "PreprocessorOutput",
    key_cols = "character",
    output_colnms= "character"
  )
)

setGeneric("setPreprocessorData",function(object,data){standardGeneric("setPreprocessorData")})
setMethod("setPreprocessorData", "PreprocessorComputation",
          function(object,data){
            object@input <- data
            return(object)
          }
)

setClass(
  Class      = "Preprocessor",
  prototype = prototype(
  	setup = setPreprocessorData
  ),
  contains = c("VirtualAggregateFeature")
)
setClassUnion("FeatureComputations",c("FeatureComputation","PreprocessorComputation"))

# FeatureGatherer
# Tabulates feature data

setClass(
  Class = "FeatureGathererComputation",
  prototype = prototype(
    compute = ftr_gther,
    key_cols= c('TradeID')
  ),
  contains = c("PreprocessorComputation")
)

setClass(
  Class = "FeatureGatherer",
  prototype = prototype(
    feature_mrg_on = list(TradeID="TradeID",TradeDate="DateTime"),
    gather_method  = getTradeInformation,
    feature_method = getTradeFeatures,
    computation    = new("FeatureGathererComputation")
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

setClass(
  Class = "FeatureGathererWithSummary",
  prototype = prototype(
    gather_method  = getPositionSummary
    ),
  contains = c('FeatureGatherer')
)

setClass(
  Class = "Fractiler",
  contains = c('FeatureGatherer')
)