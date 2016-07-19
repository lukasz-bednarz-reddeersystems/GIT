sourceTo("../common/global_configs.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../lib/functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/trade_feature.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/feature_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

#ACTIVITY INTO TRADE

setClass(
  Class     = "ActivityIntoTradeComputation",
  representation = representation(
    dates   = "Date",
    window  = "integer"
  ),
  prototype = prototype(
    compute = act_in,
    window  = feature_defaults@default_window
  ), 
  contains = c("FeatureComputation")
)

setClass(
  Class          = "ActivityIntoTrade",
  prototype      = prototype(
    data_store   = "",
    data_columns = c(),
    computation  = new("ActivityIntoTradeComputation")
  ),
  contains  = c("ControlFeature")
)
setGeneric("updateActivityIntoTrade",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateActivityIntoTrade")})
setMethod("updateActivityIntoTrade","ActivityIntoTrade",
           function(object,trade_dates,instrument,strategy,daily_data){
            object <- p_size(object,trade_dates,instrument,strategy,daily_data)
            return(object)
           }
)

#ACTIVITY OUTOF TRADE

setClass(
  Class     = "ActivityOutofTradeComputation",
  representation = representation(
    dates   = "Date",
    window  = "integer"
  ),
  prototype = prototype(
    compute = act_out,
    window  = feature_defaults@default_window
  ), 
  contains = c("FeatureComputation")
)

setClass(
  Class          = "ActivityOutofTrade",
  prototype      = prototype(
    data_store   = "",
    data_columns = c(),
    computation  = new("ActivityOutofTradeComputation")
  ),
  contains  = c("ControlFeature")
)
setGeneric("updateActivityOutofTrade",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateActivityOutofTrade")})
setMethod("updateActivityOutofTrade","ActivityOutofTrade",
           function(object,trade_dates,instrument,strategy,daily_data){
            object <- p_size(object,trade_dates,instrument,strategy,daily_data)
            return(object)
           }
)

#AGE 

setClass(
  Class      = "Age",
  prototype  = prototype(
    data_store   = "ext_pos_datastore",
    data_columns = c("Age","InstrumentID"),
    computation  = new("PassThruComputation"),
    key_columns  = c('Strategy','Date')
  ),
  contains = c("ControlFeature")
)
setGeneric("updateAge",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateAge")})
setMethod("updateAge","Age",
           function(object,trade_dates,instrument,strategy,daily_data){
              object <- updateData(object,trade_dates,strategy)
              data <- object@data_set@data
              data <- subset(data,data$InstrumentID==instrument)
              data <- data[c("Date","Age")]
              object@computation <- setComputationData(object@computation,data)
              return(object)
           }
)

#TRADE AGE

setClass(
  Class     = "TradeAgeComputation",
  representation = representation(
    dates   = "Date",
    window  = "integer"
  ),
  contains = c("FeatureComputation"),
  prototype = prototype(
    compute = age,
    window  = as.integer(40)
  )
)

setClass(
  Class          = "TradeAge",
  prototype      = prototype(
    data_store   = "",
    data_columns = c(),
    computation  = new("TradeAgeComputation"),
    key_columns = c('InstrumentID','DateTime')
  ),
  contains  = c("ControlFeature")
)

setGeneric("updateTradeAge",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateTradeAge")})
setMethod("updateTradeAge","TradeAge",
          function(object,trade_dates,instrument,strategy,daily_data){
            object@computation@dates <- trade_dates
            object@computation <- setComputationData(object@computation,daily_data@data)
            return(object)
          }
)

#Market value

setClass(
  Class      = "MarketValue",
  prototype  = prototype(
    data_store   = "",
    data_columns = c(),
    computation  = new("PassThruComputation"),
    key_columns = c('InstrumentID','DateTime')
  ),
  contains = c("ControlFeature")
)
setGeneric("updateMarketValue",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateMarketValue")})
setMethod("updateMarketValue","MarketValue",
           function(object,trade_dates,instrument,strategy,daily_data){
              data <- daily_data@data[c('DateTime','MarketValue')]
              object@computation <- setComputationData(object@computation,data)
              return(object)
           }
)
