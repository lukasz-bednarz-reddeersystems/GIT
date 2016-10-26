#' @include global_configs.r
#' @include functions.r
#' @include features_trade_feature.r
#' @include features_feature_functions.r
NULL


#CUMULATIVE PRICE MOVE INTO TRADE

setClass(
  Class     = "CompoundReturnIntoComputation",
   representation = representation(
    dates        = "Date",
    window       = "integer"
  ),
  prototype = prototype(
    compute = rtn_in,
    window  = feature_defaults@default_window
  ),
  contains = c("FeatureComputation")
)

setClass(
  Class          = "CompoundReturnInto",
  prototype      = prototype(
    data_store   = "",
    data_columns = c(),
    computation  = new("CompoundReturnIntoComputation")
  ),
  contains  = c("ContextFeature")
)
setGeneric("updateCompoundReturnInto",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateCompoundReturnInto")})
setMethod("updateCompoundReturnInto","CompoundReturnInto",
           function(object,trade_dates,instrument,strategy,daily_data){
            object@computation@dates <- trade_dates
            object@computation <- setComputationData(object@computation,daily_data@data)
            return(object)
           }
)

#CUMULATIVE PRICE MOVE OUTOF TRADE

setClass(
  Class     = "CompoundReturnOutofComputation",
  representation = representation(
    dates        = "Date",
    window       = "integer"
  ),
  prototype = prototype(
    compute = rtn_out,
    window  = feature_defaults@default_window
  ),
  contains = c("FeatureComputation")
)

setClass(
  Class          = "CompoundReturnOutof",
  prototype      = prototype(
    data_store   = "",
    data_columns = c(),
    computation  = new("CompoundReturnOutofComputation")
  ),
  contains  = c("ContextFeature")
)
setGeneric("updateCompoundReturnOutof",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateCompoundReturnOutof")})
setMethod("updateCompoundReturnOutof","CompoundReturnOutof",
           function(object,trade_dates,instrument,strategy,daily_data){
            object@computation@dates <- trade_dates
            object@computation <- setComputationData(object@computation,daily_data@data)
            return(object)
           }
)

#PnL INTO TRADE

setClass(
  Class     = "PnLIntoComputation",
  representation = representation(
    dates        = "Date",
    window       = "integer"
  ),
  prototype = prototype(
    compute = pnl_in,
    window  = feature_defaults@default_window
  ),
  contains = c("FeatureComputation")
)

setClass(
  Class          = "PnLInto",
  prototype      = prototype(
    data_store   = "",
    data_columns = c(),
    computation  = new("PnLIntoComputation")
  ),
  contains  = c("ContextFeature")
)
setGeneric("updatePnLInto",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updatePnLInto")})
setMethod("updatePnLInto","PnLInto",
           function(object,trade_dates,instrument,strategy,daily_data){
            object@computation@dates <- trade_dates
            object@computation <- setComputationData(object@computation,daily_data@data)
            return(object)
           }
)

#Positive PnL into/outof trade

setClass(
  Class     = "PtvePnLIntoComputation",
  representation = representation(
    dates        = "Date",
    window       = "integer"
  ),
  prototype = prototype(
    compute = ptv_pnl_in,
    window  = feature_defaults@default_window
  ),
  contains = c("FeatureComputation")
)

setClass(
  Class          = "PtvePnLInto",
  prototype      = prototype(
    data_store   = "",
    data_columns = c(),
    computation  = new("PtvePnLIntoComputation")
  ),
  contains  = c("ContextFeature")
)
setGeneric("updatePtvePnLInto",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updatePtvePnLInto")})
setMethod("updatePtvePnLInto","PtvePnLInto",
           function(object,trade_dates,instrument,strategy,daily_data){
            object@computation@dates <- trade_dates
            object@computation <- setComputationData(object@computation,daily_data@data)
            return(object)
           }
)

setClass(
  Class     = "PtvePnLOutofComputation",
  representation = representation(
    dates        = "Date",
    window       = "integer"
  ),
  prototype = prototype(
    compute = ptv_pnl_out,
    window  = feature_defaults@default_window
  ),
  contains = c("FeatureComputation")
)

setClass(
  Class          = "PtvePnLOutof",
  prototype      = prototype(
    data_store   = "",
    data_columns = c(),
    computation  = new("PtvePnLOutofComputation")
  ),
  contains  = c("ContextFeature")
)
setGeneric("updatePtvePnLOutof",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updatePtvePnLOutof")})
setMethod("updatePtvePnLOutof","PtvePnLOutof",
           function(object,trade_dates,instrument,strategy,daily_data){
            object@computation@dates <- trade_dates
            object@computation <- setComputationData(object@computation,daily_data@data)
            return(object)
           }
)

#PnL OUT OF TRADE

setClass(
  Class     = "PnLOutofComputation",
  representation = representation(
    dates        = "Date",
    window       = "integer"
  ),
  prototype = prototype(
    compute = pnl_out,
    window  = feature_defaults@default_window
  ),
  contains = c("FeatureComputation")
)

setClass(
  Class          = "PnLOutof",
  prototype      = prototype(
    data_store   = "",
    data_columns = c(),
    computation  = new("PnLOutofComputation")
  ),
  contains  = c("OutcomeFeature")
)
setGeneric("updatePnLOutof",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updatePnLOutof")})
setMethod("updatePnLOutof","PnLOutof",
           function(object,trade_dates,instrument,strategy,daily_data){
            object@computation@dates <- trade_dates
            object@computation <- setComputationData(object@computation,daily_data@data)
            return(object)
           }
)

#PRICE INFLUENCE

setClass(
  Class     = "PriceInfluenceComputation",
  prototype = prototype(
    compute = prc_inf
  ),
  contains = c("FeatureComputation")
)

setClass(
  Class          = "PriceInfluence",
  prototype      = prototype(
    data_store   = "",
    data_columns = c(),
    computation  = new("PriceInfluenceComputation")
  ),
  contains  = c("ControlFeature")
)
setGeneric("updatePriceInfluence",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updatePriceInfluence")})
setMethod("updatePriceInfluence","PriceInfluence",
           function(object,trade_dates,instrument,strategy,daily_data){
            object@computation <- setComputationData(object@computation,daily_data@data)
            return(object)
           }
)

#POSITION RETURN INTO TRADE

setClass(
  Class     = "PsnReturnInComputation",
  representation = representation(
    dates   = "Date"
  ),
  prototype = prototype(
    compute = psn_rtn_in,
    window  = feature_defaults@default_window
  ),
  contains = c("FeatureComputation")
)

setClass(
  Class          = "PsnReturnIn",
  prototype      = prototype(
    data_store   = "",
    data_columns = c(),
    computation  = new("PsnReturnInComputation"),
    key_columns = c('InstrumentID','DateTime')
  ),
  contains  = c("ContextFeature")
)

setGeneric("updatePsnReturnIn",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updatePsnReturnIn")})
setMethod("updatePsnReturnIn","PsnReturnIn",
           function(object,trade_dates,instrument,strategy,daily_data){
            data <- psn_update(object,trade_dates,instrument,strategy,daily_data)
            object@computation@dates <- trade_dates
            object@computation <- setComputationData(object@computation,data)
            return(object)
           }
)

# POSITION RETURN OUT OF TRADE

setClass(
  Class     = "PsnReturnOutComputation",
  representation = representation(
    dates   = "Date"
  ),
  prototype = prototype(
    compute = psn_rtn_out,
    window  = feature_defaults@default_window
  ),
  contains = c("FeatureComputation")
)

setClass(
  Class          = "PsnReturnOut",
  prototype      = prototype(
    data_store   = "",
    data_columns = c(),
    computation  = new("PsnReturnOutComputation"),
    key_columns = c('InstrumentID','DateTime')
  ),
  contains  = c("OutcomeFeature")
)

setGeneric("updatePsnReturnOut",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updatePsnReturnOut")})
setMethod("updatePsnReturnOut","PsnReturnOut",
          function(object,trade_dates,instrument,strategy,daily_data){
            data <- psn_update(object,trade_dates,instrument,strategy,daily_data)
            object@computation@dates <- trade_dates
            object@computation <- setComputationData(object@computation,data)
            return(object)
          }
)

setClass(
  Class     = "PsnReturnOutFullComputation",
  representation = representation(
    dates   = "Date"
  ),
  prototype = prototype(
    compute = psn_rtn_out,
    window  = feature_defaults@default_window_long
  ),
  contains = c("FeatureComputation")
)

setClass(
  Class          = "PsnReturnOutFull",
  prototype      = prototype(
    data_store   = "",
    data_columns = c(),
    computation  = new("PsnReturnOutFullComputation"),
    key_columns = c('InstrumentID','DateTime')
  ),
  contains  = c("OutcomeFeature")
)

setGeneric("updatePsnReturnOutFull",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updatePsnReturnOutFull")})
setMethod("updatePsnReturnOutFull","PsnReturnOutFull",
          function(object,trade_dates,instrument,strategy,daily_data){
            data <- psn_update(object,trade_dates,instrument,strategy,daily_data)
            object@computation@dates <- trade_dates
            object@computation <- setComputationData(object@computation,data)
            return(object)
          }
)

#Stock return volatility into trade

setClass(
  Class     = "VolIntoComputation",
  representation = representation(
    dates   = "Date",
    window  = "integer"
  ),
  contains = c("FeatureComputation"),
  prototype = prototype(
    compute = vol_in,
    window  = feature_defaults@default_window_long
  )
)

setClass(
  Class          = "VolInto",
  prototype      = prototype(
    data_store   = "",
    data_columns = c(),
    computation  = new("VolIntoComputation"),
    key_columns = c('InstrumentID','DateTime')
  ),
  contains  = c("ContextFeature")
)

setGeneric("updateVolInto",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateVolInto")})
setMethod("updateVolInto","VolInto",
          function(object,trade_dates,instrument,strategy,daily_data){
            object@computation@dates <- trade_dates
            object@computation <- setComputationData(object@computation,daily_data@data)
            return(object)
          }
)

#Stock rtn volatility out of trade

setClass(
  Class     = "VolOutofComputation",
  representation = representation(
    dates   = "Date",
    window  = "integer"
  ),
  contains = c("FeatureComputation"),
  prototype = prototype(
    compute = vol_out,
    window  = feature_defaults@default_window_long
  )
)

setClass(
  Class          = "VolOutof",
  prototype      = prototype(
    data_store   = "",
    data_columns = c(),
    computation  = new("VolOutofComputation"),
    key_columns = c('InstrumentID','DateTime')
  ),
  contains  = c("ContextFeature")
)

setGeneric("updateVolOutof",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateVolOutof")})
setMethod("updateVolOutof","VolOutof",
          function(object,trade_dates,instrument,strategy,daily_data){
            object@computation@dates <- trade_dates
            object@computation <- setComputationData(object@computation,daily_data@data)
            return(object)
          }
)

#Stock rtn skewness into trade

setClass(
  Class     = "SkewIntoComputation",
  representation = representation(
    dates   = "Date",
    window  = "integer"
  ),
  contains = c("FeatureComputation"),
  prototype = prototype(
    compute = skew_in,
    window  = feature_defaults@default_window_long
  )
)

setClass(
  Class          = "SkewInto",
  prototype      = prototype(
    data_store   = "",
    data_columns = c(),
    computation  = new("SkewIntoComputation"),
    key_columns = c('InstrumentID','DateTime')
  ),
  contains  = c("ContextFeature")
)

setGeneric("updateSkewInto",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateSkewInto")})
setMethod("updateSkewInto","SkewInto",
          function(object,trade_dates,instrument,strategy,daily_data){
            object@computation@dates <- trade_dates
            object@computation <- setComputationData(object@computation,daily_data@data)
            return(object)
          }
)

#Stock rtn skewness out of trade

setClass(
  Class     = "SkewOutofComputation",
  representation = representation(
    dates   = "Date",
    window  = "integer"
  ),
  contains = c("FeatureComputation"),
  prototype = prototype(
    compute = skew_out,
    window  = feature_defaults@default_window_long
  )
)

setClass(
  Class          = "SkewOutof",
  prototype      = prototype(
    data_store   = "",
    data_columns = c(),
    computation  = new("SkewOutofComputation"),
    key_columns = c('InstrumentID','DateTime')
  ),
  contains  = c("ContextFeature")
)

setGeneric("updateSkewOutof",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateSkewOutof")})
setMethod("updateSkewOutof","SkewOutof",
          function(object,trade_dates,instrument,strategy,daily_data){
            object@computation@dates <- trade_dates
            object@computation <- setComputationData(object@computation,daily_data@data)
            return(object)
          }
)

#Close price

setClass(
  Class     = "ClosePriceComputation",
  contains = c("PassThruComputation"),
)

setClass(
  Class      = "ClosePrice",
  prototype  = prototype(
    data_store   = "",
    data_columns = c(),
    computation  = new("ClosePriceComputation"),
    key_columns = c('InstrumentID','DateTime')
  ),
  contains = c("ContextFeature")
)
setGeneric("updateClosePrice",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateClosePrice")})
setMethod("updateClosePrice","ClosePrice",
           function(object,trade_dates,instrument,strategy,daily_data){
              data <- daily_data@data[c('DateTime','ClosePrice')]

              #for some stock there are multiple entries generated for the same day

              object@computation <- setComputationData(object@computation,data)
              return(object)
           }
)

#Prior day's close price

setClass(
  Class     = "PriorClosePriceComputation",
  contains = c("PassThruComputation")
)

setClass(
  Class      = "PriorClosePrice",
  prototype  = prototype(
    data_store   = "",
    data_columns = c(),
    computation  = new("PriorClosePriceComputation"),
    key_columns = c('InstrumentID','DateTime')
  ),
  contains = c("ContextFeature")
)
setGeneric("updatePriorClosePrice",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updatePriorClosePrice")})
setMethod("updatePriorClosePrice","PriorClosePrice",
           function(object,trade_dates,instrument,strategy,daily_data){
              data <- unique(daily_data@data[c('DateTime','ClosePrice')])

              if (nrow(data) > 1){
                data$ClosePrice <- c(NA, data$ClosePrice[1:(nrow(data) -1)])
              } else {
                data$ClosePrice <- NA
              }

              object@computation <- setComputationData(object@computation,data)
              return(object)
           }
)

#HIT1D

setClass(
  Class     = "Hit1DComputation",
  representation = representation(
    dates   = "Date"
  ),
  contains = c("FeatureComputation"),
  prototype = prototype(
    compute = pl_hit
  )
)

setClass(
  Class      = "Hit1D",
  prototype  = prototype(
    data_store   = "",
    data_columns = c(),
    computation  = new("Hit1DComputation"),
    key_columns = c('InstrumentID','DateTime')
  ),
  contains = c("OutcomeFeature")
)
setGeneric("updateHit1D",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateHit1D")})
setMethod("updateHit1D","Hit1D",
           function(object,trade_dates,instrument,strategy,daily_data){
              object@computation@dates <- trade_dates
              object@computation <- setComputationData(object@computation,daily_data@data)
              return(object)
           }
)

#Today PL

setClass(
  Class     = "TodayPLComputation",
  contains = c("PassThruComputation")
)

setClass(
  Class      = "TodayPL",
  prototype  = prototype(
    data_store   = "",
    data_columns = c(),
    computation  = new("TodayPLComputation"),
    key_columns = c('InstrumentID','DateTime')
  ),
  contains = c("ContextFeature")
)
setGeneric("updateTodayPL",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateTodayPL")})
setMethod("updateTodayPL","TodayPL",
           function(object,trade_dates,instrument,strategy,daily_data){
              data <- daily_data@data[c('DateTime','TodayPL')]
              object@computation <- setComputationData(object@computation,data)
              return(object)
           }
)

#Trailing average of the ClosePrice

setClass(
  Class     = "PriceMavgComputation",
  representation = representation(
    dates   = "Date",
    window  = 'integer'
  ),
  contains = c("FeatureComputation"),
  prototype = prototype(
    compute = mavg_price,
    window  = feature_defaults@default_window_long
  )
)

setClass(
  Class      = "PriceMavg",
  prototype  = prototype(
    data_store   = "",
    data_columns = c(),
    computation  = new("PriceMavgComputation"),
    key_columns = c('InstrumentID','DateTime')
  ),
  contains = c("ContextFeature")
)
setGeneric("updatePriceMavg",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updatePriceMavg")})
setMethod("updatePriceMavg","PriceMavg",
           function(object,trade_dates,instrument,strategy,daily_data){
              data <- daily_data@data[c('DateTime','ClosePrice')]
              object@computation@dates <- trade_dates
              object@computation <- setComputationData(object@computation,data)
              return(object)
           }
)

#Stoploss
setClass(
  Class    = "StopLossComputation",
  contains = c("PassThruComputation")
)

setClass(
  Class      = "StopLoss",
  prototype  = prototype(
    data_store   = "",
    data_columns = c(),
    computation  = new("StopLossComputation")
    #key_columns = c('InstrumentID','DateTime')
  ),
  contains = c("ControlFeature")
)
setGeneric("updateStopLoss",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateStopLoss")})
setMethod("updateStopLoss","StopLoss",
           function(object,trade_dates,instrument,strategy,daily_data){
              data <- tryCatch({daily_data@data[c('DateTime','StopLoss')]},error=function(cond){return(data.frame(DateTime=NA,StopLoss=NA))})
              object@computation <- setComputationData(object@computation,data)
              return(object)
           }
)


#Profit target
setClass(
  Class    = "ProfitTargetComputation",
  contains = c("PassThruComputation")
)

setClass(
  Class      = "ProfitTarget",
  prototype  = prototype(
    data_store   = "",
    data_columns = c(),
    computation  = new("ProfitTargetComputation")
    #key_columns = c('InstrumentID','DateTime')
  ),
  contains = c("ControlFeature")
)
setGeneric("updateProfitTarget",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateProfitTarget")})
setMethod("updateProfitTarget","ProfitTarget",
           function(object,trade_dates,instrument,strategy,daily_data){
              data <- tryCatch({daily_data@data[c('DateTime','ProfitTarget')]},error=function(cond){return(data.frame(DateTime=NA,StopLoss=NA))})
              object@computation <- setComputationData(object@computation,data)
              return(object)
           }
)

#Mid price
setClass(
  Class    = "MidOnEntryComputation",
  contains = c("PassThruComputation")
)

setClass(
  Class      = "MidOnEntry",
  prototype  = prototype(
    data_store   = "dealing_datastore",
    data_columns = c("dblMidOnEntry","lInstrumentID"),
    computation  = new("MidOnEntryComputation"),
    key_columns  = c("lTraderID","dtTradeDate")
  ),
  contains = c("ContextFeature")
)
setGeneric("updateMidOnEntry",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateMidOnEntry")})
setMethod("updateMidOnEntry","MidOnEntry",
           function(object,trade_dates,instrument,strategy,daily_data){
              #parse trader out from the strategy
              tr <- substr(strategy,1,2)
              if(length(tr)>0 && is.na(tr) == FALSE)
              {
                if(tr == 'JS'){
                  trader <- 11
                }
                else if(tr == 'BA'){
                  trader <- 70
                }
                else if(tr == 'DK'){
                  trader <- 101
                }
                else{
                  message(paste('Feature MidOnEntry encountered an unmapped strategy',strategy))
                  trader <- NA
                }
              }
              else{
                message(paste("Could not parse",strategy))
                trader <- NA
              }
              if(is.na(trader)==FALSE){
                object <- updateData(object,trade_dates,trader)
                data <- object@data_set@data
                data <- subset(data,data$lInstrumentID==instrument)
                data <- data[c("dtTradeDate","dblMidOnEntry")]
                colnames(data) <- c("Date","MidOnEntry")

                if (!is.null(data) && is(data , "data.frame") && nrow(data) > 1){
                  data <- aggregate(MidOnEntry ~ Date,
                                    data = data,
                                    function(x){mean(x, na.rm = TRUE)},
                                    na.action = NULL)
                }
                object@computation <- setComputationData(object@computation,data)
              }
              return(object)
           }
)

