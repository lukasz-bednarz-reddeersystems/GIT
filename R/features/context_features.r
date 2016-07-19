sourceTo("../common/global_configs.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../lib/functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/trade_feature.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/feature_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

setClass(
  Class     = "DistanceToEarningsComputation",
  prototype = prototype(
    compute = earn_prox
  ), 
  contains = c("FeatureComputation")
)

setClass(
  Class          = "DistanceToEvent",
  contains       = c("ContextFeature")
) 
setGeneric("updateDistanceToEvent",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateDistanceToEvent")})
setMethod("updateDistanceToEvent","DistanceToEvent",
           function(object,trade_dates,instrument,strategy,daily_data){
            #Updates the local feature computation data 
            object <- updateData(object,trade_dates,instrument)
            object@computation <- setComputationData(object@computation,object@data_set@data[c("dtDateTime",object@data_columns)])
            return(object)
           }
)

#EARNINGS PROXIMITY

setClass(
  Class      = "DistanceToEarnings",
  prototype  = prototype(
    data_store   = "factor_datastore",
    data_columns = c("rDaysSinceLastResults",
                     "rDaysToNextResults"),
    computation  = new("DistanceToEarningsComputation")
  ),
  contains = c("DistanceToEvent")
)
setGeneric("updateDistanceToEarnings",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateDistanceToEarnings")})
setMethod("updateDistanceToEarnings","DistanceToEarnings",
           function(object,trade_dates,instrument,strategy,daily_data){
              return(updateDistanceToEvent(object,trade_dates,instrument,strategy,daily_data))
           }
)

# DAYS SINCE LAST RESULTS

setClass(
  Class      = "DaysSinceLastResults",
  prototype  = prototype(
    data_store   = "factor_datastore",
    data_columns = c("rDaysSinceLastResults"),
    computation  = new("PassThruComputation")
  ),
  contains = c("ContextFeature")
)
setGeneric("updateDaysSinceLastResults",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateDaysSinceLastResults")})
setMethod("updateDaysSinceLastResults","DaysSinceLastResults",
           function(object,trade_dates,instrument,strategy,daily_data){
              object <- updateData(object,trade_dates,instrument)
              object@computation <- setComputationData(object@computation,object@data_set@data[c("dtDateTime",object@data_columns)])
              return(object)
           }
)

# DAYS TO NEXT RESULTS

setClass(
  Class      = "DaysToNextResults",
  prototype  = prototype(
    data_store   = "factor_datastore",
    data_columns = c("rDaysToNextResults"),
    computation  = new("PassThruComputation")
  ),
  contains = c("ContextFeature")
)
setGeneric("updateDaysToNextResults",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateDaysToNextResults")})
setMethod("updateDaysToNextResults","DaysToNextResults",
           function(object,trade_dates,instrument,strategy,daily_data){
              object <- updateData(object,trade_dates,instrument)
              object@computation <- setComputationData(object@computation,object@data_set@data[c("dtDateTime",object@data_columns)])
              return(object)
           }
)

#DAYS SINCE SECONDARY PLACING

setClass(
  Class      = "DaysSinceSecondaryPlacing",
  prototype  = prototype(
    data_store   = "factor_datastore",
    data_columns = c("rDaysSinceLastSecondaryPlacing"),
    computation  = new("PassThruComputation")
  ),
  contains = c("ContextFeature")
)
setGeneric("updateDaysSinceSecondaryPlacing",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateDaysSinceSecondaryPlacing")})
setMethod("updateDaysSinceSecondaryPlacing","DaysSinceSecondaryPlacing",
           function(object,trade_dates,instrument,strategy,daily_data){
              object <- updateData(object,trade_dates,instrument)
              object@computation <- setComputationData(object@computation,object@data_set@data[c("dtDateTime",object@data_columns)])
              return(object)
           }
)

#DAYS TO SECONDARY PLACING

setClass(
  Class      = "DaysToSecondaryPlacing",
  prototype  = prototype(
    data_store   = "factor_datastore",
    data_columns = c("rDaysToNextSecondaryPlacing"),
    computation  = new("PassThruComputation")
  ),
  contains = c("ContextFeature")
)
setGeneric("updateDaysToSecondaryPlacing",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateDaysToSecondaryPlacing")})
setMethod("updateDaysToSecondaryPlacing","DaysToSecondaryPlacing",
           function(object,trade_dates,instrument,strategy,daily_data){
              object <- updateData(object,trade_dates,instrument)
              object@computation <- setComputationData(object@computation,object@data_set@data[c("dtDateTime",object@data_columns)])
              return(object)
           }
)

#DAYS SINCE PRIMARY PLACING

setClass(
  Class      = "DaysSincePrimaryPlacing",
  prototype  = prototype(
    data_store   = "factor_datastore",
    data_columns = c("rDaysSinceLastPrimaryPlacing"),
    computation  = new("PassThruComputation")
  ),
  contains = c("ContextFeature")
)
setGeneric("updateDaysSincePrimaryPlacing",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateDaysSincePrimaryPlacing")})
setMethod("updateDaysSincePrimaryPlacing","DaysSincePrimaryPlacing",
           function(object,trade_dates,instrument,strategy,daily_data){
              object <- updateData(object,trade_dates,instrument)
              object@computation <- setComputationData(object@computation,object@data_set@data[c("dtDateTime",object@data_columns)])
              return(object)
           }
)

#DAYS TO PRIMARY PLACING

setClass(
  Class      = "DaysToPrimaryPlacing",
  prototype  = prototype(
    data_store   = "factor_datastore",
    data_columns = c("rDaysToNextPrimaryPlacing"),
    computation  = new("PassThruComputation")
  ),
  contains = c("ContextFeature")
)
setGeneric("updateDaysToPrimaryPlacing",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateDaysToPrimaryPlacing")})
setMethod("updateDaysToPrimaryPlacing","DaysToPrimaryPlacing",
           function(object,trade_dates,instrument,strategy,daily_data){
              object <- updateData(object,trade_dates,instrument)
              object@computation <- setComputationData(object@computation,object@data_set@data[c("dtDateTime",object@data_columns)])
              return(object)
           }
)

#EPS REVISION FY1

setClass(
  Class     = "EPSRevisionComputation",
  prototype = prototype(
    compute = eps_rev
  ), 
  contains = c("FeatureComputation")
)

setClass(
  Class          = "EPSRevision",
  prototype      = prototype(
    data_store   = "factor_datastore",
    data_columns = c("rNumberOfEPSUpgradesSinceNumbersFY1",
                     "rNumberOfEPSDowngradesSinceNumbersFY1"),
    computation  = new("EPSRevisionComputation")
  ),
  contains  = c("ContextFeature")
)
setGeneric("updateEPSRevision",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateEPSRevision")})
setMethod("updateEPSRevision","EPSRevision",
           function(object,trade_dates,instrument,strategy,daily_data){
            #Updates the local feature computation data 
            object <- updateData(object,trade_dates,instrument)
            object@computation <- setComputationData(object@computation,object@data_set@data[c("dtDateTime",object@data_columns)])
            return(object)
           }
)

#MARKET CAP

setClass(
  Class     = "MarketCapComputation",
  representation = representation(
    dates   = "Date"
  ),
  prototype = prototype(
    compute = mkt_cap
  ), 
  contains = c("FeatureComputation")
)

setClass(
  Class          = "MarketCap",
  prototype      = prototype(
    data_store   = "",
    data_columns = c(),
    computation  = new("MarketCapComputation"),
    key_columns = c('InstrumentID','DateTime')
  ),
  contains  = c("ContextFeature")
)

setGeneric("updateMarketCap",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateMarketCap")})
setMethod("updateMarketCap","MarketCap",
           function(object,trade_dates,instrument,strategy,daily_data){
            object@computation@dates <- trade_dates
            object@computation <- setComputationData(object@computation,daily_data@data)
            return(object)
           }
)

#Trade input direction

setClass(
  Class      = "InputDirection",
  prototype  = prototype(
    data_store   = "dealing_datastore",
    data_columns = c("sInputDirection","lInstrumentID"),
    computation  = new("PassThruComputation"),
    key_columns  = c("lTraderID","dtTradeDate")
  ),
  contains = c("ContextFeature")
)
setGeneric("updateInputDirection",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateInputDirection")})
setMethod("updateInputDirection","InputDirection",
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
                  message(paste('Feature InputDirection encountered an unmapped strategy',strategy))
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
                data <- data[c("dtTradeDate","sInputDirection")]
                colnames(data) <- c("Date","InputDirection")
                object@computation <- setComputationData(object@computation,data)
              }
              return(object)
           }
)

#5d ADV

setClass(
  Class      = "AvgDailyValTraded5Day",
  prototype  = prototype(
    data_store   = "dynamic_factor_datastore",
    data_columns = c("rAvgDailyValTraded5Day"),
    computation  = new("PassThruComputation")
  ),
  contains = c("ContextFeature")
)
setGeneric("updateAvgDailyValTraded5Day",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateAvgDailyValTraded5Day")})
setMethod("updateAvgDailyValTraded5Day","AvgDailyValTraded5Day",
           function(object,trade_dates,instrument,strategy,daily_data){
              object <- updateData(object,trade_dates,instrument)
              object@computation <- setComputationData(object@computation,object@data_set@data[c("dtDateTime",object@data_columns)])
              return(object)
           }
)

# Trade offside vs previous trade

setClass(
  Class     = "OffsideComputation",
  representation = representation(
    dates   = "Date"
  ),
  contains = c("FeatureComputation"),
  prototype = prototype(
    compute = off_side
  )
)

setClass(
  Class          = "Offside",
  prototype      = prototype(
    data_store   = "",
    data_columns = c(),
    computation  = new("OffsideComputation"),
    key_columns = c('InstrumentID','DateTime')
  ),
  contains  = c("ContextFeature")
)

setGeneric("updateOffside",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateOffside")})
setMethod("updateOffside","Offside",
          function(object,trade_dates,instrument,strategy,daily_data){
            object@computation@dates <- trade_dates
            object@computation <- setComputationData(object@computation,daily_data@data)
            return(object)
          }
)

#NEW POSITION

setClass(
  Class     = "NewPositionComputation",
  representation = representation(
    dates   = "Date"
  ),
  contains = c("FeatureComputation"),
  prototype = prototype(
    compute = new_psn
  )
)

setClass(
  Class          = "NewPosition",
  prototype      = prototype(
    data_store   = "",
    data_columns = c(),
    computation  = new("NewPositionComputation"),
    key_columns = c('InstrumentID','DateTime')
  ),
  contains  = c("ContextFeature")
)

setGeneric("updateNewPosition",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateNewPosition")})
setMethod("updateNewPosition","NewPosition",
          function(object,trade_dates,instrument,strategy,daily_data){
            object@computation@dates <- trade_dates
            object@computation <- setComputationData(object@computation,daily_data@data)
            return(object)
          }
)

#CLOSE POSITION

setClass(
  Class     = "ClosePositionComputation",
  representation = representation(
    dates   = "Date"
  ),
  contains = c("FeatureComputation"),
  prototype = prototype(
    compute = close_psn
  )
)

setClass(
  Class          = "ClosePosition",
  prototype      = prototype(
    data_store   = "",
    data_columns = c(),
    computation  = new("ClosePositionComputation"),
    key_columns = c('InstrumentID','DateTime')
  ),
  contains  = c("ContextFeature")
)

setGeneric("updateClosePosition",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateClosePosition")})
setMethod("updateClosePosition","ClosePosition",
          function(object,trade_dates,instrument,strategy,daily_data){
            object@computation@dates <- trade_dates
            object@computation <- setComputationData(object@computation,daily_data@data)
            return(object)
          }
)

#RSI

setClass(
  Class      = "RSI14",
  prototype  = prototype(
    data_store   = "factor_datastore",
    data_columns = c("rRSI14Day1DaysAgo"),
    computation  = new("PassThruComputation")
  ),
  contains = c("ContextFeature")
)
setGeneric("updateRSI14",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateRSI14")})
setMethod("updateRSI14","RSI14",
           function(object,trade_dates,instrument,strategy,daily_data){
              object <- updateData(object,trade_dates,instrument)
              object@computation <- setComputationData(object@computation,object@data_set@data[c("dtDateTime",object@data_columns)])
              return(object)
           }
)

setClass(
  Class      = "RelativeRSI14",
  prototype  = prototype(
    data_store   = "factor_datastore",
    data_columns = c("rRSIRelative14Day1DaysAgo"),
    computation  = new("PassThruComputation")
  ),
  contains = c("ContextFeature")
)
setGeneric("updateRelativeRSI14",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateRelativeRSI14")})
setMethod("updateRelativeRSI14","RelativeRSI14",
           function(object,trade_dates,instrument,strategy,daily_data){
              object <- updateData(object,trade_dates,instrument)
              object@computation <- setComputationData(object@computation,object@data_set@data[c("dtDateTime",object@data_columns)])
              return(object)
           }
)

setClass(
  Class      = "SectorRelativeRSI14",
  prototype  = prototype(
    data_store   = "factor_datastore",
    data_columns = c("rRSISectorRelative14Day1DaysAgo"),
    computation  = new("PassThruComputation")
  ),
  contains = c("ContextFeature")
)
setGeneric("updateSectorRelativeRSI14",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateSectorRelativeRSI14")})
setMethod("updateSectorRelativeRSI14","SectorRelativeRSI14",
           function(object,trade_dates,instrument,strategy,daily_data){
              object <- updateData(object,trade_dates,instrument)
              object@computation <- setComputationData(object@computation,object@data_set@data[c("dtDateTime",object@data_columns)])
              return(object)
           }
)

setClass(
  Class      = "PriorRSI14",
  prototype  = prototype(
    data_store   = "factor_datastore",
    data_columns = c("rRSI14Day2DaysAgo"),
    computation  = new("PassThruComputation")
  ),
  contains = c("ContextFeature")
)
setGeneric("updatePriorRSI14",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updatePriorRSI14")})
setMethod("updatePriorRSI14","PriorRSI14",
           function(object,trade_dates,instrument,strategy,daily_data){
              object <- updateData(object,trade_dates,instrument)
              object@computation <- setComputationData(object@computation,object@data_set@data[c("dtDateTime",object@data_columns)])
              return(object)
           }
)

#Leg OpenClose

setClass(
  Class     = "LegOpenCloseComputation",
  representation = representation(
    dates   = "Date"
  ),
  contains = c("FeatureComputation"),
  prototype = prototype(
    compute = open_close
  )
)

setClass(
  Class          = "LegOpenClose",
  prototype      = prototype(
    data_store   = "",
    data_columns = c(),
    computation  = new("LegOpenCloseComputation")
    #key_columns = c('InstrumentID','DateTime')
  ),
  contains  = c("ContextFeature")
)

setGeneric("updateLegOpenClose",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateLegOpenClose")})
setMethod("updateLegOpenClose","LegOpenClose",
          function(object,trade_dates,instrument,strategy,daily_data){
            object@computation@dates <- trade_dates
            object@computation <- setComputationData(object@computation,daily_data@data)
            return(object)
          }
)

#50d moving average price

setClass(
  Class      = "MavgPrice50",
  prototype  = prototype(
    data_store   = "factor_datastore",
    data_columns = c("r50DayMovingAverage"),
    computation  = new("PassThruComputation")
  ),
  contains = c("ContextFeature")
)
setGeneric("updateMavgPrice50",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateMavgPrice50")})
setMethod("updateMavgPrice50","MavgPrice50",
           function(object,trade_dates,instrument,strategy,daily_data){
              object <- updateData(object,trade_dates,instrument)
              object@computation <- setComputationData(object@computation,object@data_set@data[c("dtDateTime",object@data_columns)])
              return(object)
           }
)

#DailyN

setClass(
  Class      = "DailyN",
  prototype  = prototype(
    data_store   = "factor_datastore",
    data_columns = c("rDailyN"),
    computation  = new("PassThruComputation")
  ),
  contains = c("ContextFeature")
)
setGeneric("updateDailyN",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateDailyN")})
setMethod("updateDailyN","DailyN",
           function(object,trade_dates,instrument,strategy,daily_data){
              object <- updateData(object,trade_dates,instrument)
              object@computation <- setComputationData(object@computation,object@data_set@data[c("dtDateTime",object@data_columns)])
              return(object)
           }
)

#3MRelLow

setClass(
  Class      = "DaysSinceRelLow3m",
  prototype  = prototype(
    data_store   = "factor_datastore",
    data_columns = c("rDaysSinceRel3MLow"),
    computation  = new("PassThruComputation")
  ),
  contains = c("ContextFeature")
)
setGeneric("updateDaysSinceRelLow3m",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateDaysSinceRelLow3m")})
setMethod("updateDaysSinceRelLow3m","DaysSinceRelLow3m",
           function(object,trade_dates,instrument,strategy,daily_data){
              object <- updateData(object,trade_dates,instrument)
              object@computation <- setComputationData(object@computation,object@data_set@data[c("dtDateTime",object@data_columns)])
              return(object)
           }
)

#6MRelLow

setClass(
  Class      = "DaysSinceRelLow6m",
  prototype  = prototype(
    data_store   = "factor_datastore",
    data_columns = c("rDaysSinceRel6MLow"),
    computation  = new("PassThruComputation")
  ),
  contains = c("ContextFeature")
)
setGeneric("updateDaysSinceRelLow6m",function(object,trade_dates,instrument,strategy,daily_data){standardGeneric("updateDaysSinceRelLow6m")})
setMethod("updateDaysSinceRelLow6m","DaysSinceRelLow6m",
           function(object,trade_dates,instrument,strategy,daily_data){
              object <- updateData(object,trade_dates,instrument)
              object@computation <- setComputationData(object@computation,object@data_set@data[c("dtDateTime",object@data_columns)])
              return(object)
           }
)
