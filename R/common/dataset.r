sourceTo("../lib/dataset.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

setClass(
  Class          = "PositionDataSet",
  representation = representation(
    ttl_by_strat = "data.frame"
  ),
  prototype      = prototype(
    key_cols     = c("InstrumentID","StrategyID","Date"),
    data_cols    = c("TodayPL","MarketValue")
  ), contains = c("DataSet")
)

setGeneric("positionTotalsByStrat", function(object){standardGeneric("positionTotalsByStrat")})
setMethod("positionTotalsByStrat", "PositionDataSet",
          function(object){
            message("Computing position totals...")
            object@ttl_by_strat <- aggregateGroup(object,c('TodayPL','MarketValue'),c('Date','StrategyID'),sum)
            return(object)
          }
)

setClass(
  Class           = "StrategiesDataSet",
  prototype       = prototype(
    key_cols      = c("StrategyID"),
    data_cols     = c("Name","Active","Trader","FundGroup","UserID","Alias","Group","Type","Direction","Description","AliasID","GroupID","TypeID","InitiatorID")
  ), contains = c("DataSet")
)

setClass(
  Class          = "TradeHistoryDataSet",
  representation = representation(
    trader_id    = "integer",
    start_date   = "Date",
    end_date     = "Date"
  ),
  prototype      = prototype(
    key_cols     = c('InstrumentID','TradeDate','Trader','Strategy','BuySell'),
    data_cols    = c('ValueUSD')
  ), contains = c("DataSet")
)

