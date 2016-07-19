sourceTo("../common/ppmodel_client/ppmodel_client.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/ppmodel_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)


####################################
#
# PositionData Class
#
####################################

position_summary_features <- c('Av.Age','Av.PL','Av.MarketValue','Av.Quantity','Av.PsnReturn','PsnRtnVol','Total.PL','Gm.PsnReturn','PsnReturn','InitialValue','StockReturn','RelativeReturn')
trade_history_features <- c('MarketValue','TodayPL')
trade_delta_features <- c('DeltaSwing','DeltaSkew','DeltaPL')
trade_basic_features <- c("TradeDate", "TradeID", "Long", "Instrument", "Trader", "ValueUSD", "Strategy", 
                         "StrategyID", "PsnLong", "ProfitTarget", "StopLoss")
trade_basic_features_transl <- c("Date", "TradeID", "Long", "InstrumentID", "TraderName", "ValueUSD", "Strategy", 
                          "StrategyID", "PsnLong", "ProfitTarget", "StopLoss")

setClass(
  Class               = "VirtualTradeData",
  prototype           = list(
    required_colnms   = c(trade_basic_features_transl,
                          outcome_price_features, 
                          context_price_features, 
                          position_summary_features,  
                          trade_history_features ,
                          trade_delta_features)
  ),
  
  contains = c("VirtualReferenceData")
)


setClass(
  Class               = "TradeData",
    prototype           = list(
    key_cols = c("id", "start", "end"),
    values            = c(trade_basic_features,
                          outcome_price_features, 
                          context_price_features, 
                          position_summary_features,  
                          trade_history_features,
                          trade_delta_features),
    column_name_map   = hash(c(trade_basic_features, 
                               outcome_price_features, 
                               context_price_features, 
                               position_summary_features,  
                               trade_history_features ,
                               trade_delta_features), 
                             c(trade_basic_features_transl,
                               outcome_price_features, 
                               context_price_features, 
                               position_summary_features,  
                               trade_history_features ,
                               trade_delta_features)),
    #non_na_cols       = c("TradeID"), 
    key_values        = data.frame(id = integer(), 
                                   start = as.Date(character()),
                                   end = as.Date(character())),
    model_class       = "TradeHistorySimpleWithSummary",
    unique_rows       = FALSE
    ),
  
  contains = c("VirtualTradeData", "VirtualPPModelClient")
)


setMethod(".generateDataFilledWithNA",  
          signature(object = "TradeData", trader = "integer", start = "Date", end = "Date"),
          function(object, trader, start, end){
            
            ret_vars <- getDataSourceReturnColumnNames(object)
            
            diff <- setdiff(ret_vars, c("TradeDate"))
            
            ret_data <- data.frame(Date = seq(from = start, to = end, by = 1))
            
            ret_data <- cbind(ret_data, data.frame(t(rep(NA,length(diff)))))
            
            colnames(ret_data) <- c("TradeDate", diff )
            
            return(ret_data)
          }
)



setClass(
  Class             = "ExtendedTradeData",
  contains          = c("VirtualTradeData" )
)

setClassUnion("NullableExtendedTradeData", c("NULL", "ExtendedTradeData"))

setClass(
  Class             = "TradeDataWithMarketReturn",
  contains          = c("ExtendedTradeData" )
)

setClassUnion("NullableTradeDataWithMarketReturn", c("NULL", "TradeDataWithMarketReturn"))

setClass(
  Class             = "TradesExtendedReturnPerMonth",
  prototype         = list(
    required_colnms = c("Return", "Month", "Strategy", "Long", "Value")
  ),
  contains          = c("VirtualTradeData")
)

