#' @include ppmodel_client.r
NULL

####################################
#
# TradeData Class
#
####################################

#' List of position summary features names
position_summary_features <- c('Av.Age','Av.PL','Av.MarketValue','Av.Quantity','Av.PsnReturn','PsnRtnVol','Total.PL','Gm.PsnReturn','PsnReturn','InitialValue','StockReturn','RelativeReturn')

#' List of trade history features names
trade_history_features <- c('MarketValue','TodayPL')

#' List of trade delta features names
trade_delta_features <- c('DeltaSwing','DeltaSkew','DeltaPL')

#' List of trade history basic features names
trade_basic_features <- c("TradeDate", "TradeID", "OrderID", "Long", "Instrument", "Trader", "ValueUSD", "Strategy",
                         "StrategyID", "PsnLong", "ProfitTarget", "StopLoss")

#' List of translated trade history basic features names
trade_basic_features_transl <- c("Date", "TradeID", "OrderID", "Long", "InstrumentID", "TraderName", "ValueUSD", "Strategy",
                          "StrategyID", "PsnLong", "ProfitTarget", "StopLoss")

devtools::use_data(position_summary_features,
                   trade_history_features,
                   trade_delta_features,
                   trade_basic_features,
                   trade_basic_features_transl,
                   overwrite = TRUE)

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

#' Concrete S4 class handling TradeData
#'
#' Inherits from "VirtualTradeData" and "VirtualPPModelClient"
#'
#' @export

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
          signature(object = "TradeData"),
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

