#' @include trade_data.r
#' @include ppmodel_client.r
NULL

####################################
#
# PositionData Class
#
####################################

#' Concrete S4 class handling Simple TradeData
#'
#' Inherits from "VirtualTradeData" and "VirtualPPModelClient"
#'
#' @export

setClass(
  Class               = "TradeDataSimple",
    prototype           = list(
    key_cols = c("id", "start", "end"),
    values            = c("TradeDate", "TradeID", "Long", "Instrument", "Trader", "ValueUSD", "Strategy",
                          outcome_price_features, context_price_features ),
    column_name_map   = hash(c("TradeID", "TradeDate", "Long", "Instrument", "Trader", "ValueUSD", "Strategy",
                               outcome_price_features, context_price_features),
                             c("TradeID", "Date", "Long", "InstrumentID", "TraderName", "ValueUSD","Strategy",
                               outcome_price_features, context_price_features)),
    required_colnms   = c("Date", "TradeID", "Long", "InstrumentID", "TraderName", "ValueUSD", "Strategy",
                          outcome_price_features, context_price_features),
    key_values        = data.frame(id = integer(),
                                   start = as.Date(character()),
                                   end = as.Date(character())),
    model_class       = "TradeHistorySimple"
    ),

  contains = c("VirtualTradeData", "VirtualPPModelClient")
)


setMethod(".generateDataFilledWithNA",
          signature(object = "TradeDataSimple", trader = "integer", start = "Date", end = "Date"),
          function(object, trader, start, end){

            ret_vars <- getDataSourceReturnColumnNames(object)

            diff <- setdiff(ret_vars, c("TradeDate"))

            ret_data <- data.frame(Date = seq(from = start, to = end, by = 1))

            ret_data <- cbind(ret_data, data.frame(t(rep(NA,length(diff)))))

            colnames(ret_data) <- c("TradeDate", diff )

            return(ret_data)
          }
)

