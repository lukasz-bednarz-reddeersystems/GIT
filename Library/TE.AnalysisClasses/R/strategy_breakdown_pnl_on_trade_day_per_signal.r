#' @include strategy_breakdown_value_traded_per_signal.r
NULL


################################################################################
#
# StrategyBreakdownPnLOnTradeDayPerSignalAnalysisBlock Class
#
# Computation of value traded strategy breakdown per signals
#
# Pulls data required for computation and adds required columns.
###############################################################################

#' Analysis Module for computation of per Signal breakdown PnL
#'
#' Computation block class that computes Profit and Loss value
#' per signal. Depends on data generated by
#' StrategyBreakdownAnalysisBlock. Generates ggplot with PnL
#' per signal
#'
#' Inherits from "VirtualAnalysisBlock",
#'               "VirtualTradeDataHandler"
#' @export

setClass(
  Class             = "StrategyBreakdownPnLOnTradeDayPerSignalAnalysisBlock",
  slots             = c(
    trade_data      = "TradedSignalsData"
  ),
  prototype         = list(
    key_cols        = c("TraderID", "start", "end"),
    key_values      = data.frame(TraderID = character(),
                                 start    = as.Date(character()),
                                 end    = as.Date(character())),
    column_name_map = hash(c("TraderID", "start", "end"), c("id", "start", "end")),
    required_colnms = c(strategy_breakdown_per_signal_base_cols,
                        strategy_breakdown_per_signal_signal_cols),
    trade_data      = new("TradedSignalsData")
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualTradeDataHandler"
                        )
)


#' Set trade_data object in object slot
#'
#' Public method to set trade_data slot with "TradedSignalsData"
#' class object
#'
#' @rdname setTradeDataObject-StrategyBreakdownPnLOnTradeDayPerSignal-method
#' @param object object of class "StrategyBreakdownPnLOnTradeDayPerSignalAnalysisBlock"
#' @param trade_data object of class "TradedSignalsData"
#' @return \code{object} object of class "StrategyBreakdownPnLOnTradeDayPerSignalAnalysisBlock"
#' @export

setMethod("setTradeDataObject",
          signature(object = "StrategyBreakdownPnLOnTradeDayPerSignalAnalysisBlock", trade_data = "TradedSignalsData"),
          function(object, trade_data){
            TE.RefClasses:::.setTradeDataObject(object, trade_data)
          }
)


#' Trigger computation of analysis data.
#'
#' @param object object of class "StrategyBreakdownPnLOnTradeDayPerSignalAnalysisBlock"
#' @return \code{object} object object of class "StrategyBreakdownPnLOnTradeDayPerSignalAnalysisBlock"
#' @export
setMethod("Process",
          signature(object = "StrategyBreakdownPnLOnTradeDayPerSignalAnalysisBlock"),
          function(object){

            # retrieve data
            trade_data <- getTradeDataObject(object)
            all_sig <- getReferenceData(trade_data)

            plt_sig_pl <- ggplot(data=all_sig, aes_string(x="Signal", fill="Quarter")) +
              geom_bar(aes_string(weight="TodayPL"),position="dodge") +
              facet_grid(Strategy~Long, scales="free_y") +
              ylab("PL on trade day") + xlab("Strategy") + ggtitle('Strategy breakdown') +
              theme(axis.text.x = element_text(angle = 90, hjust = 1))


            object <- .setOutputGGPlotData(object, all_sig)

            object <- .setOutputGGPlot(object, plt_sig_pl)

            return(object)
          }
)
