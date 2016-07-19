sourceTo("../analysis_modules/strategy_breakdown_value_traded_per_signal/strategy_breakdown_value_traded_per_signal.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

################################################################################
#
# StrategyBreakdownPnLOnTradeDayPerSignalAnalysisBlock Class
# 
# Computation of value traded strategy breakdown per signals
# 
# Pulls data required for computation and adds required columns.
###############################################################################

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


setMethod("setTradeDataObject",  
          signature(object = "StrategyBreakdownPnLOnTradeDayPerSignalAnalysisBlock", trade_data = "TradedSignalsData"),
          function(object, trade_data){
            .setTradeDataObject(object, trade_data)
          }
)

setMethod("Process",  
          signature(object = "StrategyBreakdownPnLOnTradeDayPerSignalAnalysisBlock"),
          function(object, key_values){
            
            # retrieve data
            trade_data <- getTradeDataObject(object)
            all_sig <- getReferenceData(trade_data)
            
            plt_sig_pl <- ggplot(data=all_sig, aes(x=Signal, fill=Quarter)) +
              geom_bar(aes(weight=TodayPL),position="dodge") +
              facet_grid(Strategy~Long, scales="free_y") +
              ylab("PL on trade day") + xlab("Strategy") + ggtitle('Strategy breakdown') +
              theme(axis.text.x = element_text(angle = 90, hjust = 1))

            
            object <- .setOutputGGPlotData(object, all_sig)

            object <- .setOutputGGPlot(object, plt_sig_pl)
            
            return(object)
          }
)
