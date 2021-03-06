sourceTo("../analysis_modules/strategy_breakdown_value_traded_per_signal/strategy_breakdown_value_traded_per_signal.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

################################################################################
#
# StrategyBreakdownPnLAndTurnoverPerEventAnalysisBlock Class
# 
# Computation of value traded strategy breakdown per events
# 
# Pulls data required for computation and adds required columns.
###############################################################################

setClass(
  Class             = "StrategyBreakdownPnLAndTurnoverPerEventAnalysisBlock",
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
          signature(object = "StrategyBreakdownPnLAndTurnoverPerEventAnalysisBlock", trade_data = "TradedSignalsData"),
          function(object, trade_data){
            .setTradeDataObject(object, trade_data)
          }
)

setMethod("Process",  
          signature(object = "StrategyBreakdownPnLAndTurnoverPerEventAnalysisBlock"),
          function(object, key_values){
            
            # retrieve data
            trade_data <- getTradeDataObject(object)
            all_sig <- getReferenceData(trade_data)
            
            
            sig_increases <- all_sig[grepl('_L',all_sig$Strategy),]
            sig_increases <- sig_increases[sig_increases$Long==1,]
            
            s_sig <- all_sig[grepl('_S',all_sig$Strategy),]
            s_sig <- s_sig[s_sig$Long==0,]
            sig_increases <- rbind(sig_increases,s_sig)
            sig_increases <- aggregate(sig_increases[c('ValueUSD','TodayPL')],list(Signal=sig_increases$Signal,Quarter=sig_increases$Quarter),sum)
            sig_increases <- merge(sig_increases[sig_increases$Quarter,],sig_increases[!sig_increases$Quarter,],by=c('Signal'))
            sig_increases$ValueUSD <- sig_increases$ValueUSD.x
            sig_increases$TodayPL <- sig_increases$TodayPL.x
            sig_increases$Delta <- 100*((sig_increases$TodayPL.x/sig_increases$ValueUSD.x)-(sig_increases$TodayPL.y/sig_increases$ValueUSD.y))/abs(sig_increases$TodayPL.y/sig_increases$ValueUSD.y)
            sig_increases$Increase <- sig_increases$Delta > 0
            sig_increases <- sig_increases[order(sig_increases$ValueUSD),]
            sig_increases$Rank <- 1:nrow(sig_increases)
            sig_increases <- rbind(cbind(Quantity="PL",sig_increases[c('Signal','Rank','Delta','Increase')],Value=sig_increases$TodayPL),
                                   cbind(Quantity="Traded",sig_increases[c('Signal','Rank','Delta','Increase')],Value=sig_increases$ValueUSD))
            
            sig_increases$Quantity <- as.character(sig_increases$Quantity)
            sig_increases$Quantity[sig_increases$Quantity=="PL"] <- "PL on trade day"
            sig_increases$Quantity[sig_increases$Quantity=="Traded"] <- "Turnover"
            sig_smmry <- ggplot(data=sig_increases, aes(x=reorder(Signal,Rank),fill=Increase)) +
              geom_bar(aes(weight=Value)) +
              facet_grid(Quantity~., scales="free_y") +
              ylab("$") + xlab("") + ggtitle('PL and turnover by event') +
              labs(fill="Increased")+
              theme(axis.text.x = element_text(angle = 90, hjust = 1))

            
            object <- .setOutputGGPlotData(object, sig_increases)

            object <- .setOutputGGPlot(object, sig_smmry)
            
            return(object)
          }
)
