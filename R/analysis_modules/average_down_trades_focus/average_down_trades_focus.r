sourceTo("../analysis_modules/average_down_trades/average_down_trades.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

################################################################################
#
# AverageDownTradesFocusAnalysisBlock Class
# 
# Computation block class to pull data required for Computation of extended stock
# Pulls data required for computation and adds required columns.
###############################################################################


setClass(
  Class             = "AverageDownTradesFocusAnalysisBlock",
  slots             = c(
    trade_data     = "AverageDownTradesData"
  ),
  prototype         = list(
    required_colnms = avg_dwn_cols,
    trade_data     = new("AverageDownTradesData")
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualTradeDataHandler"
                        )
)


setMethod("setTradeDataObject",  
          signature(object = "AverageDownTradesFocusAnalysisBlock", trade_data = "AverageDownTradesData"),
          function(object, trade_data){
            .setTradeDataObject(object, trade_data)
          }
)

setMethod("Process",  
          signature(object = "AverageDownTradesFocusAnalysisBlock"),
          function(object, key_values){
            
            trade_data <- getTradeDataObject(object)
            
            # retrieve needed ref_data
            average_down_trades <- getReferenceData(trade_data)
            
            average_down_trades <- unique(average_down_trades[c('InstrumentID','Date','ValueUSD','TodayPL')])
            average_down_trades$Date <- format(average_down_trades$Date,'%Y-%m') 
            adown_focus <- aggregate(average_down_trades['InstrumentID'],list(Date=average_down_trades$Date),function(x)length(x))
            adown_focus <- merge(adown_focus,aggregate(average_down_trades[c('ValueUSD','TodayPL')],list(Date=average_down_trades$Date),function(x)mean(x,na.rm=TRUE)),by='Date')
            
            adown_focus_plt <- ggplot(data=adown_focus,aes(x=Date,y=InstrumentID,size=ValueUSD)) +
              geom_point(aes(colour=TodayPL)) +
              theme(text = element_text(size=15)) +
              ylab("N Trades") + 
              xlab("Month") + 
              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
              scale_size_continuous(guide = FALSE) +
              labs(colour="Av. $ PL") +
              #scale_colour_distiller(palette="Spectral") +
              scale_colour_gradient(low = "Red", high = "Blue") +
              ggtitle('Number average down trades')  
            
            
            # set processed data as an output
            object <- .setOutputGGPlotData(object, adown_focus)
            object <- .setOutputGGPlot(object, adown_focus_plt)
            
            
            return(object)
          }
)
