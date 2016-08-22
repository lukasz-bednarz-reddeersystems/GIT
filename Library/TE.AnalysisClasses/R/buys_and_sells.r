sourceTo("../analysis_modules/analysis_block/analysis_block.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/trade_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/market_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

################################################################################
#
# BuysAndSellsAnalysisBlock Class
# 
#1. Number of extended buys and sells in month compared to number market down days
#
###############################################################################

setClass(
  Class             = "BuysAndSellsAnalysisBlock",
  slots             = c(
    market_data = "MarketDataSX5E",
    trade_data  = "ExtendedTradeData"
  ),
  prototype         = list(
    trade_data  = new("ExtendedTradeData")
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualTradeDataHandler",
                        "VirtualMarketDataHandler"
                        )
)

setMethod("setTradeDataObject",  
          signature(object = "BuysAndSellsAnalysisBlock", trade_data = "TradeData"),
          function(object, trade_data){
            .setTradeDataObject(object, trade_data)
          }
)

setMethod("setMarketDataObject",  
          signature(object = "BuysAndSellsAnalysisBlock", market_data = "MarketData"),
          function(object, market_data){
            .setMarketDataObject(object, market_data)
          }
)

setMethod("Process",  
          signature(object = "BuysAndSellsAnalysisBlock"),
          function(object, key_values){
            
            # retrieve needed ref_data
            trades <- getReferenceData(getTradeDataObject(object))
            index <- getReferenceData(getMarketDataObject(object))
            
            trader <- unique(trades$TraderName)
            s_str <- paste0(trader, "_S")
            l_str <- paste0(trader, "_L")
            
            # compute required data
            buy_sells <- trades
            buy_sells$Month <- format(buy_sells$Date,'%Y-%m')
            buy_sells <- aggregate(buy_sells['InstrumentID'],list(Month=buy_sells$Month,Strategy=buy_sells$Strategy,Long=buy_sells$Long),function(x)length(x))
            up_days <- merge(index,trades,by='Date')
            up_days$Up_Buy <- up_days$SX5E.Return>0&(up_days$Long==1)
            up_days$Up_Sell<- up_days$SX5E.Return>0&(up_days$Long==0)
            up_days$Month <-format(up_days$Date,'%Y-%m')
            up_days <- aggregate(up_days[c('Up_Buy','Up_Sell')],list(Month=up_days$Month,Strategy=up_days$Strategy),sum)
            colnames(buy_sells) <- c('Month','Strategy','Long','Count')
            buy_sells$Side <- 'Sell'
            buy_sells[buy_sells$Long==1,]$Side <- 'Buy'
            buy_sells <- rbind(buy_sells[c('Month','Strategy','Count','Side')],
                               cbind(Side='Up_Buy',up_days[c('Month','Strategy')],Count=up_days$Up_Buy),
                               cbind(Side='Up_Sell',up_days[c('Month','Strategy')],Count=up_days$Up_Sell))
            
            
            ttls_long <- aggregate(buy_sells[grep(l_str,buy_sells$Strategy),]$Count,list(Month=buy_sells[grep(l_str,buy_sells$Strategy),]$Month,Side=buy_sells[grep(l_str,buy_sells$Strategy),]$Side),sum)
            ttls_long$Strategy <- 'Overall Long'
            colnames(ttls_long) <- c('Month','Side','Count','Strategy')
            ttls_short <- aggregate(buy_sells[grep(s_str,buy_sells$Strategy),]$Count,list(Month=buy_sells[grep(s_str,buy_sells$Strategy),]$Month,Side=buy_sells[grep(s_str,buy_sells$Strategy),]$Side),sum)
            ttls_short$Strategy <- 'Overall Short'
            colnames(ttls_short) <- c('Month','Side','Count','Strategy')
            
            
            buy_sells <- rbind(buy_sells,ttls_long)
            buy_sells <- rbind(buy_sells,ttls_short)
            
             n_buys_sells <- ggplot(data=buy_sells, aes(x=Month, fill=Side)) +
              geom_bar(aes(weight=Count),position="dodge") +
              facet_grid(Strategy~.) +
              ylab("") + xlab("") + ggtitle('Stock extension') 
            
            object <- .setOutputGGPlotData(object,buy_sells)
            object <- .setOutputGGPlot(object, n_buys_sells)
            
             
            return(object)
          }
)
