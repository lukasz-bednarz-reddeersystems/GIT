sourceTo("../analysis_modules/analysis_block/analysis_block.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/trade_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/extended_trade_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/market_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

################################################################################
#
# MarketReturnAnalysisBlock Class
# 
#1. Number of extended buys and sells in month compared to number market down days
#
###############################################################################

setClass(
  Class             = "MarketReturnAnalysisBlock",
  slots             = c(
    market_data = "MarketDataSX5E"
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualTradeDataHandler",
                        "VirtualMarketDataHandler"
                        )
)

setMethod("setTradeDataObject",  
          signature(object = "MarketReturnAnalysisBlock", trade_data = "TradeData"),
          function(object, trade_data){
            .setTradeDataObject(object, trade_data)
          }
)

setMethod("setMarketDataObject",  
          signature(object = "MarketReturnAnalysisBlock", market_data = "MarketDataSX5E"),
          function(object, market_data){
            .setMarketDataObject(object, market_data)
          }
)

setMethod("Process",  
          signature(object = "MarketReturnAnalysisBlock"),
          function(object, key_values){
            
            # retrieve needed ref_data
            trades <- getReferenceData(getTradeDataObject(object))
            index <- getReferenceData(getMarketDataObject(object))
            
            trader <- unique(trades$TraderName)
            s_str <- paste0(trader, "_S")
            l_str <- paste0(trader, "_L")
            
            # compute required data
            extended_return <- trades
            extended_return$Gm.PsnReturn <- extended_return$Gm.PsnReturn/10000
            n_trades <- aggregate(extended_return$Gm.PsnReturn,list(Date=extended_return$Date,Strategy=extended_return$Strategy,Long=extended_return$Long),function(x)sum(!is.na(x)))
            extended_return <- merge(n_trades,extended_return,by=c('Date','Strategy','Long'))
            extended_return$Gm.PsnReturn <- extended_return$Gm.PsnReturn/extended_return$x
            extended_return <- aggregate(extended_return[c('Gm.PsnReturn','Return')],list(Date=extended_return$Date,Strategy=extended_return$Strategy,Long=extended_return$Long),function(x)sum(x,na.rm=TRUE))
            extended_return <- merge(extended_return,index,by='Date')
            extended_return$Return[is.infinite(extended_return$Return)] <- NA
            extended_return_cor <- cor(extended_return$Return,extended_return$SX5E.Return,use="na.or.complete")
            extended_return$Month <-format(extended_return$Date,'%Y-%m')
            extended_return <- aggregate(extended_return[c('Gm.PsnReturn','Return','SX5E.Return')],list(Month=extended_return$Month,Strategy=extended_return$Strategy,Long=extended_return$Long),function(x)mean(log(1+x),na.rm=TRUE))
            extended_return <- rbind(cbind(Return='Position',extended_return[c('Month','Strategy','Long')],Value=extended_return$Gm.PsnReturn),
                                     cbind(Return='Trade',extended_return[c('Month','Strategy','Long')],Value=extended_return$Return),
                                     cbind(Return='Index',extended_return[c('Month','Strategy','Long')],Value=extended_return$SX5E.Return))
            
            
            ttl_rtn_long <- aggregate(extended_return[grep(l_str,extended_return$Strategy),]$Value,list(Month=extended_return[grep(l_str,extended_return$Strategy),]$Month,Side=extended_return[grep(l_str,extended_return$Strategy),]$Long,Return=extended_return[grep(l_str,extended_return$Strategy),]$Return),mean)
            ttl_rtn_long$Strategy <- 'Overall Long'
            colnames(ttl_rtn_long) <- c('Month','Long','Return','Value','Strategy')
            ttl_rtn_short <- aggregate(extended_return[grep(s_str,extended_return$Strategy),]$Value,list(Month=extended_return[grep(s_str,extended_return$Strategy),]$Month,Side=extended_return[grep(s_str,extended_return$Strategy),]$Long,Return=extended_return[grep(s_str,extended_return$Strategy),]$Return),mean)
            ttl_rtn_short$Strategy <- 'Overall Short'
            colnames(ttl_rtn_short) <- c('Month','Long','Return','Value','Strategy')
            
            extended_return <- rbind(extended_return,ttl_rtn_long)
            extended_return <- rbind(extended_return,ttl_rtn_short)
            extended_return$Value[is.infinite(extended_return$Value)] <- 0
            extended_return$Value[is.nan(extended_return$Value)] <- 0
            
            # generate plot data
            extension_rtns <- ggplot(data=extended_return, aes(x=Month, fill=Return)) +
              geom_bar(aes(weight=Value),position="dodge") +
              facet_grid(Strategy~Long, scales="free_y") +
              ylab("ln(Return)") + xlab("Month") + ggtitle('Average daily log return due to extended trades') 
            
           
            object <- .setOutputGGPlotData(object,extended_return)
            object <- .setOutputGGPlot(object, extension_rtns)
            
            output_obj <- new("TradesExtendedReturnPerMonth")
            
            output_obj <- setReferenceData(output_obj, extended_return)
            object <- .setOutputObject(object, output_obj)
            
             
            return(object)
          }
)
