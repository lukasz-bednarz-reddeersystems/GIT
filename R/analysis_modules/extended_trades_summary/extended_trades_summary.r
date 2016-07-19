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
  Class             = "ExtendedTradesSummaryAnalysisBlock",
  contains          = c("VirtualAnalysisBlock",
                        "VirtualTradeDataHandler",
                        "VirtualExtendedTradeDataHandler"
                        )
)

setMethod("setTradeDataObject",  
          signature(object = "ExtendedTradesSummaryAnalysisBlock", trade_data = "TradeData"),
          function(object, trade_data){
            .setTradeDataObject(object, trade_data)
          }
)

setMethod("setExtendedTradeDataObject",  
          signature(object = "ExtendedTradesSummaryAnalysisBlock", ex_trade_data = "ExtendedTradeData"),
          function(object, ex_trade_data){
            .setExtendedTradeDataObject(object, ex_trade_data)
          }
)

setMethod("Process",  
          signature(object = "ExtendedTradesSummaryAnalysisBlock"),
          function(object, key_values){
            
            # retrieve needed ref_data
            extended_trades <- getReferenceData(getExtendedTradeDataObject(object))
            history_data <- getReferenceData(getTradeDataObject(object))
            
            
            extended_InstrumentIDs <- extended_trades$InstrumentID
            extended_positions <- merge(data.frame(InstrumentID=extended_InstrumentIDs),history_data,by=c('InstrumentID'))
            smmry_cols <- c('SkewInto','SkewOutof','DeltaSwing','DeltaSkew','DeltaPL')
            mn_data <- unique(extended_positions[c('Strategy','InstrumentID','Date',smmry_cols)])
            mn_smmry<- aggregate(extended_positions[smmry_cols],list(Strategy=extended_positions$Strategy),function(x)mean(x,na.rm=TRUE))
            pl_data <- unique(extended_positions[c('Strategy','InstrumentID','Date','TodayPL')])
            pl_data$Month <- format(pl_data$Date,'%Y-%m')
            pl_mnth_smmry<- aggregate(pl_data$TodayPL,list(Strategy=pl_data$Strategy,Month=pl_data$Month),function(x)sum(x,na.rm=TRUE))
            colnames(pl_mnth_smmry) <- c('Strategy','Month','PL')
            pl_smmry<- aggregate(pl_data$TodayPL,list(Strategy=pl_data$Strategy),function(x)sum(x,na.rm=TRUE))
            all_data<- unique(history_data[c('Strategy','InstrumentID','Date','TodayPL')])
            all_data$Month <- format(all_data$Date,'%Y-%m')
            all_mnth_pl  <- aggregate(all_data$TodayPL,list(Strategy=all_data$Strategy,Month=all_data$Month),function(x)sum(x,na.rm=TRUE))
            colnames(all_mnth_pl) <- c('Strategy','Month','PL')
            all_pl  <- aggregate(all_data$TodayPL,list(Strategy=all_data$Strategy),function(x)sum(x,na.rm=TRUE))
            
            mnthly_pl <- rbind(cbind(Key='Total',all_mnth_pl),
                               cbind(Key='Extended',pl_mnth_smmry))
            mpl <- ggplot(data=mnthly_pl, aes(x=Month, fill=Key)) +
              geom_bar(aes(weight=PL),position="dodge") +
              facet_grid(Strategy~.) +
              ylab("PL") + xlab("Month") + ggtitle('PL in positions featuring extended trades') 
            
            
            object <- .setOutputGGPlotData(object,mnthly_pl)
            object <- .setOutputGGPlot(object, mpl)
            
             
            return(object)
          }
)
