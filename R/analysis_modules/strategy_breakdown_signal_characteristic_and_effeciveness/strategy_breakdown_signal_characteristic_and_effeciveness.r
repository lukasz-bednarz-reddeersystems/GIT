sourceTo("../analysis_modules/strategy_breakdown_value_traded_per_signal/strategy_breakdown_value_traded_per_signal.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

################################################################################
#
# StrategyBreakdownSignalCharacteristicAndEffectivenessAnalysisBlock Class
# 
# Computation of value traded strategy breakdown per events
# 
# Pulls data required for computation and adds required columns.
###############################################################################

setClass(
  Class             = "StrategyBreakdownSignalCharacteristicAndEffectivenessAnalysisBlock",
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
          signature(object = "StrategyBreakdownSignalCharacteristicAndEffectivenessAnalysisBlock", trade_data = "TradedSignalsData"),
          function(object, trade_data){
            .setTradeDataObject(object, trade_data)
          }
)

setMethod("Process",  
          signature(object = "StrategyBreakdownSignalCharacteristicAndEffectivenessAnalysisBlock"),
          function(object, key_values){
            
            # retrieve data
            trade_data <- getTradeDataObject(object)
            all_sig <- getReferenceData(trade_data)
            
            signal_map <- data.frame(Type=c(rep('Company Event',3),rep('Corporate Action',6),rep('Dividend',2),rep('Index',5),rep('Insiders',4),rep('Results',4)),
                                     Signal=c('AGM','Road Show','Shareholder Meeting','Primary Placing','Secondary Meeting','Close Period','Stock Split','Rights Issue','New Issue','Special Dividend','Dividend','Index Add Confirmed','Index Reweight Increase Expected','Index Reweight Reduce Expected','Index Reweight Increase Confirmed','Index Remove Expected','Director Buy Core','Director Sell Core','Director Buy Non Core','Director Sell Non Core','Trading Statement','Results','Conference Call','Monthly Unit Sales'),
                                     SCoord=1:24)
            
            sig_to <- aggregate(all_sig['ValueUSD'],list(Signal=all_sig$Signal,Strategy=all_sig$Strategy,Quarter=all_sig$Quarter),sum)
            rtn <- all_sig[grepl('_L',all_sig$Strategy),]
            rtn <- rtn[rtn$Long==1,]
            s_rtn <- all_sig[grepl('_S',all_sig$Strategy),]
            s_rtn <- s_rtn[s_rtn$Long==0,]
            rtn <- rbind(rtn,s_rtn) 
            rtn$Yeild <- 1+(rtn$TodayPL/rtn$ValueUSD)
            sig_to <- sig_to[c('Quarter','Signal','Strategy','ValueUSD')]
            sig_to <- rbind(data.frame(Quarter=TRUE,Signal=rep(unique(sig_to$Signal),length(unique(sig_to$Strategy))),Strategy=rep(unique(sig_to$Strategy),length(unique(sig_to$Signal))),ValueUSD=0),
                            data.frame(Quarter=FALSE,Signal=rep(unique(sig_to$Signal),length(unique(sig_to$Strategy))),Strategy=rep(unique(sig_to$Strategy),length(unique(sig_to$Signal))),ValueUSD=0),
                            sig_to)
            sig_to <- aggregate(sig_to['ValueUSD'],list(Signal=sig_to$Signal,Strategy=sig_to$Strategy,Quarter=sig_to$Quarter),sum)
            sig_to <- merge(sig_to,signal_map,by='Signal')
            sig_to <- merge(sig_to,rtn[c('Signal','Strategy','Quarter','Yeild')],by=c('Signal','Strategy','Quarter'),all.x=TRUE)
            sig_to$Yeild[is.na(sig_to$Yeild)] <-0
            fd <- sig_to
            sig_to$x <- sin(2*pi*(sig_to$SCoord-1)/24)*sig_to$ValueUSD
            sig_to$y <- cos(2*pi*(sig_to$SCoord-1)/24)*sig_to$ValueUSD
            sig_to$yld_y <- cos(2*(pi*sig_to$SCoord-1)/24)*sig_to$Yeild
            sig_to$yld_x <- sin(2*(pi*sig_to$SCoord-1)/24)*sig_to$Yeild
            sig_to$Yeild <- sig_to$Yeild*sig_to$ValueUSD
            sig_to <- aggregate(sig_to[c('x','y','yld_x','yld_y','ValueUSD','Yeild')],list(Strategy=sig_to$Strategy,Quarter=sig_to$Quarter),sum)
            sig_to$Direction <- lst_cnv(sig_to$x,sig_to$y,sig_to$ValueUSD)
            sig_to$Direction <- 24*sig_to$Direction/(2*pi)
            sig_to$ValueUSD <- sqrt(sig_to$x^2+sig_to$y^2)
            sig_to$YeildDir <- lst_cnv(sig_to$yld_x,sig_to$yld_y,sig_to$Yeild)
            sig_to$YeildDir <- 24*sig_to$YeildDir/(2*pi)
            sig_to$Yeild <- sqrt(sig_to$yld_x^2+sig_to$yld_y^2)
            
            sig_to <- cbind(Type="Turnover",sig_to[c('Strategy','Quarter')],Direction=sig_to$Direction,Value=log(sig_to$ValueUSD))
            fd <- cbind(Type="Turnover",fd[c('Strategy','Quarter')],Direction=fd$SCoord,Value=log(fd$ValueUSD))
            
            sig_to$Quarter <- as.character(sig_to$Quarter)
            sig_to$Quarter[sig_to$Quarter=='TRUE'] <- 'This Q'
            sig_to$Quarter[sig_to$Quarter=='FALSE'] <- 'Previous 3Q'
            fd$Quarter <- as.character(fd$Quarter)
            fd$Quarter[fd$Quarter=='TRUE'] <- 'This Q'
            fd$Quarter[fd$Quarter=='FALSE'] <- 'Previous 3Q'
            sig_strat <- ggplot(data=sig_to, aes(x = Direction, y = Value)) + 
              coord_polar() + 
              geom_segment(data=fd,aes(y=0, xend = Direction, yend = Value, colour=Quarter)) +
              geom_segment(aes(y = 0, xend = Direction, yend = Value, colour=Quarter),arrow = arrow(length = unit(0.1,"npc")), size = 1.2) +
              scale_x_continuous(breaks = c(2,6,10,14,18,22), labels = unique(as.character(signal_map$Type))) +
              ylab("") + xlab("") + ggtitle('Overall event type by strategy') +
              theme(axis.text.y = element_blank(),axis.ticks.y = element_blank(),legend.title=element_blank(),legend.position="bottom") +
              facet_wrap(~Strategy, ncol=3)

            
            object <- .setOutputGGPlotData(object, sig_to)

            object <- .setOutputGGPlot(object, sig_strat)
            
            return(object)
          }
)
