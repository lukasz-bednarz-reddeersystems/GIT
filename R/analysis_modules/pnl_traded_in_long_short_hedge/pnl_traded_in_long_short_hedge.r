sourceTo("../analysis_modules/value_traded_in_long_short_hedge/value_traded_in_long_short_hedge.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

################################################################################
#
# PnLTradedInLongShortHedgeAnalysisBlock Class
# 
# Generates plot for Traded PNL per month
#
# Pulls data required for computation and adds required columns.
###############################################################################



setClass(
  Class             = "PnLTradedInLongShortHedgeAnalysisBlock",
  prototype         = list(
    trade_data  = new("ValueTradedData")
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualTradeDataHandler"
                        )
)

setMethod("setTradeDataObject",  
          signature(object = "PnLTradedInLongShortHedgeAnalysisBlock", trade_data = "ValueTradedData"),
          function(object, trade_data){
            .setTradeDataObject(object, trade_data)
          }
)

setMethod("Process",  
          signature(object = "PnLTradedInLongShortHedgeAnalysisBlock"),
          function(object, key_values){
            
            # retrieve data
            trade_data <- getTradeDataObject(object)
            
            trd_data <- getReferenceData(trade_data)
            
            ttl_pl_smrry <- aggregate(trd_data[c('TodayPL','ValueUSD')],
                                      list(Strategy=trd_data$ST,Month=trd_data$Month),
                                      function(x)sum(x,na.rm=TRUE))
            
            # compute output
            pl_plot <- ggplot(data=ttl_pl_smrry,aes(x=Month,y=TodayPL,group=Strategy,colour=Strategy)) +
              geom_line(size=1) 

            
            object <- .setOutputGGPlotData(object, ttl_pl_smrry)

            object <- .setOutputGGPlot(object, pl_plot)
            
            return(object)
          }
)
