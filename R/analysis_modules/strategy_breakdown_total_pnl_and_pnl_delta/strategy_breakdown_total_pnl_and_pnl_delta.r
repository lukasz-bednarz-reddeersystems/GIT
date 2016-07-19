sourceTo("../analysis_modules/strategy_breakdown/strategy_breakdown.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/analysis_block_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/strategy_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

################################################################################
#
# StrategyBreakdownTotalAndDeltaPnLAnalysisBlock Class
# 
# Computation of strategy breakdown pie plots
# 
# Pulls data required for computation and adds required columns.
###############################################################################


setClass(
  Class             = "StrategyBreakdownTotalAndDeltaPnLAnalysisBlock",
  prototype         = list(
    strategy_data   = new("StrategyBreakDownData"), 
    required_colnms = c("Type", "Quantity", "Value", "Strategy", "Quarter" )
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualStrategyDataHandler"
                        )
)


setMethod("setStrategyDataObject",  
          signature(object = "StrategyBreakdownTotalAndDeltaPnLAnalysisBlock", strategy_data = "StrategyBreakDownData"),
          function(object, strategy_data){
            .setStrategyDataObject(object, strategy_data)
          }
)

setMethod("Process",  
          signature(object = "StrategyBreakdownTotalAndDeltaPnLAnalysisBlock"),
          function(object, key_values){
            
            # retrieve data
            strategy_data <- getStrategyDataObject(object)
            
            strategy_data <- getReferenceData(strategy_data)
            
            # compute output
            pl_data <- strategy_data[strategy_data$Type=='Position level'&strategy_data$Quantity=='PL',]
            pl_data <- merge(pl_data[pl_data$Quarter,],pl_data[!pl_data$Quarter,],by=c('Type','Quantity','Strategy'))
            pl_data$Value <- pl_data$Value.x
            pl_data$Delta <- pl_data$Value.x - pl_data$Value.y
            pl_data$Delta <- 100*(pl_data$Delta/abs(pl_data$Value.y))
            
            lbls <- paste(round(pl_data$Delta),"%",sep="")
            lbls <- gsub('NaN%','',lbls)
            pl_smmry <- ggplot(data=pl_data, aes(x=reorder(Strategy,Value), fill=Strategy)) +
              geom_bar(aes(weight=Value)) +
              coord_flip() +
              geom_text(aes(x= Strategy, y=Value, label = lbls),size=3) +  
              theme(legend.position = "none") +
              ylab("PL $") + xlab("Strategy") + ggtitle('Total PL and PL change by strategy') +
              theme(axis.text.x = element_text(angle = 90, hjust = 1))

            
            object <- .setOutputGGPlotData(object, pl_data)

            object <- .setOutputGGPlot(object, pl_smmry)
            
            return(object)
          }
)
