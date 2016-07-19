sourceTo("../analysis_modules/strategy_breakdown/strategy_breakdown.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/analysis_block_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/strategy_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

################################################################################
#
# StrategyBreakdownAUMAndTurnoverAnalysisBlock Class
# 
# Computation of strategy breakdown pie plots
# 
# Pulls data required for computation and adds required columns.
###############################################################################


setClass(
  Class             = "StrategyBreakdownAUMAndTurnoverAnalysisBlock",
  prototype         = list(
    strategy_data   = new("StrategyBreakDownData"), 
    required_colnms = c("Type", "Quantity", "Value", "Strategy", "Quarter" )
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualStrategyDataHandler"
                        )
)


setMethod("setStrategyDataObject",  
          signature(object = "StrategyBreakdownAUMAndTurnoverAnalysisBlock", strategy_data = "StrategyBreakDownData"),
          function(object, strategy_data){
            .setStrategyDataObject(object, strategy_data)
          }
)

setMethod("Process",  
          signature(object = "StrategyBreakdownAUMAndTurnoverAnalysisBlock"),
          function(object, key_values){
            
            # retrieve data
            strategy_data <- getStrategyDataObject(object)
            
            strategy_data <- getReferenceData(strategy_data)
            
            # compute output
            sd_quarter <- pie_data(strategy_data[strategy_data$Quantity=='Value'&strategy_data$Quarter,])
            sd_history <- pie_data(strategy_data[strategy_data$Quantity=='Value'&!strategy_data$Quarter,])
            sd <- merge(sd_quarter[c('Type','Value','Strategy')],sd_history[c('Type','Value','Strategy')],by=c('Type','Strategy'))
            sd$Value <- sd$Value.x
            sd$Delta <- sd$Value.x - sd$Value.y
            sd$breaks[sd$Type=='Position level'] <- cumsum(sd$Value[sd$Type=='Position level']) - sd$Value[sd$Type=='Position level']/2
            sd$breaks[sd$Type=='Trade level'] <- cumsum(sd$Value[sd$Type=='Trade level']) - sd$Value[sd$Type=='Trade level']/2
            sd$angle[sd$Type=='Position level'] <- (90-360*(sd$breaks[sd$Type=='Position level']/sum(sd$Value[sd$Type=='Position level'])))
            sd$angle[sd$Type=='Trade level'] <- (90-360*(sd$breaks[sd$Type=='Trade level']/sum(sd$Value[sd$Type=='Trade level'])))
            sd$dlabel <- NA
            sd$dlabel[abs(sd$Delta)>5] <- sd$Delta[abs(sd$Delta)>5]
            lbels <- paste(round(sd$dlabel),"%",sep="")
            lbels <- gsub("NA%","",lbels)
            
            sd$Type <- as.character(sd$Type)
            sd$Type[sd$Type=='Position level'] <- 'AUM'
            sd$Type[sd$Type=='Trade level'] <- 'Turnover'
            pie_smmry <- ggplot(data=sd, aes(x=factor(1), y=Value, fill=Strategy)) +
              geom_bar(stat="identity") +
              geom_text(aes(x= factor(1.2), y=breaks, label = Strategy, angle=angle),size=3) +  
              geom_text(aes(x= factor(1), y=breaks, label = lbels),size=4,fontface='bold') +  
              theme(legend.position = "none",axis.ticks = element_blank(),panel.background = element_rect(fill = "white")) +
              coord_polar(theta = "y") +
              scale_y_continuous(breaks=NULL) +
              scale_x_discrete(breaks=NULL) +
              labs(x="",y="") +
              ggtitle('AUM and turnover by strategy') +
              facet_grid(facets = Type~.) 

            
            object <- .setOutputGGPlotData(object, sd)

            object <- .setOutputGGPlot(object, pie_smmry)
            
            return(object)
          }
)
