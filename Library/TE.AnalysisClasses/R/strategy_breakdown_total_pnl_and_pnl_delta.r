#' @include strategy_breakdown.r
NULL


################################################################################
#
# StrategyBreakdownTotalAndDeltaPnLAnalysisBlock Class
#
# Computation of strategy breakdown pie plots
#
# Pulls data required for computation and adds required columns.
###############################################################################

#' Analysis Module for computation of Total and Delta PnL per strategy
#'
#' Computation block class that computes total and delta Profit and Loss.
#' Depends on output from StrategyBreakdownAnalysisBlock.
#' Generates ggplot computed data.
#'
#' Inherits from "VirtualAnalysisBlock",
#'               "VirtualStrategyDataHandler"
#' @export
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

#' @describeIn setStrategyDataObject
#' Set strategy_data object in object slot
#' @inheritParams setStrategyDataObject
#'
# ' @rdname setStrategyDataObject-StrategyBreakdownTotalAndDeltaPnL-method
# ' @param object object of class "StrategyBreakdownTotalAndDeltaPnLAnalysisBlock"
# ' @param strategy_data object of class "StrategyBreakDownData"
# ' @return \code{object} object of class "StrategyBreakdownTotalAndDeltaPnLAnalysisBlock"
#' @export
setMethod("setStrategyDataObject",
          signature(object = "StrategyBreakdownTotalAndDeltaPnLAnalysisBlock", strategy_data = "StrategyBreakDownData"),
          function(object, strategy_data){
            TE.RefClasses:::.setStrategyDataObject(object, strategy_data)
          }
)



#' @describeIn Process
#'
#' Trigger computation of analysis data.
#'
#' @inheritParams Process
#'
# ' @param object object of class "StrategyBreakdownTotalAndDeltaPnLAnalysisBlock"
# ' @return \code{object} object object of class "StrategyBreakdownTotalAndDeltaPnLAnalysisBlock"
#' @export
setMethod("Process",
          signature(object = "StrategyBreakdownTotalAndDeltaPnLAnalysisBlock"),
          function(object){

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
            pl_smmry <- ggplot(data=pl_data, aes_string(x="reorder(Strategy,Value)", fill="Strategy")) +
              geom_bar(aes_string(weight="Value")) +
              coord_flip() +
              geom_text(aes_string(x= "Strategy", y="Value", label = "lbls"),size=3) +
              theme(legend.position = "none") +
              ylab("PL $") + xlab("Strategy") + ggtitle('Total PL and PL change by strategy') +
              theme(axis.text.x = element_text(angle = 90, hjust = 1))


            object <- .setOutputGGPlotData(object, pl_data)

            object <- .setOutputGGPlot(object, pl_smmry)
            object <- .setOutputFrontendData(object, data.frame(omit = c("Value", "lbls")))

            return(object)
          }
)
