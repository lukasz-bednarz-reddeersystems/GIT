#' @include value_traded_in_long_short_hedge.r
NULL

################################################################################
#
# PnLTradedInLongShortHedgeAnalysisBlock Class
#
# Generates plot for Traded PNL per month
#
# Pulls data required for computation and adds required columns.
###############################################################################

#' Analysis Module for Computation of traded PnL data.
#'
#' Computation block class to pull data required for Computation of
#' PnL categorized per long , short and hedge strats
#'
#' Pulls data required for computation and adds required columns.
#'
#' Inherits from "VirtualAnalysisBlock",
#'               "VirtualTradeDataHandler"
#' @export

setClass(
  Class             = "PnLTradedInLongShortHedgeAnalysisBlock",
  prototype         = list(
    trade_data  = new("ValueTradedData")
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualTradeDataHandler"
                        )
)


#' Set trade_data object in object slot
#'
#' Public method to set trade_data slot with "TradeData"
#' class object
#'
#' @rdname setTradeDataObject-PnLTradedInLongShortHedge-method
#' @param object object of class "PnLTradedInLongShortHedgeAnalysisBlock"
#' @param trade_data object of class "TradeData"
#' @return \code{object} object of class "PnLTradedInLongShortHedgeAnalysisBlock"
#' @export

setMethod("setTradeDataObject",
          signature(object = "PnLTradedInLongShortHedgeAnalysisBlock", trade_data = "ValueTradedData"),
          function(object, trade_data){
            TE.RefClasses:::.setTradeDataObject(object, trade_data)
          }
)



#' Trigger computation of analysis data.
#'
#' @rdname Process-PnLTradedInLongShortHedge-method
#' @param object object of class "PnLTradedInLongShortHedgeAnalysisBlock"
#' @return \code{object} object object of class "PnLTradedInLongShortHedgeAnalysisBlock"
#' @export

setMethod("Process",
          signature(object = "PnLTradedInLongShortHedgeAnalysisBlock"),
          function(object){

            # retrieve data
            trade_data <- getTradeDataObject(object)

            trd_data <- getReferenceData(trade_data)

            ttl_pl_smrry <- aggregate(trd_data[c('TodayPL','ValueUSD')],
                                      list(Strategy=trd_data$ST,Month=trd_data$Month),
                                      function(x)sum(x,na.rm=TRUE))

            # compute output
            pl_plot <- ggplot(data=ttl_pl_smrry,aes_string(x="Month",
                                                           y="TodayPL",
                                                           group="Strategy",
                                                           colour="Strategy")) +
              geom_line(size=1)


            object <- .setOutputGGPlotData(object, ttl_pl_smrry)

            object <- .setOutputGGPlot(object, pl_plot)

            return(object)
          }
)
