#' @include average_down_trades.r
NULL

################################################################################
#
# AverageDownTradesFocusAnalysisBlock Class
#
# Computation block class to pull data required for Computation of extended stock
# Pulls data required for computation and adds required columns.
###############################################################################


#' Analysis Module for extraction of Average Down Trades
#'
#' Computation block class compute summary focus data for
#' average down trades.
#'
#' Inherits from "VirtualAnalysisBlock",
#'               "VirtualTradeDataHandler"
#'
#' @export

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


#' Set trade_data object in object slot
#'
#' Public method to set trade_data slot with "AverageDownTradesData"
#' class object
#'
#' @rdname setTradeDataObject-AverageDownTradesFocusAnalysisBlock-method
#' @param object object of class "AverageDownTradesFocusAnalysisBlock"
#' @param trade_data object of class "AverageDownTradesData"
#' @return \code{object} object of class "AverageDownTradesFocusAnalysisBlock"
#' @export

setMethod("setTradeDataObject",
          signature(object = "AverageDownTradesFocusAnalysisBlock", trade_data = "AverageDownTradesData"),
          function(object, trade_data){
            object <- TE.RefClasses:::.setTradeDataObject(object, trade_data)
            return(object)
          }
)


#' Trigger computation of analysis data.
#'
#' @param object object of class "OffsidePositionsBpsPerMonthAnalysisBlock"
#' @return \code{object} object object of class "OffsidePositionsBpsPerMonthAnalysisBlock"
#' @export

setMethod("Process",
          signature(object = "AverageDownTradesFocusAnalysisBlock"),
          function(object){

            trade_data <- getTradeDataObject(object)

            # retrieve needed ref_data
            average_down_trades <- getReferenceData(trade_data)

            average_down_trades <- unique(average_down_trades[c('InstrumentID','Date','ValueUSD','TodayPL')])
            average_down_trades$Date <- format(average_down_trades$Date,'%Y-%m')
            adown_focus <- aggregate(average_down_trades['InstrumentID'],list(Date=average_down_trades$Date),function(x)length(x))
            adown_focus <- merge(adown_focus,aggregate(average_down_trades[c('ValueUSD','TodayPL')],list(Date=average_down_trades$Date),function(x)mean(x,na.rm=TRUE)),by='Date')

            adown_focus_plt <- ggplot(data=adown_focus,aes_string(x="Date",y="InstrumentID",size="ValueUSD")) +
              geom_point(aes_string(colour="TodayPL")) +
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
