#' @include trades_performance_on_results_day.r
NULL

################################################################################
#
# TradesPnLOutOfOnResultsDayAnalysisBlock Class
#
# Computation block class to pull data required for Computation
# performance of trades done on results day
###############################################################################



#' Analysis Module for extraction of performance of trades on results day
#'
#' Computation block class compute data for
#' long and short time performance of trades done on result day.
#' Specificaly it looks on performance of stock that was
#'
#' Inherits from "VirtualAnalysisBlock",
#'               "VirtualTradeDataHandler",
#'               "VirtualPriceDataHandler"
#'
#' @export

setClass(
  Class             = "TradesPnLOutOfOnResultsDayAnalysisBlock",
  slots             = c(
    trade_data      = "TradesOnResultsDayData"
  ),
  prototype         = list(
    key_cols        = c("TraderID", "start", "end"),
    key_values      = data.frame(TraderID = character(),
                              start    = as.Date(character()),
                              end    = as.Date(character())),
    column_name_map = hash(c("TraderID", "start", "end"), c("id", "start", "end")),
    trade_data      = new("TradesOnResultsDayData")
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualTradeDataHandler"
                        )
)


#' Set trade_data object in object slot
#'
#' Public method to set trade_data slot with "TradesOnResultsDayData"
#' class object
#'
#' @rdname setTradeDataObject-TradesPnLOutOfOnResultsDay-method
#' @param object object of class "TradesPnLOutOfOnResultsDayAnalysisBlock"
#' @param trade_data object of class "TradesOnResultsDayData"
#' @return \code{object} object of class "TradesPnLOutOfOnResultsDayAnalysisBlock"
#' @export

setMethod("setTradeDataObject",
          signature(object = "TradesPnLOutOfOnResultsDayAnalysisBlock", trade_data = "TradesOnResultsDayData"),
          function(object, trade_data){
            TE.RefClasses:::.setTradeDataObject(object, trade_data)
          }
)

#' Request data from data source
#'
#' @param object object of class 'TradesPnLOutOfOnResultsDayAnalysisBlock'.
#' @param key_values data.frame with keys specifying data query.
#' @return \code{object} object of class 'TradesPnLOutOfOnResultsDayAnalysisBlock'.

setMethod("dataRequest",
          signature(object = "TradesPnLOutOfOnResultsDayAnalysisBlock", key_values = "data.frame"),
          function(object, key_values){

            object <- TE.RefClasses:::.setDataSourceQueryKeyValues(object,key_values)

            trade_data <- getTradeDataObject(object)

            if (getStoredNRows(trade_data) == 0) {

              trades_on_numbers.an <- new("TradesPerformanceOnResultsDayAnalysisBlock")
              trades_on_numbers.an <- dataRequest(trades_on_numbers.an, key_values)

              trades_on_numbers.an <- Process(trades_on_numbers.an)

              trades_on_numbers.rd <- getOutputObject(trades_on_numbers.an)

              # get price data
              object <- TE.RefClasses:::.setTradeDataObject(object, trades_on_numbers.rd)

            }

            return(object)
          }
)



#' Trigger computation of analysis data.
#'
#' @param object object of class "TradesPnLOutOfOnResultsDayAnalysisBlock"
#' @return \code{object} object object of class "TradesPnLOutOfOnResultsDayAnalysisBlock"
#' @export

setMethod("Process",
          signature(object = "TradesPnLOutOfOnResultsDayAnalysisBlock"),
          function(object){

            trade_data    <- getTradeDataObject(object)

            # retrieve needed ref_data
            trades    <- getReferenceData(trade_data)

            trades_mean <- aggregate(PnLOutof ~ Quarter + Category + TraderID + Classification + Long,
                                   data = trades, mean)
            trades_mean$Quantity <- "Average"

            trades_sum <- aggregate(PnLOutof ~ Quarter + Category + TraderID + Classification + Long,
                                   data = trades, sum)
            trades_sum$Quantity <- "Total"

            trades_ON <- rbind(trades_mean, trades_sum)

            trades_ON$Direction[trades_ON$Long] <- "Long"
            trades_ON$Direction[!trades_ON$Long] <- "Short"

            trades_ON$Long <- NULL


            panel <- expand.grid(Quarter        = unique(trades_ON$Quarter),
                                 Category       = unique(trades_ON$Category),
                                 TraderID       = unique(trades_ON$TraderID),
                                 Classification = unique(trades_ON$Classification),
                                 Quantity       = unique(trades_ON$Quantity),
                                 Direction      = unique(trades_ON$Direction))

            trades_ON <- merge(trades_ON, panel, all.y = TRUE)

            trades_ON[is.na(trades_ON)] <- 0

            trades_pl <- ggplot(data=trades_ON, aes_string(x="as.character(Quarter)",
                                                           fill="paste(Category , Direction)"
                                                          )
                                ) +
                        geom_bar(aes_string(weight = "PnLOutof"), position= "dodge") +
                        facet_grid(Quantity ~ Classification, scales = "free_y") +
                        theme_dark() +
                        #theme(plot.background  = element_rect(fill = "black", colour = "black")) +
                        guides(fill = guide_legend(title = "Category")) +
                        ylab("PnL (USD)") +
                        xlab("Quarter") +
                        ggtitle(sprintf('PnL OutOf on Results day for trader ID: %s', na.omit(unique(trades$TraderName))))


            object <- .setOutputGGPlotData(object, trades_ON)
            object <- .setOutputGGPlot(object, trades_pl)
            object <- .setOutputFrontendData(object, data.frame(omit = c("PnLOutof")))


            return(object)
          }
)
