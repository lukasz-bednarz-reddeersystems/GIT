#' @include analysis_block.r
NULL

################################################################################
#
# TradesPerformanceOnResultsDayAnalysisBlock Class
#
# Computation block class to pull data required for Computation
# performance of trades done on results day
###############################################################################
trd_num_cols <- c("Date"          , "InstrumentID"      , "TradeID"            , "Long"          ,
                  "TraderName"    , "ValueUSD"          , "Strategy"           , "StrategyID"    ,
                  "PsnLong"       , "ProfitTarget"      , "StopLoss"           , "PnLOutof"      ,
                  "PsnReturnOut"  , "PsnReturnOutFull"  , "Hit1D"              , "PtvePnLOutof"  ,
                  "PnLInto"       , "CompoundReturnInto", "CompoundReturnOutof", "PsnReturnIn"   ,
                  "VolInto"       , "VolOutof"          , "SkewInto"           , "SkewOutof"     ,
                  "ClosePrice"    , "PriorClosePrice"   , "PtvePnLInto"        , "PriceMavg"     ,
                  "MidOnEntry"    , "Offside"           , "RSI14"              , "PriorRSI14"    ,
                  "LegOpenClose"  , "RelativeRSI14"     , "SectorRelativeRSI14", "DailyN"        ,
                  "MavgPrice50"   , "Av.Age"            , "Av.PL"              , "Av.MarketValue",
                  "Av.Quantity"   , "Av.PsnReturn"      , "PsnRtnVol"          , "Total.PL"      ,
                  "Gm.PsnReturn"  , "PsnReturn"         , "InitialValue"       , "StockReturn"   ,
                  "RelativeReturn", "MarketValue"       , "TodayPL"            , "DeltaSwing"    ,
                  "DeltaSkew"     , "DeltaPL"           , "Age"                , "Category"      ,
                  "Quarter")
#' Trades on Results Reference Data class.,
#'
#' Concrete S4 class storing data trades happening on Results day
#'
#' Inherits from "VirtualTradeData"
#'
#' @export

setClass(
  Class             = "TradesOnResulsData",
  prototype         = list(
    required_colnms = trd_num_cols
  ),
  contains          = c("VirtualTradeData")
)

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
  Class             = "TradesPerformanceOnResultsDayAnalysisBlock",
  prototype         = list(
    key_cols        = c("TraderID", "start", "end"),
    key_values      = data.frame(TraderID = character(),
                              start    = as.Date(character()),
                              end    = as.Date(character())),
    column_name_map = hash(c("TraderID", "start", "end"), c("id", "start", "end")),
    output          = trd_num_cols
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualTradeDataHandler",
                        "VirtualPositionDataHandler",
                        "VirtualPriceDataHandler",
                        "VirtualEventDataHandler"
                        )
)


#' Set position_data object in object slot
#'
#' Public method to set position_data slot with "OffsidePositionData"
#' class object
#'
#' @rdname setPositionDataObject-TradesPerformanceOnResultsDayAnalysisBlock-method
#' @param object object of class "TradesPerformanceOnResultsDayAnalysisBlock"
#' @param position_data object of class "OffsidePositionData"
#' @return \code{object} object of class "TradesPerformanceOnResultsDayAnalysisBlock"
#' @export

setMethod("setPositionDataObject",
          signature(object = "TradesPerformanceOnResultsDayAnalysisBlock", position_data = "OffsidePositionData"),
          function(object, position_data){
            object <- TE.RefClasses:::.setPositionDataObject(object, position_data)
            return(object)
          }
)

#' Set trade_data object in object slot
#'
#' Public method to set trade_data slot with "TradeData"
#' class object
#'
#' @rdname setTradeDataObject-TradesPerformanceOnResultsDay-method
#' @param object object of class "TradesPerformanceOnResultsDayAnalysisBlock"
#' @param trade_data object of class "TradeData"
#' @return \code{object} object of class "TradesPerformanceOnResultsDayAnalysisBlock"
#' @export

setMethod("setTradeDataObject",
          signature(object = "TradesPerformanceOnResultsDayAnalysisBlock", trade_data = "TradeData"),
          function(object, trade_data){
            TE.RefClasses:::.setTradeDataObject(object, trade_data)
          }
)

#' Set price_data object in object slot
#'
#' Public method to set price_data slot with "PriceData"
#' class object
#'
#' @rdname setPriceDataObject-TradesPerformanceOnResultsDay-method
#' @param object object of class "TradesPerformanceOnResultsDayAnalysisBlock"
#' @param price_data object of class "PriceData"
#' @return \code{object} object of class "TradesPerformanceOnResultsDayAnalysisBlock"

setMethod("setPriceDataObject",
          signature(object = "TradesPerformanceOnResultsDayAnalysisBlock", price_data = "PriceData"),
          function(object, price_data){
            TE.RefClasses:::.setPriceDataObject(object, price_data)
          }
)

#' Set event_data object in object slot
#'
#' Public method to set event_data slot with "EventData"
#' class object
#'
#' @rdname setEventDataObject-TradesPerformanceOnResultsDay-method
#' @param object object of class "TradesPerformanceOnResultsDayAnalysisBlock"
#' @param event_data object of class "EventData"
#' @return \code{object} object of class "TradesPerformanceOnResultsDayAnalysisBlock"

setMethod("setEventDataObject",
          signature(object = "TradesPerformanceOnResultsDayAnalysisBlock", event_data = "EventData"),
          function(object, event_data){
            TE.RefClasses:::.setEventDataObject(object, event_data)
          }
)

#' Request data from data source
#'
#' @param object object of class 'TradesPerformanceOnResultsDayAnalysisBlock'.
#' @param key_values data.frame with keys specifying data query.
#' @return \code{object} object of class 'TradesPerformanceOnResultsDayAnalysisBlock'.

setMethod("dataRequest",
          signature(object = "TradesPerformanceOnResultsDayAnalysisBlock", key_values = "data.frame"),
          function(object, key_values){

            object <- TE.RefClasses:::.setDataSourceQueryKeyValues(object,key_values)

            trade_data <- getTradeDataObject(object)
            position_data <- getPositionDataObject(object)
            price_data <- getPriceDataObject(object)
            event_data <- getEventDataObject(object)

            trade_data_keys <- key_values
            colnames(trade_data_keys) <- TE.RefClasses:::.translateDataSourceColumnNames(object, colnames(key_values))
            id <- trade_data_keys[1,1]
            start <- min(trade_data_keys[[2]])
            end <- max(trade_data_keys[[3]])

            if (getStoredNRows(trade_data) == 0) {

              # retrieve trade reference data for query key_values
              trade_data <- tryCatch({
                dataRequest(trade_data, trade_data_keys)

              },error = function(cond){
                message(sprintf("Error when calling %s on %s class", "dataRequest()", class(trade_data)))
                message(sprintf("Querried for keys: id = %s, start = %s, end = %s", id, start, stop))
                stop(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(trade_data), cond))
              })

              # get price data
              object <- TE.RefClasses:::.setTradeDataObject(object, trade_data)

            }

            # retrieve position reference data for query key_values
            if (getStoredNRows(position_data) == 0) {

              # using AverageDownTradesAnalysisBlock to retrieve and process input data
              position_data <- tryCatch({
                dataRequest(position_data, trade_data_keys)
              },error = function(cond){
                message(sprintf("Error when calling %s on %s class", "dataRequest()", class(position_data)))
                message(sprintf("Querried for keys: id = %s, start = %s, end = %s", id, start, stop))
                stop(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(trade_data), cond))
              })
              object <- TE.RefClasses:::.setPositionDataObject(object, position_data)
            }

            if (getStoredNRows(price_data) == 0){

              trade_df <- getReferenceData(trade_data)
              InstrumentIDs <- unique(trade_df$InstrumentID)
              dates <- unique(trade_df$Date)

              price_key_vals <- expand.grid(InstrumentID = InstrumentIDs, Date = dates)

              price_data <- tryCatch({
                dataRequest(price_data, price_key_vals)

              },error = function(cond){
                message(sprintf("Error when calling %s on %s class", "dataRequest()", class(price_data)))
                message(sprintf("Querried for keys: id = %s, start = %s, end = %s", id, start, end))
                stop(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(price_data), cond))
              })

              object <- TE.RefClasses:::.setPriceDataObject(object, price_data)
            }

            # retrieve event reference data for query key_values
            if (getStoredNRows(event_data) == 0) {

              trade_df <- getReferenceData(trade_data)
              InstrumentIDs <- unique(trade_df$InstrumentID)
              dates <- unique(trade_df$Date)

              event_data_keys <- expand.grid(InstrumentID = InstrumentIDs, Date = dates)

              event_data <- tryCatch({
                dataRequest(event_data, event_data_keys)

              },error = function(cond){
                message(sprintf("Error when calling %s on %s class", "dataRequest()", class(event_data)))
                message(sprintf("Querried for keys: id = %s, start = %s, end = %s", id, start, stop))
                stop(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(event_data), cond))
              })

              object <- TE.RefClasses:::.setEventDataObject(object, event_data)
            }

            return(object)
          }
)



#' Trigger computation of analysis data.
#'
#' @param object object of class "TradesPerformanceOnResultsDayAnalysisBlock"
#' @return \code{object} object object of class "TradesPerformanceOnResultsDayAnalysisBlock"
#' @export

setMethod("Process",
          signature(object = "TradesPerformanceOnResultsDayAnalysisBlock"),
          function(object){


            trade_data    <- getTradeDataObject(object)
            position_data <- getPositionDataObject(object)
            event_data    <- getEventDataObject(object)


            # retrieve needed ref_data
            trades    <- getReferenceData(trade_data)
            positions <- getReferenceData(position_data)
            events    <- getReferenceData(event_data)
            prices    <- getReferenceData(price_data)


            trades <- merge(trades, positions[c("Date", "InstrumentID", "Age", "TraderID")])

            numbers_col <- c("Results")

            if (!(numbers_col %in% colnames(events))) {
              events[numbers_col] <- FALSE
            }


            # trades that trade on results
            trades <- merge(trades, events[c("Date", "InstrumentID", numbers_col)],
                                   by = c("Date", "InstrumentID"))

            # traded indexes
            trades_idx <- !is.na(trades$TradeID)

            # trades where the position is new on results day
            new_trades_idx <- trades_idx & trades$Results & (trades$Age == 0)

            trades$Category[new_trades_idx] <- "New Position"

            # trades where position is increased on results day
            incr_trades_idx <- trades_idx & trades$Results & (trades$Age > 0) & !xor(trades$Long, trades$PsnLong)

            trades$Category[incr_trades_idx] <- "Increase Position"

            # trades where position is decreased on results day
            # decr_trades_idx <- trades_idx & trades$Results & (trades$Age > 0) & xor(trades$Long, trades$PsnLong)
            #
            # trades$Category[decr_trades_idx] <- "Decrease Position"


            # trades where position is held on results day
            # hold_psn_idx <- !trades_idx & trades$Results & (trades$Age > 0)
            #
            # trades$Category[hold_psn_idx] <- "Hold Position"

            trades$Quarter <- quarter(trades$Date, with_year = TRUE)


            trades_ON <- aggregate(cbind(TodayPL, Age)~ Quarter + Category + TraderID , data = trades, mean)


            # # generate stock classifiers
            # x <- (trades$MidOnEntry-trades$PriceMavg)/(trades$MidOnEntry*trades$VolInto/10000)
            # trades$NFromMAVG20 <- clean_data(x, rm.outliers = "lofactor", scale = TRUE)
            #
            # x <- (trades$MidOnEntry-trades$MavgPrice50)/(trades$MidOnEntry*trades$DailyN/100)
            # trades$NFromMAVG50 <- clean_data(x, rm.outliers = "lofactor", scale = TRUE)
            #
            # trades$RSI14 <- clean_data(trades$RSI14, scale = TRUE)
            # trades$RelativeRSI14 <- clean_data(trades$RelativeRSI14, scale = TRUE)
            #
            # trades$Return <- clean_data(trades$TodayPL/trades$MarketValue)
            #
            # # save trades for future use
            # trade_rd <- setReferenceData(trade_rd, trades)
            # object <- TE.RefClasses:::.setTradeDataObject(object,trade_rd)
            #
            # # compute extended trades
            # distance_measure <- 'NFromMAVG20'
            # rsi_measure <- 'RSI14'
            # thr <- 1
            #
            # trd <- trades[!is.na(trades[distance_measure])&!is.na(trades[rsi_measure]),]
            # extended_trades <- trd[trd$Long==1&(trd[distance_measure]>thr|trd[rsi_measure]>thr),]
            # extended_trades <- rbind(extended_trades,trd[trd$Long==0&(trd[distance_measure]<(-thr)|trd[rsi_measure]<(-thr)),])
            #
            # outp_object <- getOutputObject(object)
            # outp_object <- setReferenceData(outp_object, extended_trades)
            #
            # object <- .setOutputObject(object, outp_object)
            #
            # plot_trades <- rbind(cbind(Measure='NFromMAVG20',trades[c('Strategy','Long')],Value=trades$NFromMAVG20),
            #                      cbind(Measure='NFromMAVG50',trades[c('Strategy','Long')],Value=trades$NFromMAVG50),
            #                      cbind(Measure='RSI14',trades[c('Strategy','Long')],Value=trades$RSI14),
            #                      cbind(Measure='RelativeRSI14',trades[c('Strategy','Long')],Value=trades$RelativeRSI14))

            browser()

            # trades_pl <- ggplot(data=trades, aes_string(x="Category",
            #                                                   y="TodayPL",
            #                                                   color="Category"
            # )
            # ) +
            #   geom_boxplot() +
            #   coord_flip() +
            #   guides(color = FALSE)

            trades_pl <- ggplot(data=trades_ON, aes_string(x="as.character(Quarter)",
                                                           fill="Category"
            )
            ) +
              geom_bar(aes_string(weight = "TodayPL"), position= "dodge") +
              ylab("Average PnL (USD)") +
                xlab("Quarter") +
                ggtitle(sprintf('Average PnL on Results day for trader ID: %s', unique(trades$TraderID)))


            object <- .setOutputGGPlotData(object, trades_ON)
            object <- .setOutputGGPlot(object, trades_pl)
            object <- .setOutputFrontendData(object, data.frame(omit = c("TodayPL")))


            return(object)
          }
)
