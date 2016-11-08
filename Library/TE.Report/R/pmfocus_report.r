#' @include report_analysis_block.r
NULL
################################################################################
#
# PMFocusReport Class
#
# Computation block class to generate plots required for the PM focus
# section ot MBAM trading review
###############################################################################

#' List of Analysis modules used by PMFocusReport Class
pm_focus_report_analysis_blocks <- c(
                                      "OffsidePositionsBpsPerMonth",
                                      "AverageDownTradesFocus",
                                      "PositionsHoldingDayZeroPnL"
                                    )

#' PM Focus Report class.
#'
#' Report class computing following blocks:
#'  "OffsidePositionsBpsPerMonth",
#'  "AverageDownTradesFocus",
#'  "PositionsHoldingDayZeroPnL"
#' @export

setClass(
  Class             = "PMFocusReport",
  prototype         = list(
    key_cols        = c("TraderID", "start", "end"),
    key_values      = data.frame(TraderID = character(),
                              start    = as.Date(character()),
                              end    = as.Date(character())),
    column_name_map = hash(c("TraderID", "start", "end"), c("id", "start", "end")),
    ggplot_list        = list(),
    ggplot_data_list   = list(),
    frontend_data_list = list(),
    output_list        = list()
  ),
  contains          = c("VirtualReportAnalysisBlock"
                        )
)

#' Request data from data source
#'
#' @param object object of class 'PMFocusReport'.
#' @param key_values data.frame with keys specifying data query.
#' @return \code{object} object of class 'TradingReview2016.2Report'.
#' @export
setMethod("dataRequest",
          signature(object = "PMFocusReport", key_values = "data.frame"),
          function(object, key_values){

            object <- setDataSourceQueryKeyValues(object,key_values)

            return(object)
          }
)

#' Trigger computation of report data.
#'
#' @param object object of class "PMFocusReport"
#' @return \code{object} object object of class "PMFocusReport"
#' @export
setMethod("Process",
          signature(object = "PMFocusReport"),
          function(object){

            # retrieve query keys
            key_values <- getDataSourceQueryKeyValues(object)

            ######################################################
            #
            # OffsidePositionsAnalysisBlock xx
            #
            ######################################################

            # create/get data/process offside positions analyzer
            offside.pos.an <- new("OffsidePositionsAnalysisBlock")

            # gets position and price data
            offside.pos.an <- dataRequest(offside.pos.an, key_values)

            #process
            offside.pos.an <- Process(offside.pos.an)

            offside.pos.rd <- getOutputObject(offside.pos.an)

            # set processed result
            object <- .copyAnalyzerOutputData(object, offside.pos.an)

            ######################################################
            #
            # "OffsidePositionsBpsPerMonthAnalysisBlock" xx
            #
            ######################################################
            # create analyzer
            offside.bps.an <- new("OffsidePositionsBpsPerMonthAnalysisBlock")

            # set needed ref_data from previous analysis block
            offside.bps.an <- setPositionDataObject(offside.bps.an, offside.pos.rd)

            # process
            offside.bps.an <- Process(offside.bps.an)

            # set processed result
            object <- .copyAnalyzerOutputData(object, offside.bps.an)


            ######################################################
            #
            # "AverageDownTradesAnalysisBlock" x
            #
            ######################################################
            # create analyzer
            avg.down.trd.an <- new("AverageDownTradesAnalysisBlock")

            # set needed ref_data from previous analysis block
            avg.down.trd.an <- setPositionDataObject(avg.down.trd.an, offside.pos.rd)

            # get additional trade data
            avg.down.trd.an <- dataRequest(avg.down.trd.an, key_values)

            # store row trade data for use by other blocks
            trade.data.rd   <- getTradeDataObject(avg.down.trd.an)

            # process
            avg.down.trd.an <- Process(avg.down.trd.an)

            # retreive data for later block
            avg.down.trd.rd <- getOutputObject(avg.down.trd.an)

            # set processed result
            #object <- .copyAnalyzerOutputData(object, avg.down.trd.an)

            ######################################################
            #
            # "AverageDownTradesFocusAnalysisBlock" xx
            #
            ######################################################
            # create analyzer
            avg.down.fcs.an <- new("AverageDownTradesFocusAnalysisBlock")

            # set needed ref_data from previous analysis block
            avg.down.fcs.an <- setTradeDataObject(avg.down.fcs.an, avg.down.trd.rd)

            # process
            avg.down.fcs.an <- Process(avg.down.fcs.an)

            # set processed result
            object <- .copyAnalyzerOutputData(object, avg.down.fcs.an)


            ######################################################
            #
            # "PositionsHoldingDayZeroPnLAnalysisBlock" xx
            #
            ######################################################
            # create analyzer
            pos.hold.d0.an <- new("PositionsHoldingDayZeroPnLAnalysisBlock")

            # set data computed in previous blocks
            pos.hold.d0.an <- setPositionDataObject(pos.hold.d0.an, offside.pos.rd)
            pos.hold.d0.an <- setTradeDataObject(pos.hold.d0.an, trade.data.rd)

            # process
            pos.hold.d0.an <- Process(pos.hold.d0.an)

            # retreive data for later block
            offside.pos.rd <- getPositionDataObject(pos.hold.d0.an)

            # set processed result
            object <- .copyAnalyzerOutputData(object, pos.hold.d0.an)

            return(object)
          }
)
