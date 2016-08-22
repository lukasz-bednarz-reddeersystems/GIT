#' @include offside_positions_gain_vs_days.r
NULL

################################################################################
#
# OffsidePositionsCumulativePnLAnalysisBlock Class
#
# Computation block class to pull data required for Computation of extended stock
# Pulls data required for computation and adds required columns.
###############################################################################

#' OffsidePositionCumulativePnLData Reference Data class.
#'
#' Concrete S4 class storing data of Offside Market Positions
#' cumulative PnL
#' Generated only by OffsidePositionsCumulativePnLAnalysisBlock module.
#'
#' Inherits from "VirtualPositionData"
#'
#' @export
setClass(
  Class             = "OffsidePositionCumulativePnLData",
  prototype         = list(
    required_colnms = c( "Days", "TodayPL", "MarketRelPL")
  ),
  contains          = c("VirtualPositionData")
)


#' Analysis Module for computation of Market Offside Positions PnL
#'
#' Computation block class to pull data required for Computation of offside positions
#' Pulls data required for computation and adds required columns.
#'
#' Inherits from "VirtualAnalysisBlock",
#'               "VirtualPositionDataHandler"
#' @export

setClass(
  Class             = "OffsidePositionsCumulativePnLAnalysisBlock",
  slots             = c(
    position_data   = "OffsidePositionGainData"
  ),
  prototype         = list(
    required_colnms = c( "InstrumentID",          "CumulativePL",          "CumulativeMarketRelPL", "TodayPL" ,
                         "MarketRelPL",           "OffsideCnt",            "RelOffsideCnt",         "MarketValue",
                         "Offside",               "OffsideRel",            "Gain",                  "RelGain"),
    position_data   = new("OffsidePositionGainData"),
    output          = new("OffsidePositionCumulativePnLData")
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualPositionDataHandler"
                        )
)


#' Set position_data object in object slot
#'
#' Public method to set position_data slot with "OffsidePositionGainData"
#' class object
#'
#' @rdname setPositionDataObject-OffsidePositionsCumulativePnLAnalysisBlock-method
#' @param object object of class "OffsidePositionsCumulativePnLAnalysisBlock"
#' @param position_data object of class "OffsidePositionGainData"
#' @return \code{object} object of class "OffsidePositionsCumulativePnLAnalysisBlock"
#' @export

setMethod("setPositionDataObject",
          signature(object = "OffsidePositionsCumulativePnLAnalysisBlock", position_data = "OffsidePositionGainData"),
          function(object, position_data){
            object <- TE.RefClasses:::.setPositionDataObject(object, position_data)
            return(object)
          }
)


#' Trigger computation of analysis data.
#'
#'
#' @param object object of class "OffsidePositionsCumulativePnLAnalysisBlock"
#' @return \code{object} object object of class "OffsidePositionsCumulativePnLAnalysisBlock"
#' @export

setMethod("Process",
          signature(object = "OffsidePositionsCumulativePnLAnalysisBlock"),
          function(object){

            pos_data <- getPositionDataObject(object)

            # retrieve needed ref_data
            rank_offside <- getReferenceData(pos_data)

            pl_by_day <- aggregate(rank_offside[c('TodayPL','MarketRelPL')],list(Days=rank_offside$OffsideCnt),sum)
            pl_by_day <- pl_by_day[order(pl_by_day$Days),]

            cum_pl_plt <- rbind(cbind(Type='Abs. offside',data.frame(Days=pl_by_day$Days,PL=cumsum(pl_by_day$TodayPL))),
                                cbind(Type='Rel. offside',data.frame(Days=pl_by_day$Days,PL=cumsum(pl_by_day$MarketRelPL))))

            cum_pl_smmry <- ggplot(data=cum_pl_plt,aes_string(x="Days",y="PL",group="Type",colour="Type")) +
              geom_line(size=1) +
              ylab("Cumulative PL $") +
              xlab("Total days position offside") +
              labs(colour="") +
              theme(legend.position = "bottom") +
              theme(text = element_text(size=15)) +
              scale_colour_brewer(palette="Set1") +
              ggtitle('Cumulative PL by days offside')


            # set processed data as an output
            object <- setReferenceData(object, rank_offside)

            outp_object <- getOutputObject(object)
            outp_object <- setReferenceData(outp_object, pl_by_day)
            object <- .setOutputObject(object, outp_object)


            object <- .setOutputGGPlotData(object, cum_pl_plt)
            object <- .setOutputGGPlot(object, cum_pl_smmry)


            return(object)
          }
)
