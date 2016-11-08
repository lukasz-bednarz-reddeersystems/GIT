#' @include offside_positions.r
NULL

################################################################################
#
# OffsidePositionsGainVsDaysAnalysisBlock Class
#
# Computation block class to pull data required for Computation of extended stock
# Pulls data required for computation and adds required columns.
###############################################################################


#' OffsidePositionGainData Reference Data class.
#'
#' Concrete S4 class storing data of Offside Market Positions
#' gain Data
#' Generated only by OffsidePositionsGainVsDaysAnalysisBlock module.
#'
#' Inherits from "VirtualPositionData"
#'
#' @export
setClass(
  Class             = "OffsidePositionGainData",
  prototype         = list(
    required_colnms = c( "InstrumentID",          "CumulativePL",          "CumulativeMarketRelPL", "TodayPL" ,
                         "MarketRelPL",           "OffsideCnt",            "RelOffsideCnt",         "MarketValue",
                         "Offside",               "OffsideRel",            "Gain",                  "RelGain")
  ),
  contains          = c("VirtualPositionData")
)


#' Analysis Module for computation of Market Offside Positions Gain
#'
#' Computation block class to pull data required for Computation of
#' offside positions Gain vs Days
#'
#' Pulls data required for computation and adds required columns.
#'
#' Inherits from "VirtualAnalysisBlock",
#'               "VirtualPositionDataHandler"
#' @export

setClass(
  Class             = "OffsidePositionsGainVsDaysAnalysisBlock",
  slots             = c(
    position_data   = "OffsidePositionData"
  ),
  prototype         = list(
    required_colnms = c('Date','InstrumentID','CumulativePL','CumulativeMarketRelPL','MarketRelPL',
                        'TodayPL','MarketValue','MinDate','PsnAge', "OffsideCnt", "RelOffsideCnt"),
    position_data   = new("OffsidePositionData"),
    output          = new("OffsidePositionGainData")
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualPositionDataHandler"
                        )
)

#' @describeIn setPositionDataObject
#' Set position_data object in object slot
#' @inheritParams setPositionDataObject
#'
# ' @rdname setPositionDataObject-OffsidePositionsGainVsDaysAnalysisBlock-method
# ' @param object object of class "OffsidePositionsGainVsDaysAnalysisBlock"
# ' @param position_data object of class "OffsidePositionData"
# ' @return \code{object} object of class "OffsidePositionsGainVsDaysAnalysisBlock"
#' @export

setMethod("setPositionDataObject",
          signature(object = "OffsidePositionsGainVsDaysAnalysisBlock", position_data = "OffsidePositionData"),
          function(object, position_data){
            object <- TE.RefClasses:::.setPositionDataObject(object, position_data)
            return(object)
          }
)


#' @describeIn Process
#'
#' Trigger computation of analysis data.
#'
#' @inheritParams Process
#'
#'
# ' @param object object of class "OffsidePositionsGainVsDaysAnalysisBlock"
# ' @return \code{object} object object of class "OffsidePositionsGainVsDaysAnalysisBlock"
#' @export

setMethod("Process",
          signature(object = "OffsidePositionsGainVsDaysAnalysisBlock"),
          function(object){

            pos_data <- getPositionDataObject(object)

            # retrieve needed ref_data
            history_data <- getReferenceData(pos_data)

            hd <- unique(history_data[c('Date','InstrumentID','CumulativePL','CumulativeMarketRelPL','MarketRelPL','TodayPL','MarketValue','MinDate','PsnAge')])
            hd <- hd[hd$PsnAge<50,]
            hd$OffsideCnt <- hd$CumulativePL < 0
            hd$RelOffsideCnt <- hd$CumulativeMarketRelPL < 0

            # saving data to as reference data
            object <- setReferenceData(object, hd)

            rank_offside <- aggregate(hd[c('CumulativePL','CumulativeMarketRelPL')],list(InstrumentID=hd$InstrumentID),function(x)min(x,na.rm=TRUE))
            rank_offside <- merge(rank_offside,
                                  aggregate(hd[c('TodayPL','MarketRelPL','OffsideCnt','RelOffsideCnt')],list(InstrumentID=hd$InstrumentID),function(x)sum(x,na.rm=TRUE)),
                                  by = c('InstrumentID'))
            rank_offside <- merge(rank_offside,
                                  aggregate(hd[c('MarketValue')],list(InstrumentID=hd$InstrumentID),function(x)mean(abs(x),na.rm=TRUE)),
                                  by = c('InstrumentID'))
            rank_offside$Offside <- 100*(rank_offside$CumulativePL/abs(rank_offside$MarketValue))
            rank_offside$OffsideRel <- 100*(rank_offside$CumulativeMarketRelPL/abs(rank_offside$MarketValue))
            rank_offside <- rank_offside[rank_offside$OffsideCnt<50,]
            rank_offside$Gain <- rank_offside$TodayPL > 0
            rank_offside$RelGain <- rank_offside$MarketRelPL > 0
            rank_plt_data <- with(rank_offside,rbind(cbind(Type='Absolute Offside',data.frame(DaysOffside=OffsideCnt,PcntOffside=Offside,Size=MarketValue,WinLoss=Gain)),
                                                     cbind(Type='Relative Offside',data.frame(DaysOffside=OffsideCnt,PcntOffside=OffsideRel,Size=MarketValue,WinLoss=RelGain))))
            off_rank <- ggplot(data=rank_plt_data,aes_string(x="DaysOffside",y="PcntOffside",size="Size")) +
              geom_point(aes(colour="WinLoss")) +
              ylim(c(-200,200)) +
              labs(size='Av. Size $',colour='Positive PL') +
              theme(text = element_text(size=15)) +
              ylab("% offside") +
              xlab("Total days position offside") +
              theme(legend.position = "bottom") +
              ggtitle('Position PL in buckets of total days offside')  +
              facet_grid(Type~.)

            # set processed data as an output

            outp_object <- getOutputObject(object)
            outp_object <- setReferenceData(outp_object, rank_offside)
            object <- .setOutputObject(object, outp_object)


            object <- .setOutputGGPlotData(object, rank_plt_data)
            object <- .setOutputGGPlot(object, off_rank)
            object <- .setOutputFrontendData(object, data.frame(omit = c("PcntOffside", "Size", "WinLoss")))

            return(object)
          }
)
