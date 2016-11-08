#' @include offside_positions.r
NULL

################################################################################
#
# OffsidePositionsBpsPerMonthAnalysisBlock Class
#
# Computation block class to pull data required for Computation of extended stock
# Pulls data required for computation and adds required columns.
###############################################################################


#' OffsidePositionRelativeBpsData Reference Data class.
#'
#' Concrete S4 class storing data of Offside Market Positions
#' Relative Gain in terms of Base Position Points (BPS)
#' Generated only by OffsidePositionsBpsPerMonthAnalysisBlock module.
#'
#' Inherits from "VirtualPositionData"
#'
#' @export

setClass(
  Class             = "OffsidePositionRelativeBpsData",
  prototype         = list(
    required_colnms = c('Date','Offside','NPos','BpsOff','DaysOff','MarketValue')
  ),
  contains          = c("VirtualPositionData")
)


#' Analysis Module for computation of Market Offside Positions
#' Relative Gain in terms of Base Position Points (BPS)
#'
#' Computation block class to pull data required for Computation of offside positions
#' Pulls data required for computation and adds required columns.
#'
#' Inherits from "VirtualAnalysisBlock",
#'               "VirtualPositionDataHandler"
#' @export

setClass(
  Class             = "OffsidePositionsBpsPerMonthAnalysisBlock",
  slots             = c(
    position_data   = "OffsidePositionData",
    output          = "OffsidePositionRelativeBpsData"
  ),
  prototype         = list(
    required_colnms = c('InstrumentID','Date','CumulativePL','CumulativeMarketRelPL','MarketRelPL','MarketValue','Strategy',
                        'RelOff', 'DaysOff', 'BpsOff'),
    position_data   = new("OffsidePositionData"),
    output          = new("OffsidePositionRelativeBpsData")
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualPositionDataHandler"
                        )
)

#' @describeIn setPositionDataObject
#' Set position_data object in object slot
#' @inheritParams setPositionDataObject
#'
# ' @rdname setPositionDataObject-OffsidePositionsBpsPerMonthAnalysisBlock-method
# ' @param object object of class "OffsidePositionsBpsPerMonthAnalysisBlock"
# ' @param position_data object of class "OffsidePositionData"
# ' @return \code{object} object of class "OffsidePositionsBpsPerMonthAnalysisBlock"
#' @export

setMethod("setPositionDataObject",
          signature(object = "OffsidePositionsBpsPerMonthAnalysisBlock", position_data = "OffsidePositionData"),
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
# ' @param object object of class "OffsidePositionsBpsPerMonthAnalysisBlock"
# ' @return \code{object} object object of class "OffsidePositionsBpsPerMonthAnalysisBlock"
#' @export

setMethod("Process",
          signature(object = "OffsidePositionsBpsPerMonthAnalysisBlock"),
          function(object){

            pos_data <- getPositionDataObject(object)

            # retrieve needed ref_data
            history_data <- getReferenceData(pos_data)

            hd <- unique(history_data[c('InstrumentID','Date','CumulativePL','CumulativeMarketRelPL','MarketRelPL','MarketValue','Strategy')])

            # remove hedges
            hd <- hd[!grepl('_SHEDGE$', hd$Strategy),]

            hd$RelOff <- hd$CumulativeMarketRelPL < 0
            hd$DaysOff<- NA
            for(ins in unique(hd$InstrumentID)){
              hd[hd$InstrumentID==ins,]$DaysOff <- cumsum(hd[hd$InstrumentID==ins,]$RelOff)
            }
            hd$BpsOff <- 10000*(hd$CumulativePL/abs(hd$MarketValue))
            hd$BpsOff[is.infinite(hd$BpsOff)] <- NA
            hd$BpsOff[is.nan(hd$BpsOff)] <- NA

            # set reference data
            object <- setReferenceData(object, hd)

            track_offside <- with(hd,aggregate(InstrumentID,list(Date=Date,Offside=RelOff),function(x)length(unique(x))))
            track_offside <- merge(track_offside,aggregate(hd[c('BpsOff','DaysOff')],list(Date=hd$Date,Offside=hd$RelOff),function(x)mean(x,na.rm=TRUE)),by=c('Date','Offside'))
            track_offside <- merge(track_offside,aggregate(hd[c('MarketValue')],list(Date=hd$Date,Offside=hd$RelOff),function(x)median(abs(x),na.rm=TRUE)),by=c('Date','Offside'))
            colnames(track_offside) <- c('Date','Offside','NPos','BpsOff','DaysOff','MarketValue')

            # set output data
            outp_object <- getOutputObject(object)
            outp_object <- setReferenceData(outp_object, track_offside)
            object <- .setOutputObject(object, outp_object)

            # generate plot data
            track_offside <- rbind(cbind(Quantity="Fraction offside",data.frame(Date=track_offside$Date,Value=(track_offside[track_offside$Offside,]$NPos/(track_offside[track_offside$Offside,]$NPos+track_offside[!track_offside$Offside,]$NPos)),Days=track_offside[track_offside$Offside,]$DaysOff,MarketValue=track_offside[track_offside$Offside,]$MarketValue)),
                                   cbind(Quantity="Bps Offside",data.frame(Date=track_offside$Date,Value=track_offside[track_offside$Offside,]$BpsOff,Days=track_offside[track_offside$Offside,]$DaysOff,MarketValue=track_offside[track_offside$Offside,]$MarketValue)))
            track_offside$Date <- format(track_offside$Date,'%Y-%m')
            track_offside <- aggregate(track_offside[c('Value','Days','MarketValue')],list(Date=track_offside$Date,Quantity=track_offside$Quantity),function(x)mean(x,na.rm=TRUE))

            track_offside_plt <-ggplot(data=track_offside[track_offside$Quantity=="Bps Offside",],
                                       aes_string(x="Date",y="Value",size="MarketValue")) +
              geom_point(aes_string(colour="Days")) +
              theme(text = element_text(size=15)) +
              ylab("Bps offside") +
              xlab("Month") +
              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
              scale_size_continuous(guide = FALSE) +
              labs(colour="Av. days offside") +
              scale_colour_distiller(palette="Spectral") +
              ggtitle('Av. bps offside of relative offside positions by month')

            # set processed data as an output
            object <- .setOutputGGPlotData(object, track_offside)
            object <- .setOutputGGPlot(object, track_offside_plt)
            object <- .setOutputFrontendData(object, data.frame(omit = c("MarketValue", "Value", "Days")))

            return(object)
          }
)
