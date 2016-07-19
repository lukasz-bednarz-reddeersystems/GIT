sourceTo("../analysis_modules/analysis_block/analysis_block.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/offside_positions_gain_vs_days/offside_positions_gain_vs_days.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/position_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

################################################################################
#
# OffsidePositionsCumulativePnLAnalysisBlock Class
# 
# Computation block class to pull data required for Computation of extended stock
# Pulls data required for computation and adds required columns.
###############################################################################

setClass(
  Class             = "OffsidePositionCumulativePnLData",
  prototype         = list(
    required_colnms = c( "Days", "TodayPL", "MarketRelPL")
  ),
  contains          = c("VirtualPositionData")
)

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

setMethod("setPositionDataObject",  
          signature(object = "OffsidePositionsCumulativePnLAnalysisBlock", position_data = "OffsidePositionGainData"),
          function(object, position_data){
            .setPositionDataObject(object, position_data)
          }
)


setMethod("Process",  
          signature(object = "OffsidePositionsCumulativePnLAnalysisBlock"),
          function(object, key_values){
            
            pos_data <- getPositionDataObject(object)
            
            # retrieve needed ref_data
            rank_offside <- getReferenceData(pos_data)
            
            pl_by_day <- aggregate(rank_offside[c('TodayPL','MarketRelPL')],list(Days=rank_offside$OffsideCnt),sum)
            pl_by_day <- pl_by_day[order(pl_by_day$Days),]
            
            cum_pl_plt <- rbind(cbind(Type='Abs. offside',data.frame(Days=pl_by_day$Days,PL=cumsum(pl_by_day$TodayPL))),
                                cbind(Type='Rel. offside',data.frame(Days=pl_by_day$Days,PL=cumsum(pl_by_day$MarketRelPL))))
            
            cum_pl_smmry <- ggplot(data=cum_pl_plt,aes(x=Days,y=PL,group=Type,colour=Type)) +
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
