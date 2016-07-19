sourceTo("../analysis_modules/analysis_block/analysis_block.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/offside_positions/offside_positions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)


################################################################################
#
# PositionsHoldingPeriodAnalysisBlock Class
# 
# Computation block class to pull data required for Computation of position holding period
# Pulls data required for computation and adds required columns.
###############################################################################


setClass(
  Class             = "PositionsHoldingPeriodAnalysisBlock",
  slots             = c(
    position_data   = "OffsidePositionData"
  ),
  prototype         = list(
    required_colnms = c('Date','InstrumentID','TodayPL','PassiveTodayPL','ActiveTodayPL',
                        'MarketRelPL','MinDate','MarketValue','PsnAge'),
    position_data   = new("OffsidePositionData")
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualPositionDataHandler"
                        )
)

setMethod("setPositionDataObject",  
          signature(object = "PositionsHoldingPeriodAnalysisBlock", position_data = "OffsidePositionData"),
          function(object, position_data){
            .setPositionDataObject(object, position_data)
          }
)


setMethod("dataRequest",
          signature(object = "PositionsHoldingPeriodAnalysisBlock", key_values = "data.frame"),
          function(object, key_values){
            
            object <- .setDataSourceQueryKeyValues(object,key_values)
            
            trader <- unique(key_values$TraderID)[1]
            start <- min(key_values$start)
            end <- max(key_values$end)
            
            req_key_vals <- data.frame(id = trader, start = start, end = end)
            
            # retrieve position reference data for query key_values
            position_data <- getPositionDataObject(object)
            
            if (getStoredNRows(position_data) == 0) {
              
              # using AverageDownTradesAnalysisBlock to retrieve and process input data
              offside.pos.an <- new("OffsidePositionsAnalysisBlock")
              offside.pos.an <- dataRequest(offside.pos.an, key_values)
              offside.pos.an <- Process(offside.pos.an)
              offside.pos.rd <- getOutputObject(offside.pos.an)
              object <- .setPositionDataObject(object, offside.pos.rd)
            }
            
            return(object)
          }
)

setMethod("Process",  
          signature(object = "PositionsHoldingPeriodAnalysisBlock"),
          function(object, key_values){
            
            pos_data <- getPositionDataObject(object)
            
            # retrieve needed ref_data
            history_data <- getReferenceData(pos_data)

            pl_hd <- unique(history_data[c('Date','InstrumentID','TodayPL','PassiveTodayPL','ActiveTodayPL','MarketRelPL','MinDate','MarketValue','PsnAge')])
            
            object <- setReferenceData(object, pl_hd)
            
            pl_hd <- pl_hd[pl_hd$PsnAge<70,]
            
            pl_hd$Indicator <- 1
            pl_by_age <- aggregate(pl_hd[c('TodayPL','PassiveTodayPL','ActiveTodayPL','MarketRelPL','Indicator')],list(Age=pl_hd$PsnAge),function(x)sum(x,na.rm=TRUE))
            pl_by_age <- merge(pl_by_age,aggregate(pl_hd['MarketValue'],list(Age=pl_hd$PsnAge),function(x)mean(abs(x),na.rm=TRUE)),by='Age')
            pl_by_age <- clean_df_column_data(pl_by_age, c("TodayPL", "PassiveTodayPL", "ActiveTodayPL", "MarketRelPL"))
            pl_by_age[is.na(pl_by_age)] <- 0
            
            plt_cum_pl_data <- rbind(cbind(Type='Cumulative PL',Quantity='Total PL',data.frame(Age=pl_by_age$Age,PL=cumsum(pl_by_age$TodayPL))),
                                     cbind(Type='Cumulative PL',Quantity='MarketRel PL',data.frame(Age=pl_by_age$Age,PL=cumsum(pl_by_age$MarketRelPL))),
                                     cbind(Type='Cumulative PL',Quantity='Passive PL',data.frame(Age=pl_by_age$Age,PL=cumsum(pl_by_age$PassiveTodayPL))),
                                     cbind(Type='Cumulative PL',Quantity='Active PL',data.frame(Age=pl_by_age$Age,PL=cumsum(pl_by_age$ActiveTodayPL))),
                                     cbind(Type='Av. Market Value',Quantity='$ Value',data.frame(Age=pl_by_age$Age,PL=pl_by_age$MarketValue)),
                                     cbind(Type='Av. Number positions',Quantity='N. Psns',data.frame(Age=pl_by_age$Age,PL=(pl_by_age$Indicator/252))),
                                     cbind(Type='Av. Capital distribution',Quantity='$ at age',data.frame(Age=pl_by_age$Age,PL=pl_by_age$MarketValue*(pl_by_age$Indicator/252))))
            cum_tpl_smmry <- ggplot(plt_cum_pl_data,aes(x=as.numeric(Age),y=PL,group=Quantity,colour=Quantity)) + 
              geom_line(size=1) +
              ylab("") + 
              xlab("Position Age") + 
              labs(colour="") +
              ggtitle('Position PL and size by age') +
              theme(legend.position = "bottom") +
              theme(text = element_text(size=15)) +
              facet_grid(Type~.,scales="free_y")
            
            # set processed data as an output
           
            object <- .setOutputGGPlotData(object, plt_cum_pl_data)
            object <- .setOutputGGPlot(object, cum_tpl_smmry)
            
            
            return(object)
          }
)
