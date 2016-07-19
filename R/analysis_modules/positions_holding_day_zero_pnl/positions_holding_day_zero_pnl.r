sourceTo("../analysis_modules/analysis_block/analysis_block.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/trade_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/offside_positions/offside_positions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)


################################################################################
#
# PositionsHoldingDayZeroPnLAnalysisBlock Class
# 
# Computation block class to pull data required for Computation of position holding period
# Pulls data required for computation and adds required columns.
###############################################################################


setClass(
  Class             = "PositionsHoldingDayZeroPnLAnalysisBlock",
  slots             = c(
    position_data   = "OffsidePositionData"
  ),
  prototype         = list(
    required_colnms = c('Date','InstrumentID','TodayPL','PassiveTodayPL','ActiveTodayPL',
                        'MarketRelPL','MinDate','MarketValue','Age','VolOutof'),
    position_data   = new("OffsidePositionData")
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualPositionDataHandler",
                        "VirtualTradeDataHandler"
                        )
)

setMethod("setPositionDataObject",  
          signature(object = "PositionsHoldingDayZeroPnLAnalysisBlock", position_data = "OffsidePositionData"),
          function(object, position_data){
            .setPositionDataObject(object, position_data)
          }
)

setMethod("setTradeDataObject",  
          signature(object = "PositionsHoldingDayZeroPnLAnalysisBlock", trade_data = "TradeData"),
          function(object, trade_data){
            .setTradeDataObject(object, trade_data)
          }
)

setMethod("dataRequest",
          signature(object = "PositionsHoldingDayZeroPnLAnalysisBlock", key_values = "data.frame"),
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
            
            
            # retrieve price reference data for query key_values
            trade_data <- getTradeDataObject(object)
            
            trade_data <- tryCatch({
              req_key_vals <- key_values
              colnames(req_key_vals) <- c("id", "start", "end")
              
              dataRequest(trade_data, req_key_vals)
              
            },error = function(cond){
              message(sprintf("Error when calling %s on %s class", "dataRequest()", class(trade_data)))
              message(sprintf("Querried for keys: id = %s, start = %s, end = %s", trader, start, end))
              stop(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(trade_data), cond))
            })
            
            object <- .setTradeDataObject(object, trade_data)
            
            return(object)
          }
)

setMethod("Process",  
          signature(object = "PositionsHoldingDayZeroPnLAnalysisBlock"),
          function(object, key_values){
            
            pos_data <- getPositionDataObject(object)
            trade_data <- getTradeDataObject(object)
            
            # retrieve needed ref_data
            history_data <- getReferenceData(pos_data)
            instruments <- unique(history_data$Instrument)
            
            trade_df <- getReferenceData(trade_data)
            
            trade_df_ht <- trade_df[!is.na(trade_df$TradeID),]
            trade_df_nt <- unique(trade_df[is.na(trade_df$TradeID),])
            
            trade_df <- rbind(trade_df_ht, trade_df_nt)
            
            merge_cols <- c("Date", "InstrumentID", "StrategyID")
            hist_cols <- setdiff(colnames(history_data), colnames(trade_df))
            hist_cols <- c(merge_cols, hist_cols)
            
            # trade_df <- trade_df[!is.na(trade_df$TradeID), trade_cols]
            
            # merge price data to position data
            history_data <- merge(trade_df,history_data[hist_cols], by = merge_cols)
            
            object <- setReferenceData(object, history_data)
            
            day_0_focus <- unique(history_data[c('Date','InstrumentID','TodayPL','PassiveTodayPL','ActiveTodayPL','MarketRelPL','MinDate','MarketValue','Age','VolOutof')])
            day_0_focus <- day_0_focus[day_0_focus$Age==0&!is.na(day_0_focus$Age),]
            day_0_focus$MarketValue <- abs(day_0_focus$MarketValue)
            day_0_focus$Swing <- (day_0_focus$VolOutof/10000)*day_0_focus$MarketValue
            day_0_focus$Date <- format(day_0_focus$Date,'%Y-%m') 
            day_0_focus <- aggregate(day_0_focus[c('TodayPL','MarketValue','Swing')],list(Date=day_0_focus$Date),function(x)mean(x,na.rm=TRUE))
            
            d0_plt <- ggplot(data=day_0_focus,aes(x=Date,y=TodayPL,size=MarketValue)) +
              geom_point(aes(colour=Swing)) +
              theme(text = element_text(size=15)) +
              ylab("Av. Day 0 PL") + 
              xlab("Month") + 
              theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
              scale_size_continuous(guide = FALSE) +
              labs(colour="Av. $ Swing") +
              scale_colour_distiller(palette="Spectral") +
              ggtitle('Day 0 sizing and PL') 
            
            # set processed data as an output
           
            object <- .setOutputGGPlotData(object, day_0_focus)
            object <- .setOutputGGPlot(object, d0_plt)
            
            
            return(object)
          }
)
