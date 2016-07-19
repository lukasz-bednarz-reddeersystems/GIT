sourceTo("../analysis_modules/analysis_block/analysis_block.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/position_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/price_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/market_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/analysis_block_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

################################################################################
#
# OffsidePositionsAnalysisBlock Class
# 
# Computation block class to pull data required for Computation of extended stock
# Pulls data required for computation and adds required columns.
###############################################################################

setClass(
  Class             = "OffsidePositionData",
  prototype         = list(
    required_colnms = c("Date", "InstrumentID","Strategy","TraderID","StrategyID",
                        "Direction", "MarketValue", "TodayPL", "Quantity", "Age", "PsnReturn", "ClosePrice", "PreviousClosePrice",
                        "Volume", "OutstandingShares", "AvgVol30Day", "MinDate", "EarliestMarketValue", "EarliestPrice",
                        "EarliestIndexLevel", "EarliestHolding", "EarliestIndexHolding", "CurrentPassiveValue",  
                        "CurrentIndexValue", "CurrentPassivePL", "PassiveTodayPL", "ActiveTodayPL", "CumulativePL",         
                        "CumulativeMarketRelPL","MarketRelPL","PsnAge")
  ),
  contains          = c("VirtualPositionData")
)

setClass(
  Class             = "OffsidePositionsAnalysisBlock",
  slots             = c(
    market_data     = "MarketDataSX5E"
  ),
  prototype         = list(
    key_cols        = c("TraderID", "start", "end"),
    key_values      = data.frame(TraderID = character(),
                              start    = as.Date(character()),
                              end    = as.Date(character())),
    column_name_map = hash(c("TraderID", "start", "end"), c("id", "start", "end")),
    output          = new("OffsidePositionData")
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualPositionDataHandler",
                        "VirtualPriceDataHandler",
                        "VirtualMarketDataHandler"
                        )
)


setMethod("dataRequest",
          signature(object = "OffsidePositionsAnalysisBlock", key_values = "data.frame"),
          function(object, key_values){
            
            object <- .setDataSourceQueryKeyValues(object,key_values)
            
            trader <- unique(key_values$TraderID)[1]
            start <- min(key_values$start)
            end <- max(key_values$end)
            
            # retrieve position reference data for query key_values
            position_data <- getPositionDataObject(object)
            
            position_data <- tryCatch({
              pos_key_values <- key_values
              colnames(pos_key_values) <- c("id", "start", "end")
              dataRequest(position_data, pos_key_values)
              
            },error = function(cond){
              message(sprintf("Error when calling %s on %s class", "dataRequest()", class(position_data)))
              message(sprintf("Querried for keys: id = %s, start = %s, end = %s", trader, start, end))
              stop(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(position_data), cond))
            })
            
            object <- .setPositionDataObject(object, position_data)
            
            
            # retrieve price reference data for query key_values
            price_data <- getPriceDataObject(object)
            
            position_df <- getReferenceData(position_data)
            
            instruments <- unique(position_df$InstrumentID)
            dates <- unique(position_df$Date)
            
            price_key_vals <- expand.grid(lInstrumentID = instruments, dtDateTime = dates)
            
            price_data <- tryCatch({
              dataRequest(price_data, price_key_vals)
              
            },error = function(cond){
              message(sprintf("Error when calling %s on %s class", "dataRequest()", class(price_data)))
              message(sprintf("Querried for keys: id = %s, start = %s, end = %s", trader, start, end))
              stop(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(price_data), cond))
            })
            
            object <- .setPriceDataObject(object, price_data)
            
            # get market Data
            index_rd <- new("MarketDataSX5E", start, end)
            
            object <- .setMarketDataObject(object, index_rd)
            
            return(object)
          }
)



setMethod("Process",  
          signature(object = "OffsidePositionsAnalysisBlock"),
          function(object, key_values){
            
            pos_data <- getPositionDataObject(object)
            price_data <- getPriceDataObject(object)
            index_data <- getMarketDataObject(object)
            
            # retrieve needed ref_data
            history_data <- getReferenceData(pos_data)
            price_data <- getReferenceData(price_data)
            index <- getReferenceData(index_data)
            
            # merge price data to position data
            history_data <- merge(history_data, price_data, by = c("Date", "InstrumentID"), all.x = TRUE )
            
            # adding TradeDate column for compatibility with legacy functions.
            history_data <- market_rel_pl(history_data,trade_rel=FALSE, index)
            history_data <- market_day_age(history_data)
            
            #NB: Need to handle going flat (at the moment just look at earliest position date)
            rel_offside_data <- integrated_offside(history_data,type="CumulativeMarketRelPL")
            abs_offside_data <- integrated_offside(history_data,type="CumulativePL")
            abs_off <- new_psns(abs_offside_data[[1]])
            abs_off <- abs_off[abs_off$PsnAge>5,]
            rel_off <- new_psns(rel_offside_data[[1]])
            rel_off <- rel_off[rel_off$PsnAge>5,]
            n_psns <- aggregate(history_data['InstrumentID'],list(Date=history_data$Date),function(x)sum(!is.na(unique(x))))
            
            n_abs_offside <- aggregate(abs_off['CumulativePL'],list(Date=abs_off$Date),function(x)sum(unique(x)<0))
            n_rel_offside <- aggregate(rel_off['CumulativeMarketRelPL'],list(Date=rel_off$Date),function(x)sum(unique(x)<0))
            
            thisQ <- quarter(max(history_data$Date))
            
            n_psns$Q <- quarter(n_psns$Date)
            n_psns <- aggregate(n_psns['InstrumentID'],list(Quarter=(n_psns$Q==thisQ)),mean)
            n_abs_offside$Q <- quarter(n_abs_offside$Date)
            n_abs_offside <- aggregate(n_abs_offside['CumulativePL'],list(Quarter=(n_abs_offside$Q==thisQ)),mean)
            n_rel_offside$Q <- quarter(n_rel_offside$Date)
            n_rel_offside <- aggregate(n_rel_offside['CumulativeMarketRelPL'],list(Quarter=(n_rel_offside$Q==thisQ)),mean)
            all_n_psn_data <- merge(n_psns,n_abs_offside,by='Quarter')
            all_n_psn_data <- merge(all_n_psn_data,n_rel_offside,by='Quarter')
            
            colnames(all_n_psn_data) <- c('Quarter','Total','AbOff','RelOff')
            
            
            if(nrow(all_n_psn_data[all_n_psn_data$Quarter,]) == 0) {
              all_n_psn_data <- rbind(all_n_psn_data, data.frame(Quarter = TRUE, Total = 0, AbOff = 0 , RelOff = 0))
            }
            
            if(nrow(all_n_psn_data[!all_n_psn_data$Quarter,]) == 0) {
              all_n_psn_data <- rbind(all_n_psn_data, data.frame(Quarter = FALSE, Total = 0, AbOff = 0 , RelOff = 0))
              
            }
            
            off_chrt_data <- rbind(data.frame(Quantity='Number abs. offside',
                                              Value=all_n_psn_data$AbOff[all_n_psn_data$Quarter],
                                              Delta=all_n_psn_data$AbOff[all_n_psn_data$Quarter]-all_n_psn_data$AbOff[!all_n_psn_data$Quarter]),
                                   data.frame(Quantity='Number rel. offside',
                                              Value=all_n_psn_data$RelOff[all_n_psn_data$Quarter],
                                              Delta=all_n_psn_data$RelOff[all_n_psn_data$Quarter]-all_n_psn_data$RelOff[!all_n_psn_data$Quarter]),
                                   data.frame(Quantity='Total positions',
                                              Value=all_n_psn_data$Total[all_n_psn_data$Quarter],
                                              Delta=all_n_psn_data$Total[all_n_psn_data$Quarter]-all_n_psn_data$Total[!all_n_psn_data$Quarter]))
            
            off_smmry <- ggplot(data=off_chrt_data, aes(x=Quantity, fill=Quantity)) +
              geom_bar(aes(weight=Value)) +
              ylab("Number of Positions") + xlab("") + ggtitle('Total number of offside positions') +
              geom_text(aes(x= Quantity, y=Value, label = round(Delta)),size=4,fontface='bold') +  
              theme(legend.position = "none",axis.text.x = element_text(angle = 30, hjust = 1)) +
              # theme(text = element_text(size=15)) +
              scale_fill_brewer(palette="Set1")
            
            # set processed data as an output
            
            outp_object <- getOutputObject(object)
            outp_object <- setReferenceData(outp_object, history_data)
            object <- .setOutputObject(object, outp_object)
            
            
            object <- .setOutputGGPlotData(object, off_chrt_data)
            object <- .setOutputGGPlot(object, off_smmry)
            
            
            return(object)
          }
)
