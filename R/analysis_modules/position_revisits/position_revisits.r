sourceTo("../analysis_modules/analysis_block/analysis_block.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/position_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/trade_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/analysis_block_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

################################################################################
#
# PositionRevisitsAnalysisBlock Class
# 
# Computation block class to pull data required for Computation of extended stock
# Pulls data required for computation and adds required columns.
###############################################################################


setClass(
  Class             = "PositionRevisitsAnalysisBlock",
  prototype         = list(
    key_cols        = c("TraderID", "start", "end"),
    key_values      = data.frame(TraderID = character(),
                              start    = as.Date(character()),
                              end    = as.Date(character())),
    column_name_map = hash(c("TraderID", "start", "end"), c("id", "start", "end"))
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualTradeDataHandler"
                        )
)

setMethod("setTradeDataObject",  
          signature(object = "PositionRevisitsAnalysisBlock", trade_data = "TradeData"),
          function(object, trade_data){
            .setTradeDataObject(object, trade_data)
          }
)

setMethod("dataRequest",
          signature(object = "PositionRevisitsAnalysisBlock", key_values = "data.frame"),
          function(object, key_values){
            
            object <- .setDataSourceQueryKeyValues(object,key_values)
            
            trader <- unique(key_values$TraderID)[1]
            start <- min(key_values$start)
            end <- max(key_values$end)
            
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
          signature(object = "PositionRevisitsAnalysisBlock"),
          function(object, key_values){
            
            trade_data <- getTradeDataObject(object)
            
            # retrieve needed ref_data
            trade_df <- getReferenceData(trade_data)
            
            trade_df_ht <- trade_df[!is.na(trade_df$TradeID),]
            trade_df_nt <- unique(trade_df[is.na(trade_df$TradeID),])
            
            history_data <- rbind(trade_df_ht, trade_df_nt)
            
            object <- setReferenceData(object, history_data)
            
            flat_data <- count_flats(history_data)
            revisit_data <- create_revisit_data(flat_data[[1]],history_data)
            revisit_plt_data  <- build_revisit_plot_data(revisit_data,flat_data[[2]],function(x)sum(x,na.rm=TRUE))
            #df <- cbind(IsTrade=0,TodayPL=0,Reduce(function(x,y)rbind(x,y),Map(function(x)data.frame(x,1:11),1:11)))
            #colnames(df) <- c('IsTrade','TodayPL','TotalVisits','Visit')
            #revisit_plt_data <- rbind(revisit_plt_data,df)
            revisit_plt_data <- aggregate(revisit_plt_data[c('IsTrade','TodayPL')],list(TotalVisits=revisit_plt_data$TotalVisits,Visit=revisit_plt_data$Visit),sum)
            revisit_plt_data <- with(revisit_plt_data,rbind(data.frame(Quantity='Total PL after visit',Value=TodayPL,TotalN=TotalVisits,VisitN=Visit),
                                                            data.frame(Quantity='Number Trades',Value=IsTrade,TotalN=TotalVisits,VisitN=Visit)))
            revisit_smmry <- ggplot(revisit_plt_data,aes(x=TotalN,group=VisitN,fill=VisitN)) +
              geom_bar(position="dodge",aes(weight=Value)) +
              scale_fill_distiller(palette = "Spectral") +
              ylab("") + 
              xlab("Total visits to stock") + 
              labs(fill="Visit number") +
              ggtitle('PL by number of visits to a stock') +
              # theme(text = element_text(size=15)) +
              facet_grid(Quantity~.,scales="free_y")
            
            
            # set processed data as an output
            
            object <- .setOutputGGPlotData(object, revisit_plt_data)
            object <- .setOutputGGPlot(object, revisit_smmry)
            
            
            return(object)
          }
)
