sourceTo("../analysis_modules/analysis_block/analysis_block.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/portfolio_variance_decomposition/portfolio_variance_decomposition.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/risk_model/risk_model_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/market_style_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

library(RColorBrewer)


################################################################################
#
# MarketStyleAnalysisBlock Class
# 
# Computation block class to pull data required data for Market Style 
# Pulls data required for computation and adds required columns.
###############################################################################


setClass(
  Class             = "MarketStyleAnalysisBlock",
  slots             = c(
    market_style       = "MarketStyleData"
  ),
  prototype         = list(
    key_cols        = c("TraderID", "start", "end"),
    key_values      = data.frame(TraderID = character(),
                                 start    = as.Date(character()),
                                 end    = as.Date(character())),
    column_name_map = hash(c("TraderID", "start", "end"), 
                           c("id", "start", "end")),
    market_style       = new("MarketStyleData")
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualRiskModelHandler",
                        "VirtualMarketStyleDataHandler"
  )
)


setMethod("dataRequest",
          signature(object = "MarketStyleAnalysisBlock", key_values = "data.frame"),
          function(object, key_values){
            
            object <- .setDataSourceQueryKeyValues(object,key_values)

            market_style <- getMarketStyleDataObject(object)
            
            # getting marketStyle data 

            query_keys <- unique(query_keys["Date"])
            
            market_style <- tryCatch({
              dataRequest(market_style, query_keys)
              
            },error = function(cond){
              message(sprintf("Error when calling %s on %s class", "dataRequest()", class(market_style)))
              message(sprintf("Querried for keys: id = %s, start = %s, end = %s", trader, start, end))
              end(sprintf("Error when calling %s on %s class : \n %s", "dataRequest()", class(market_style), cond))
            })
            
            object <- .setMarketStyleDataObject(object, market_style)

            return(object)
          }
)



setMethod("Process",  
          signature(object = "MarketStyleAnalysisBlock"),
          function(object){
            
            # retrieve data
            market_style <- getMarketStyleDataObject(object)
            
            all_market_st <- getReferenceData(market_style)
            
            # stack market data
            for(rm_date in sort(unique(all_market_st$Date))){
              
              rm_date <- as.Date(rm_date)
              
              if(wday(rm_date)!=7&wday(rm_date)!=1){
                
                plot_data <- stack(rd.tot, select = c(portfolio_decomposition_all_factors))
                
                colnames(plot_data) <- c("Value", "Factor")
                
                if(first){
                  mrkt_plot_data <- plot_data
                  first <- FALSE
                }
                else{
                  mrkt_plot_data <- rbind(mrkt_plot_data, plot_data)
                  
                } 
                
              }
            }

            #Create plot
            
            plt_risk <- ggplot(data=mrkt_plot_data,aes(x=Date,y=Value,color=Factor)) +
                        geom_boxplot()

            object <- .setOutputGGPlotData(object, mrkt_plot_data)
            object <- .setOutputGGPlot(object, plt_risk)
            
            return(object)
          }
)
