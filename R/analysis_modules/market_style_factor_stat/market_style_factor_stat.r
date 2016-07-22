sourceTo("../analysis_modules/analysis_block/analysis_block.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/portfolio_variance_decomposition/portfolio_variance_decomposition.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/risk_model/risk_model_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_block/market_style_data_handler.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

library(RColorBrewer)


################################################################################
#
# MarketStyleFactorStatisticAnalysisBlock Class
# 
# Computation block class to pull data required data for Market Style 
# Pulls data required for computation and adds required columns.
###############################################################################


setClass(
  Class             = "MarketStyleFactorStatisticAnalysisBlock",
  slots             = c(
    market_style       = "MarketStyleData"
  ),
  prototype         = list(
    key_cols        = c("start", "end"),
    key_values      = data.frame(start    = as.Date(character()),
                                 end    = as.Date(character())),
    column_name_map = hash(c("start", "end"), 
                           c("start", "end")),
    market_style       = new("MarketStyleData")
  ),
  contains          = c("VirtualAnalysisBlock",
                        "VirtualRiskModelHandler",
                        "VirtualMarketStyleDataHandler"
  )
)


setMethod("dataRequest",
          signature(object = "MarketStyleFactorStatisticAnalysisBlock", key_values = "data.frame"),
          function(object, key_values){
            
            object <- .setDataSourceQueryKeyValues(object,key_values)

            market_style <- getMarketStyleDataObject(object)
            
            # getting marketStyle data 

            query_keys <- data.frame(Date = seq(from = as.Date(min(key_values$start)),
                                                to   = as.Date(min(key_values$end)),
                                                by = 1))
            
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
          signature(object = "MarketStyleFactorStatisticAnalysisBlock"),
          function(object){
            
            # retrieve data
            market_style <- getMarketStyleDataObject(object)
            
            all_market_st <- getReferenceData(market_style)
            
            first <- TRUE
            
            # stack market data
            for(rm_date in sort(unique(all_market_st$Date))){
              
              rm_date <- as.Date(rm_date)
              
              if(wday(rm_date)!=7&wday(rm_date)!=1){
                
                market_st <- all_market_st[all_market_st$Date==rm_date,setdiff(colnames(all_market_st),'Date')]
                
                plot_data <- stack(market_st, select = c(portfolio_decomposition_all_factors))
                
                colnames(plot_data) <- c("Value", "RiskType")
                
                plot_data <- data.frame(Date = rm_date, plot_data)
                
                plot_data$RiskGroup <- plot_data$RiskType
                levels(plot_data$RiskGroup) <- portfolio_decomposition_factor_groups
                plot_data <- data.frame(Date = rm_date, plot_data)
                
                if(first){
                  mrkt_plot_data <- plot_data
                  first <- FALSE
                }
                else{
                  mrkt_plot_data <- rbind(mrkt_plot_data, plot_data)
                  
                } 
                
              }
            }

            #order of factors
            browser()
            
            ord_frm <- unique(mrkt_plot_data[c("RiskType", "RiskGroup")])
            ord_frm <- ord_frm[order(ord_frm$RiskGroup, ord_frm$RiskType, decreasing = TRUE),]
            
            mrkt_plot_data$RiskType <- factor(as.character(mrkt_plot_data$RiskType), 
                                                 levels = as.character(ord_frm$RiskType))
                   
            #Create plot
            plt_risk <- ggplot(data=mrkt_plot_data,aes(x=RiskType,y=Value,
                                                       color=RiskType
                                                       )
                               ) +
                        geom_boxplot() +
                        coord_flip() +
                        guides(color = FALSE)
                        

            object <- .setOutputGGPlotData(object, mrkt_plot_data)
            object <- .setOutputGGPlot(object, plt_risk)
            
            return(object)
          }
)
