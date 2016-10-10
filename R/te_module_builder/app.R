library(TE.AnalysisClasses)
library(TE.DataAccess)
library(TE.RefClasses)
library(TE.RiskModel)
library(TE.FrontendEngine)
library(shiny)

module_name <- "MarketStyleFactorStatisticAnalysisBlock"
lookback <- dated_twelve_monthly_lookback
trader <- 11
date <- '2016-09-01'

#get test data
block_client   <- tryCatch({new(paste(module_name,"Client",sep=""))},error=function(cond)stop(paste("Failed to set module name:",module_name,cond)))
key_function   <- tryCatch({lookback},error=function(cond)stop(paste("Failed to set lookback, exiting:",cond)))
key_values     <- tryCatch({key_function(trader, date)},error=function(cond)stop(paste("Failed to set key values on date",date,"for trader",trader,":",cond)))
block_client   <- tryCatch({dataRequest(block_client, key_values)},error=function(cond)stop(paste("Analysis data request failed:",cond)))
block          <- tryCatch({getAnalysisBlock(block_client)},error=function(cond)stop(paste("Failed to set analysis block:",cond)))
analysis_ggplot<- getOutputGGPlot(block)
analysis_data  <- getOutputGGPlotData(block)
ui_options <- getOutputFrontendData(block)

factory <- new("ShinyFactory")
factory <- tryCatch({shinyUIFactory(factory,analysis_ggplot,analysis_data,ui_options=ui_options)},error=function(cond)stop(paste("UI factory failed:",cond)))
factory <- tryCatch({shinyServerFactory(factory,analysis_ggplot,analysis_data,ui_options=ui_options)},error=function(cond)stop(paste("Server factory failed:",cond)))
shinyApp(ui = getUI(factory), server = getServer(factory))
