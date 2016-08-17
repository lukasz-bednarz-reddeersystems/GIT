#This file should be sourced by the managing process to activate the 
#trading enchancement engine.
#define working path, sckt_port and host prior to running.
library(R.utils)
options(modifiedOnlySource=TRUE)
#working_path <- "C:/Development/TradingEnhancementEngine/R/engine"
#host <- 'localhost'
#sckt_port <- 130
setwd(working_path)
sourceTo("engine.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
engine <- new("Engine")
engine@socket@remote_host <- host
engine@socket@port <- sckt_port
startEngine(engine)
