#This file should be sourced by the managing process to activate the 
#trading enchancement engine.
#define working path, sckt_port and host prior to running.
library(R.utils)
library(TE.FrontendEngine)

# test <- FALSE

working_path <- "C:/Development/GIT/R/engine"
host <- 'localhost'
sckt_port <- 130
message(paste("ENGINE IS IN TEST MODE"))

# setwd(working_path)
engine <- new("Engine")
engine@socket@remote_host <- host
engine@socket@port <- sckt_port
startEngine(engine)
