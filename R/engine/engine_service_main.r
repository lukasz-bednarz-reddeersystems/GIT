#This file should be sourced by the managing process to activate the 
#trading enchancement engine.
working_path <- "C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/engine"
setwd(working_path)
sourceTo("engine.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
engine <- new("Engine")
startEngine(engine)
