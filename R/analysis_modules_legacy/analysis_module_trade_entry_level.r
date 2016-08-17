library(functional)
sourceTo("../analysis_modules_legacy/visualisation_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules_legacy/analysis_module.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/ppmodel_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/trade_feature_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../reporting/panel_computation_base_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

context_names     <- c('Buy High (long)','Buy Central (long)','Buy Low (long)','Sell High (long)','Sell Central (long)','Sell Low (long)','Buy High (short)','Buy Central (short)','Buy Low (short)','Sell High (short)','Sell Central (short)','Sell Low (short)')
computation_names <- c('Counts','Ratios')
parameter_names   <- c('aggregate_what','aggregate_by','subset_by','subset_with','aggregate_fn','y_label','title','subset_fn','x_label_variable','psn_level','visuln_comp')
contexts <- list()
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","EntryType","PsnLong","Long"),c("EntryType","PsnLong","Long"),list('High',TRUE,TRUE),sum,"Count",context_names[1],list(identity,identity,identity),"Tag",FALSE,data_aggregate_and_subset),
									   list("TodayPL",c("Hit1D","EntryType","PsnLong","Long"),c("Hit1D","EntryType","PsnLong","Long"),list(list(TRUE,'High',TRUE,TRUE),list(FALSE,'High',TRUE,TRUE)),function(x)mean(abs(x)),"Win/Loss",context_names[1],list(identity,identity,identity,identity),"Tag",FALSE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","EntryType","PsnLong","Long"),c("EntryType","PsnLong","Long"),list('Central',TRUE,TRUE),sum,"Count",context_names[2],list(identity,identity,identity),"Tag",FALSE,data_aggregate_and_subset),
									   list("TodayPL",c("Hit1D","EntryType","PsnLong","Long"),c("Hit1D","EntryType","PsnLong","Long"),list(list(TRUE,'Central',TRUE,TRUE),list(FALSE,'Central',TRUE,TRUE)),function(x)mean(abs(x)),"Win/Loss",context_names[2],list(identity,identity,identity,identity),"Tag",FALSE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","EntryType","PsnLong","Long"),c("EntryType","PsnLong","Long"),list('Low',TRUE,TRUE),sum,"Count",context_names[3],list(identity,identity,identity),"Tag",FALSE,data_aggregate_and_subset),
									   list("TodayPL",c("Hit1D","EntryType","PsnLong","Long"),c("Hit1D","EntryType","PsnLong","Long"),list(list(TRUE,'Low',TRUE,TRUE),list(FALSE,'Low',TRUE,TRUE)),function(x)mean(abs(x)),"Win/Loss",context_names[3],list(identity,identity,identity,identity),"Tag",FALSE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","EntryType","PsnLong","Long"),c("EntryType","PsnLong","Long"),list('High',TRUE,FALSE),sum,"Count",context_names[4],list(identity,identity,identity),"Tag",FALSE,data_aggregate_and_subset),
									   list("TodayPL",c("Hit1D","EntryType","PsnLong","Long"),c("Hit1D","EntryType","PsnLong","Long"),list(list(TRUE,'High',TRUE,FALSE),list(FALSE,'High',TRUE,FALSE)),function(x)mean(abs(x)),"Win/Loss",context_names[4],list(identity,identity,identity,identity),"Tag",FALSE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","EntryType","PsnLong","Long"),c("EntryType","PsnLong","Long"),list('Central',TRUE,FALSE),sum,"Count",context_names[5],list(identity,identity,identity),"Tag",FALSE,data_aggregate_and_subset),
									   list("TodayPL",c("Hit1D","EntryType","PsnLong","Long"),c("Hit1D","EntryType","PsnLong","Long"),list(list(TRUE,'Central',TRUE,FALSE),list(FALSE,'Central',TRUE,FALSE)),function(x)mean(abs(x)),"Win/Loss",context_names[5],list(identity,identity,identity,identity),"Tag",FALSE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","EntryType","PsnLong","Long"),c("EntryType","PsnLong","Long"),list('Low',TRUE,FALSE),sum,"Count",context_names[6],list(identity,identity,identity),"Tag",FALSE,data_aggregate_and_subset),
									   list("TodayPL",c("Hit1D","EntryType","PsnLong","Long"),c("Hit1D","EntryType","PsnLong","Long"),list(list(TRUE,'Low',TRUE,FALSE),list(FALSE,'Low',TRUE,FALSE)),function(x)mean(abs(x)),"Win/Loss",context_names[6],list(identity,identity,identity,identity),"Tag",FALSE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","EntryType","PsnLong","Long"),c("EntryType","PsnLong","Long"),list('High',FALSE,TRUE),sum,"Count",context_names[7],list(identity,identity,identity),"Tag",FALSE,data_aggregate_and_subset),
									   list("TodayPL",c("Hit1D","EntryType","PsnLong","Long"),c("Hit1D","EntryType","PsnLong","Long"),list(list(TRUE,'High',FALSE,TRUE),list(FALSE,'High',FALSE,TRUE)),function(x)mean(abs(x)),"Win/Loss",context_names[7],list(identity,identity,identity,identity),"Tag",FALSE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","EntryType","PsnLong","Long"),c("EntryType","PsnLong","Long"),list('Central',FALSE,TRUE),sum,"Count",context_names[8],list(identity,identity,identity),"Tag",FALSE,data_aggregate_and_subset),
									   list("TodayPL",c("Hit1D","EntryType","PsnLong","Long"),c("Hit1D","EntryType","PsnLong","Long"),list(list(TRUE,'Central',FALSE,TRUE),list(FALSE,'Central',FALSE,TRUE)),function(x)mean(abs(x)),"Win/Loss",context_names[8],list(identity,identity,identity,identity),"Tag",FALSE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","EntryType","PsnLong","Long"),c("EntryType","PsnLong","Long"),list('Low',FALSE,TRUE),sum,"Count",context_names[9],list(identity,identity,identity),"Tag",FALSE,data_aggregate_and_subset),
									   list("TodayPL",c("Hit1D","EntryType","PsnLong","Long"),c("Hit1D","EntryType","PsnLong","Long"),list(list(TRUE,'Low',FALSE,TRUE),list(FALSE,'Low',FALSE,TRUE)),function(x)mean(abs(x)),"Win/Loss",context_names[9],list(identity,identity,identity,identity),"Tag",FALSE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","EntryType","PsnLong","Long"),c("EntryType","PsnLong","Long"),list('High',FALSE,FALSE),sum,"Count",context_names[10],list(identity,identity,identity),"Tag",FALSE,data_aggregate_and_subset),
									   list("TodayPL",c("Hit1D","EntryType","PsnLong","Long"),c("Hit1D","EntryType","PsnLong","Long"),list(list(TRUE,'High',FALSE,FALSE),list(FALSE,'High',FALSE,FALSE)),function(x)mean(abs(x)),"Win/Loss",context_names[10],list(identity,identity,identity,identity),"Tag",FALSE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","EntryType","PsnLong","Long"),c("EntryType","PsnLong","Long"),list('Central',FALSE,FALSE),sum,"Count",context_names[11],list(identity,identity,identity),"Tag",FALSE,data_aggregate_and_subset),
									   list("TodayPL",c("Hit1D","EntryType","PsnLong","Long"),c("Hit1D","EntryType","PsnLong","Long"),list(list(TRUE,'Central',FALSE,FALSE),list(FALSE,'Central',FALSE,FALSE)),function(x)mean(abs(x)),"Win/Loss",context_names[11],list(identity,identity,identity,identity),"Tag",FALSE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","EntryType","PsnLong","Long"),c("EntryType","PsnLong","Long"),list('Low',FALSE,FALSE),sum,"Count",context_names[12],list(identity,identity,identity),"Tag",FALSE,data_aggregate_and_subset),
									   list("TodayPL",c("Hit1D","EntryType","PsnLong","Long"),c("Hit1D","EntryType","PsnLong","Long"),list(list(TRUE,'Low',FALSE,FALSE),list(FALSE,'Low',FALSE,FALSE)),function(x)mean(abs(x)),"Win/Loss",context_names[12],list(identity,identity,identity,identity),"Tag",FALSE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
names(contexts) <- context_names

ppmodel_subsets <- c(3,1)
context_map <- list()
cntxt <- 1
n_cntxts <- length(contexts)
for(context in contexts){
	context_map[[length(context_map)+1]] <- list(list(cntxt,1),list(cntxt,2),list(cntxt,3),list(cntxt,4))	
	names(context_map[[length(context_map)]]) <- c("Trades1M","Trades3M","Winloss1M","Winloss3M")
	cntxt <- cntxt + 1
}
names(context_map) <- context_names

entry_level_panels 	             <- frequencyplot_scorecard_panel_builder(contexts,ppmodel_subsets=ppmodel_subsets)
entry_level_comp                 <- Curry(trade_level_scorecard,state_context_map=context_map)
entry_level_analysis_module_builder <- new("AnalysisModuleFactory",name = "TradeEntryLevelModule",ppmdl_class = "TradeLevelGatherer",visualisations = entry_level_panels,panel_computation=entry_level_comp)






