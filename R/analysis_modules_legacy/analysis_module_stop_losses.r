library(functional)
sourceTo("../analysis_modules/visualisation_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_module.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/ppmodel_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/trade_feature_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../reporting/panel_computation_base_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE) 

context_names     <- c('Overall stop (long)','Overall no stop (long)','Stopped (long)','Not stopped (long)','Overall stop (short)','Overall no stop (short)','Stopped (short)','Not stopped (short)')
computation_names <- c('Counts','Ratios')
parameter_names   <- c('aggregate_what','aggregate_by','subset_by','subset_with','aggregate_fn','y_label','title','subset_fn','x_label_variable','psn_level','visuln_comp')
contexts <- list()
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","StopSet","PsnLong"),c("StopSet","PsnLong"),list(TRUE,TRUE),sum,"Count",context_names[1],list(identity,identity),"Tag",FALSE,data_aggregate_and_subset),
									   list("PnLInto",c("PtvePnLOutof","StopSet","PsnLong"),c("PtvePnLOutof","StopSet","PsnLong"),list(list(TRUE,TRUE,TRUE),list(FALSE,TRUE,TRUE)),function(x)mean(abs(x)),"Win/Loss",context_names[1],list(identity,identity,identity),"Tag",FALSE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","StopSet","PsnLong"),c("StopSet","PsnLong"),list(FALSE,TRUE),sum,"Count",context_names[2],list(identity,identity),"Tag",FALSE,data_aggregate_and_subset),
									   list("PnLInto",c("PtvePnLOutof","StopSet","PsnLong"),c("PtvePnLOutof","StopSet","PsnLong"),list(list(TRUE,FALSE,TRUE),list(FALSE,FALSE,TRUE)),function(x)mean(abs(x)),"Win/Loss",context_names[2],list(identity,identity,identity),"Tag",FALSE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","StoppedOut","PsnLong"),c("StoppedOut","PsnLong"),list(TRUE,TRUE),sum,"Count",context_names[3],list(identity,identity),"Tag",FALSE,data_aggregate_and_subset),
									   list("PnLInto",c("PtvePnLOutof","StoppedOut","PsnLong"),c("PtvePnLOutof","StoppedOut","PsnLong"),list(list(TRUE,TRUE,TRUE),list(FALSE,TRUE,TRUE)),function(x)mean(abs(x)),"Win/Loss",context_names[3],list(identity,identity,identity),"Tag",FALSE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","StoppedOut","PsnLong"),c("StoppedOut","PsnLong"),list(FALSE,TRUE),sum,"Count",context_names[4],list(identity,identity),"Tag",FALSE,data_aggregate_and_subset),
									   list("PnLInto",c("PtvePnLOutof","StoppedOut","PsnLong"),c("PtvePnLOutof","StoppedOut","PsnLong"),list(list(TRUE,FALSE,TRUE),list(FALSE,FALSE,TRUE)),function(x)mean(abs(x)),"Win/Loss",context_names[4],list(identity,identity,identity),"Tag",FALSE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","StopSet","PsnLong"),c("StopSet","PsnLong"),list(TRUE,FALSE),sum,"Count",context_names[5],list(identity,identity),"Tag",FALSE,data_aggregate_and_subset),
									   list("PnLInto",c("PtvePnLOutof","StopSet","PsnLong"),c("PtvePnLOutof","StopSet","PsnLong"),list(list(TRUE,TRUE,FALSE),list(FALSE,TRUE,FALSE)),function(x)mean(abs(x)),"Win/Loss",context_names[5],list(identity,identity,identity),"Tag",FALSE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","StopSet","PsnLong"),c("StopSet","PsnLong"),list(FALSE,FALSE),sum,"Count",context_names[6],list(identity,identity),"Tag",FALSE,data_aggregate_and_subset),
									   list("PnLInto",c("PtvePnLOutof","StopSet","PsnLong"),c("PtvePnLOutof","StopSet","PsnLong"),list(list(TRUE,FALSE,FALSE),list(FALSE,FALSE,FALSE)),function(x)mean(abs(x)),"Win/Loss",context_names[6],list(identity,identity,identity),"Tag",FALSE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","StoppedOut","PsnLong"),c("StoppedOut","PsnLong"),list(TRUE,FALSE),sum,"Count",context_names[7],list(identity,identity),"Tag",FALSE,data_aggregate_and_subset),
									   list("PnLInto",c("PtvePnLOutof","StoppedOut","PsnLong"),c("PtvePnLOutof","StoppedOut","PsnLong"),list(list(TRUE,TRUE,FALSE),list(FALSE,TRUE,FALSE)),function(x)mean(abs(x)),"Win/Loss",context_names[7],list(identity,identity,identity),"Tag",FALSE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","StoppedOut","PsnLong"),c("StoppedOut","PsnLong"),list(FALSE,FALSE),sum,"Count",context_names[8],list(identity,identity),"Tag",FALSE,data_aggregate_and_subset),
									   list("PnLInto",c("PtvePnLOutof","StoppedOut","PsnLong"),c("PtvePnLOutof","StoppedOut","PsnLong"),list(list(TRUE,FALSE,FALSE),list(FALSE,FALSE,FALSE)),function(x)mean(abs(x)),"Win/Loss",context_names[8],list(identity,identity,identity),"Tag",FALSE,data_aggregate_ratio_by_subset))
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

stop_loss_panels 	             <- frequencyplot_scorecard_panel_builder(contexts,ppmodel_subsets=ppmodel_subsets)
stop_loss_comp                   <- Curry(trade_level_scorecard,state_context_map=context_map)
stop_loss_analysis_module_builder<- new("AnalysisModuleFactory",name = "StopLossModule",ppmdl_class = "StopTargetsGatherer",visualisations = stop_loss_panels,panel_computation=stop_loss_comp)






