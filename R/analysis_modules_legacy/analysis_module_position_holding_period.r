library(functional)
sourceTo("../analysis_modules_legacy/visualisation_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules_legacy/analysis_module.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/ppmodel_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/trade_feature_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../reporting/panel_computation_base_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

context_names     <- c('New positions long','Old positions long','Nominal positions long','New positions short','Old positions short','Nominal positions short')
computation_names <- c('Counts','Ratios')
parameter_names   <- c('aggregate_what','aggregate_by','subset_by','subset_with','aggregate_fn','y_label','title','subset_fn','x_label_variable','psn_level','visuln_comp')
contexts <- list()
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","AgeCategory","PsnLong"),c("AgeCategory","PsnLong"),list('New',TRUE),sum,"Count",context_names[1],list(identity,identity),"Tag",TRUE,data_aggregate_and_subset),
									   list("Total.PL",c("PsnUp","AgeCategory","PsnLong"),c("PsnUp","AgeCategory","PsnLong"),list(list(TRUE,'New',TRUE),list(FALSE,'New',TRUE)),function(x)mean(abs(x)),"Win/Loss",context_names[1],list(identity,identity,identity),"Tag",TRUE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","AgeCategory","PsnLong"),c("AgeCategory","PsnLong"),list('Old',TRUE),sum,"Count",context_names[2],list(identity,identity),"Tag",TRUE,data_aggregate_and_subset),
									   list("Total.PL",c("PsnUp","AgeCategory","PsnLong"),c("PsnUp","AgeCategory","PsnLong"),list(list(TRUE,'Old',TRUE),list(FALSE,'Old',TRUE)),function(x)mean(abs(x)),"Win/Loss",context_names[2],list(identity,identity,identity),"Tag",TRUE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","AgeCategory","PsnLong"),c("AgeCategory","PsnLong"),list('Remainder',TRUE),sum,"Count",context_names[3],list(identity,identity),"Tag",TRUE,data_aggregate_and_subset),
									   list("Total.PL",c("PsnUp","AgeCategory","PsnLong"),c("PsnUp","AgeCategory","PsnLong"),list(list(TRUE,'Remainder',TRUE),list(FALSE,'Remainder',TRUE)),function(x)mean(abs(x)),"Win/Loss",context_names[3],list(identity,identity,identity),"Tag",TRUE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","AgeCategory","PsnLong"),c("AgeCategory","PsnLong"),list('New',FALSE),sum,"Count",context_names[4],list(identity,identity),"Tag",TRUE,data_aggregate_and_subset),
									   list("Total.PL",c("PsnUp","AgeCategory","PsnLong"),c("PsnUp","AgeCategory","PsnLong"),list(list(TRUE,'New',FALSE),list(FALSE,'New',FALSE)),function(x)mean(abs(x)),"Win/Loss",context_names[4],list(identity,identity,identity),"Tag",TRUE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","AgeCategory","PsnLong"),c("AgeCategory","PsnLong"),list('Old',FALSE),sum,"Count",context_names[5],list(identity,identity),"Tag",TRUE,data_aggregate_and_subset),
									   list("Total.PL",c("PsnUp","AgeCategory","PsnLong"),c("PsnUp","AgeCategory","PsnLong"),list(list(TRUE,'Old',FALSE),list(FALSE,'Old',FALSE)),function(x)mean(abs(x)),"Win/Loss",context_names[5],list(identity,identity,identity),"Tag",TRUE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","AgeCategory","PsnLong"),c("AgeCategory","PsnLong"),list('Remainder',FALSE),sum,"Count",context_names[6],list(identity,identity),"Tag",TRUE,data_aggregate_and_subset),
									   list("Total.PL",c("PsnUp","AgeCategory","PsnLong"),c("PsnUp","AgeCategory","PsnLong"),list(list(TRUE,'Remainder',FALSE),list(FALSE,'Remainder',FALSE)),function(x)mean(abs(x)),"Win/Loss",context_names[6],list(identity,identity,identity),"Tag",TRUE,data_aggregate_ratio_by_subset))
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

holding_period_panels 	  <- frequencyplot_scorecard_panel_builder(contexts,ppmodel_subsets=ppmodel_subsets)
holding_period_panel_comp <- Curry(trade_level_scorecard,state_context_map=context_map)
holding_period_analysis_module_builder <- new("AnalysisModuleFactory",name = "HoldingPeriodModule",ppmdl_class = "PsnAgeGatherer",visualisations = holding_period_panels,panel_computation=holding_period_panel_comp)
history_analysis_module_builder <- new("AnalysisModuleFactory",name = "TradeHistoryModule",ppmdl_class = "TradeHistory",visualisations = holding_period_panels,panel_computation=holding_period_panel_comp)





