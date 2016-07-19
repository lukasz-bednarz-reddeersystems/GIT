library(functional)
sourceTo("../analysis_modules/visualisation_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_module.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/ppmodel_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/trade_feature_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../reporting/panel_computation_base_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

context_names     <- c('Average down long','Average down short')
computation_names <- c('Counts','Ratios')
parameter_names   <- c('aggregate_what','aggregate_by','subset_by','subset_with','aggregate_fn','y_label','title','subset_fn','x_label_variable','psn_level','visuln_comp')
contexts <- list()
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","AverageDown","PsnLong"),c("AverageDown","PsnLong"),list(TRUE,TRUE),sum,"Count",context_names[1],list(identity,identity),"Tag",TRUE,data_aggregate_and_subset),
									   list("Total.PL",c("PsnUp","AverageDown","PsnLong"),c("PsnUp","AverageDown","PsnLong"),list(list(TRUE,TRUE,TRUE),list(FALSE,TRUE,TRUE)),function(x)mean(abs(x)),"Win/Loss",context_names[1],list(identity,identity,identity),"Tag",TRUE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","AverageDown","PsnLong"),c("AverageDown","PsnLong"),list(TRUE,FALSE),sum,"Count",context_names[1],list(identity,identity),"Tag",TRUE,data_aggregate_and_subset),
									   list("Total.PL",c("PsnUp","AverageDown","PsnLong"),c("PsnUp","AverageDown","PsnLong"),list(list(TRUE,TRUE,FALSE),list(FALSE,TRUE,FALSE)),function(x)mean(abs(x)),"Win/Loss",context_names[2],list(identity,identity,identity),"Tag",TRUE,data_aggregate_ratio_by_subset))
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
	names(context_map[[length(context_map)]]) <- c("Positions1M","Positions3M","Winloss1M","Winloss3M")
	cntxt <- cntxt + 1
}
names(context_map) <- context_names

average_down_panels 	             <- frequencyplot_scorecard_panel_builder(contexts,ppmodel_subsets=ppmodel_subsets)
average_down_panel_comp              <- Curry(position_level_scorecard,state_context_map=context_map)
average_down_analysis_module_builder <- new("AnalysisModuleFactory",name = "AverageDownModule",ppmdl_class = "AverageDownPsnGatherer",visualisations = average_down_panels,panel_computation=average_down_panel_comp)






