library(functional)
sourceTo("../analysis_modules_legacy/visualisation_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules_legacy/analysis_module.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/ppmodel_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/trade_feature_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../reporting/panel_computation_base_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

context_names     <- c('Pet long onside','Pet short onside','Pet long offside','Pet short offside','Notpet long onside','Notpet short onside','Notpet long offside','Notpet short offside')
computation_names <- c('Counts','Ratios')
parameter_names   <- c('aggregate_what','aggregate_by','subset_by','subset_with','aggregate_fn','y_label','title','subset_fn','x_label_variable','psn_level','visuln_comp')
contexts <- list()
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","PetName","PsnLong","PsnOffside"),c("PetName","PsnLong","PsnOffside"),list(TRUE,TRUE,FALSE),sum,"Count","Pet long onside",list(identity,identity,identity),"Tag",FALSE,data_aggregate_and_subset),
									   list("PnLOutof",c("PtvePnLOutof","PetName","PsnLong","PsnOffside"),c("PtvePnLOutof","PetName","PsnLong","PsnOffside"),list(list(TRUE,TRUE,TRUE,FALSE),list(FALSE,TRUE,TRUE,FALSE)),function(x)mean(abs(x)),"Win/Loss","Pet long onside",list(identity,identity,identity,identity),"Tag",FALSE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","PetName","PsnLong","PsnOffside"),c("PetName","PsnLong","PsnOffside"),list(TRUE,FALSE,FALSE),sum,"Count","Pet short onside",list(identity,identity,identity),"Tag",FALSE,data_aggregate_and_subset),
									   list("PnLOutof",c("PtvePnLOutof","PetName","PsnLong","PsnOffside"),c("PtvePnLOutof","PetName","PsnLong","PsnOffside"),list(list(TRUE,TRUE,FALSE,FALSE),list(FALSE,TRUE,FALSE,FALSE)),function(x)mean(abs(x)),"Win/Loss","Pet short onside",list(identity,identity,identity,identity),"Tag",FALSE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","PetName","PsnLong","PsnOffside"),c("PetName","PsnLong","PsnOffside"),list(TRUE,TRUE,TRUE),sum,"Count","Pet long offside",list(identity,identity,identity),"Tag",FALSE,data_aggregate_and_subset),
									   list("PnLOutof",c("PtvePnLOutof","PetName","PsnLong","PsnOffside"),c("PtvePnLOutof","PetName","PsnLong","PsnOffside"),list(list(TRUE,TRUE,TRUE,TRUE),list(FALSE,TRUE,TRUE,TRUE)),function(x)mean(abs(x)),"Win/Loss","Pet long offside",list(identity,identity,identity,identity),"Tag",FALSE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","PetName","PsnLong","PsnOffside"),c("PetName","PsnLong","PsnOffside"),list(TRUE,FALSE,TRUE),sum,"Count","Pet short offside",list(identity,identity,identity),"Tag",FALSE,data_aggregate_and_subset),
									   list("PnLOutof",c("PtvePnLOutof","PetName","PsnLong","PsnOffside"),c("PtvePnLOutof","PetName","PsnLong","PsnOffside"),list(list(TRUE,TRUE,FALSE,TRUE),list(FALSE,TRUE,FALSE,TRUE)),function(x)mean(abs(x)),"Win/Loss","Pet short offside",list(identity,identity,identity,identity),"Tag",FALSE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","PetName","PsnLong","PsnOffside"),c("PetName","PsnLong","PsnOffside"),list(FALSE,TRUE,FALSE),sum,"Count","Notpet long onside",list(identity,identity,identity),"Tag",FALSE,data_aggregate_and_subset),
									   list("PnLOutof",c("PtvePnLOutof","PetName","PsnLong","PsnOffside"),c("PtvePnLOutof","PetName","PsnLong","PsnOffside"),list(list(TRUE,FALSE,TRUE,FALSE),list(FALSE,FALSE,TRUE,FALSE)),function(x)mean(abs(x)),"Win/Loss","Notpet long onside",list(identity,identity,identity,identity),"Tag",FALSE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","PetName","PsnLong","PsnOffside"),c("PetName","PsnLong","PsnOffside"),list(FALSE,FALSE,FALSE),sum,"Count","Notpet short onside",list(identity,identity,identity),"Tag",FALSE,data_aggregate_and_subset),
									   list("PnLOutof",c("PtvePnLOutof","PetName","PsnLong","PsnOffside"),c("PtvePnLOutof","PetName","PsnLong","PsnOffside"),list(list(TRUE,FALSE,FALSE,FALSE),list(FALSE,FALSE,FALSE,FALSE)),function(x)mean(abs(x)),"Win/Loss","Notpet short onside",list(identity,identity,identity,identity),"Tag",FALSE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","PetName","PsnLong","PsnOffside"),c("PetName","PsnLong","PsnOffside"),list(FALSE,TRUE,TRUE),sum,"Count","Notpet long offside",list(identity,identity,identity),"Tag",FALSE,data_aggregate_and_subset),
									   list("PnLOutof",c("PtvePnLOutof","PetName","PsnLong","PsnOffside"),c("PtvePnLOutof","PetName","PsnLong","PsnOffside"),list(list(TRUE,FALSE,TRUE,TRUE),list(FALSE,FALSE,TRUE,TRUE)),function(x)mean(abs(x)),"Win/Loss","Notpet long offside",list(identity,identity,identity,identity),"Tag",FALSE,data_aggregate_ratio_by_subset))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts[[length(contexts)]][[2]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list("Indicator",c("Tag","PetName","PsnLong","PsnOffside"),c("PetName","PsnLong","PsnOffside"),list(FALSE,FALSE,TRUE),sum,"Count","Notpet short offside",list(identity,identity,identity),"Tag",FALSE,data_aggregate_and_subset),
									   list("PnLOutof",c("PtvePnLOutof","PetName","PsnLong","PsnOffside"),c("PtvePnLOutof","PetName","PsnLong","PsnOffside"),list(list(TRUE,FALSE,FALSE,TRUE),list(FALSE,FALSE,FALSE,TRUE)),function(x)mean(abs(x)),"Win/Loss","Notpet short offside",list(identity,identity,identity,identity),"Tag",FALSE,data_aggregate_ratio_by_subset))
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

petnames_traded_panels 	   <- frequencyplot_scorecard_panel_builder(contexts,ppmodel_subsets=ppmodel_subsets)
petnames_traded_panel_comp <- Curry(trade_level_scorecard,state_context_map=context_map)
pet_names_traded_analysis_module_builder <- new("AnalysisModuleFactory",name = "PetNamesTradedModule",ppmdl_class = "PetNamesPsnGatherer",visualisations = petnames_traded_panels,panel_computation=petnames_traded_panel_comp)






