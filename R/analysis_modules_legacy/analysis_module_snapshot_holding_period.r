library(functional)
sourceTo("../analysis_modules_legacy/visualisation_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules_legacy/analysis_module.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/ppmodel_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/trade_feature_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../reporting/panel_computation_base_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

context_names     <- c('New positions long','Old positions long','Nominal positions long','New positions short','Old positions short','Nominal positions short')
computation_names <- c('Price snapshot')
parameter_names   <- c('aggregate_by','subset_by','subset_with','y_label','title','x_label')
contexts <- list()
contexts[[length(contexts)+1]] <- list(list(c('Long','AgeCategory'),c('Long','AgeCategory'),list(TRUE,'New'),"Rel.Rtn",context_names[1],'Days'))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list(c('Long','AgeCategory'),c('Long','AgeCategory'),list(TRUE,'Old'),"Rel.Rtn",context_names[2],'Days'))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list(c('Long','AgeCategory'),c('Long','AgeCategory'),list(TRUE,'Nominal'),"Rel.Rtn",context_names[3],'Days'))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list(c('Long','AgeCategory'),c('Long','AgeCategory'),list(FALSE,'New'),"Rel.Rtn",context_names[4],'Days'))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list(c('Long','AgeCategory'),c('Long','AgeCategory'),list(FALSE,'Old'),"Rel.Rtn",context_names[5],'Days'))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list(c('Long','AgeCategory'),c('Long','AgeCategory'),list(FALSE,'Nominal'),"Rel.Rtn",context_names[6],'Days'))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
names(contexts) <- context_names

context_map <- list()
cntxt <- 1
n_cntxts <- length(contexts)
for(context in contexts){
	context_map[[length(context_map)+1]] <- list(list(cntxt,1))	
	names(context_map[[length(context_map)]]) <- c("TradeSnapshot1M")
	cntxt <- cntxt + 1
}
names(context_map) <- context_names

price_snapshot_panels 	  <- snapshot_panel_builder(contexts,parameters='mfrow=c(2,3),mar=c(6,4,2.5,2.5)',plot_path='../reporting/graphics')
price_snapshot_analysis_module_builder <- new("AnalysisModuleFactory",name="HoldingPeriodSnapshotModule",ppmdl_class="PriceBySnapshotGatherer",visualisations=price_snapshot_panels)






