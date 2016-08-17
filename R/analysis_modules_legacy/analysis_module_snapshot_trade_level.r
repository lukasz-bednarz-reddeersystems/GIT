library(functional)
sourceTo("../analysis_modules_legacy/visualisation_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules_legacy/analysis_module.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/ppmodel_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/trade_feature_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../reporting/panel_computation_base_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

context_names     <- c('Buy High (long)','Buy Central (long)','Buy Low (long)','Sell High (long)','Sell Central (long)','Sell Low (long)','Buy High (short)','Buy Central (short)','Buy Low (short)','Sell High (short)','Sell Central (short)','Sell Low (short)')
computation_names <- c('Price snapshot')
parameter_names   <- c('aggregate_by','subset_by','subset_with','y_label','title','x_label')
contexts <- list()
contexts[[length(contexts)+1]] <- list(list(c('PsnLong','Long','EntryType'),c('PsnLong','Long','EntryType'),list(TRUE,TRUE,'High'),"Rel.Rtn",context_names[1],'Days'))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list(c('PsnLong','Long','EntryType'),c('PsnLong','Long','EntryType'),list(TRUE,TRUE,'Central'),"Rel.Rtn",context_names[2],'Days'))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list(c('PsnLong','Long','EntryType'),c('PsnLong','Long','EntryType'),list(TRUE,TRUE,'Low'),"Rel.Rtn",context_names[3],'Days'))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list(c('PsnLong','Long','EntryType'),c('PsnLong','Long','EntryType'),list(TRUE,FALSE,'High'),"Rel.Rtn",context_names[4],'Days'))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list(c('PsnLong','Long','EntryType'),c('PsnLong','Long','EntryType'),list(TRUE,FALSE,'Central'),"Rel.Rtn",context_names[5],'Days'))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list(c('PsnLong','Long','EntryType'),c('PsnLong','Long','EntryType'),list(TRUE,FALSE,'Low'),"Rel.Rtn",context_names[6],'Days'))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list(c('PsnLong','Long','EntryType'),c('PsnLong','Long','EntryType'),list(FALSE,TRUE,'High'),"Rel.Rtn",context_names[7],'Days'))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list(c('PsnLong','Long','EntryType'),c('PsnLong','Long','EntryType'),list(FALSE,TRUE,'Central'),"Rel.Rtn",context_names[8],'Days'))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list(c('PsnLong','Long','EntryType'),c('PsnLong','Long','EntryType'),list(FALSE,TRUE,'Low'),"Rel.Rtn",context_names[9],'Days'))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list(c('PsnLong','Long','EntryType'),c('PsnLong','Long','EntryType'),list(FALSE,FALSE,'High'),"Rel.Rtn",context_names[10],'Days'))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list(c('PsnLong','Long','EntryType'),c('PsnLong','Long','EntryType'),list(FALSE,FALSE,'Central'),"Rel.Rtn",context_names[11],'Days'))
names(contexts[[length(contexts)]]) <- computation_names
names(contexts[[length(contexts)]][[1]]) <- parameter_names
contexts[[length(contexts)+1]] <- list(list(c('PsnLong','Long','EntryType'),c('PsnLong','Long','EntryType'),list(FALSE,FALSE,'Low'),"Rel.Rtn",context_names[12],'Days'))
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

level_snapshot_panels 	  <- snapshot_panel_builder(contexts,parameters='mfrow=c(1,1),mar=c(6,4,2.5,2.5)',plot_path='../reporting/graphics/')
level_snapshot_analysis_module_builder <- new("AnalysisModuleFactory",name="TradeLevelSnapshotModule",ppmdl_class="TradeLevelSnapShot",visualisations=level_snapshot_panels)