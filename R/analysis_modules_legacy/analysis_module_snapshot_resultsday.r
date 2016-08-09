library(functional)
sourceTo("../analysis_modules_legacy/visualisation_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules_legacy/analysis_module.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/ppmodel_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/trade_feature_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../reporting/panel_computation_base_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

context_names     <- c('Psn Increase (long)','Psn Increase (new long)','Psn Increase (short)','Psn Increase (new short)','Psn Decrease (long)','Psn Close (long)','Psn Decrease (short)','Psn Close (short)')
rday_panels <- function(context_names,cname="Rel.Rtn"){
	computation_names <- c('Price snapshot')
	parameter_names   <- c('aggregate_by','subset_by','subset_with','y_label','title','x_label','options')
	contexts <- list()
	contexts[[length(contexts)+1]] <- list(list(c('PsnLong','Long','DaysSinceLastResults'),c('PsnLong','Long','DaysSinceLastResults'),list(TRUE,TRUE,0),cname,context_names[1],'Days','cex=2.5'))
	names(contexts[[length(contexts)]]) <- computation_names
	names(contexts[[length(contexts)]][[1]]) <- parameter_names
	contexts[[length(contexts)+1]] <- list(list(c('PsnLong','Long','DaysSinceLastResults','NewPosition'),c('PsnLong','Long','DaysSinceLastResults','NewPosition'),list(TRUE,TRUE,0,TRUE),cname,context_names[2],'Days','cex=2.5'))
	names(contexts[[length(contexts)]]) <- computation_names
	names(contexts[[length(contexts)]][[1]]) <- parameter_names
	contexts[[length(contexts)+1]] <- list(list(c('PsnLong','Long','DaysSinceLastResults'),c('PsnLong','Long','DaysSinceLastResults'),list(FALSE,FALSE,0),cname,context_names[3],'Days','cex=2.5'))
	names(contexts[[length(contexts)]]) <- computation_names
	names(contexts[[length(contexts)]][[1]]) <- parameter_names
	contexts[[length(contexts)+1]] <- list(list(c('PsnLong','Long','DaysSinceLastResults','NewPosition'),c('PsnLong','Long','DaysSinceLastResults','NewPosition'),list(FALSE,FALSE,0,TRUE),cname,context_names[4],'Days','cex=2.5'))
	names(contexts[[length(contexts)]]) <- computation_names
	names(contexts[[length(contexts)]][[1]]) <- parameter_names
	contexts[[length(contexts)+1]] <- list(list(c('PsnLong','Long','DaysSinceLastResults'),c('PsnLong','Long','DaysSinceLastResults'),list(TRUE,FALSE,0),cname,context_names[5],'Days','cex=2.5'))
	names(contexts[[length(contexts)]]) <- computation_names
	names(contexts[[length(contexts)]][[1]]) <- parameter_names
	contexts[[length(contexts)+1]] <- list(list(c('PsnLong','Long','DaysSinceLastResults','ClosePosition'),c('PsnLong','Long','DaysSinceLastResults','ClosePosition'),list(TRUE,FALSE,0,TRUE),cname,context_names[6],'Days','cex=2.5'))
	names(contexts[[length(contexts)]]) <- computation_names
	names(contexts[[length(contexts)]][[1]]) <- parameter_names
	contexts[[length(contexts)+1]] <- list(list(c('PsnLong','Long','DaysSinceLastResults'),c('PsnLong','Long','DaysSinceLastResults'),list(FALSE,TRUE,0),cname,context_names[7],'Days','cex=2.5'))
	names(contexts[[length(contexts)]]) <- computation_names
	names(contexts[[length(contexts)]][[1]]) <- parameter_names
	contexts[[length(contexts)+1]] <- list(list(c('PsnLong','Long','DaysSinceLastResults','ClosePosition'),c('PsnLong','Long','DaysSinceLastResults','ClosePosition'),list(FALSE,TRUE,0,TRUE),cname,context_names[8],'Days','cex=2.5'))
	names(contexts[[length(contexts)]]) <- computation_names
	names(contexts[[length(contexts)]][[1]]) <- parameter_names
	names(contexts) <- context_names	
	return(contexts)
}

contexts <- rday_panels(context_names)
context_map <- list()
cntxt <- 1
n_cntxts <- length(contexts)
for(context in contexts){
	context_map[[length(context_map)+1]] <- list(list(cntxt,1))	
	names(context_map[[length(context_map)]]) <- c("TradeSnapshot1M")
	cntxt <- cntxt + 1
}
names(context_map) <- context_names

resultsday_snapshot_panels 	  <- snapshot_panel_builder(contexts,parameters='mfrow=c(1,1),mar=c(6,4,2.5,2.5)')
resultsday_exposure_snapshot_panels 	  <- snapshot_panel_builder(rday_panels(context_names,cname="Rel. Exp"),parameters='mfrow=c(1,1),mar=c(6,4,2.5,2.5)')

resultsday_snapshot_analysis_module_builder <- new("AnalysisModuleFactory",name="ResultsDaySnapShotModule",ppmdl_class="ResultsDaySnapShotBatchGatherer",visualisations=resultsday_snapshot_panels)
resultsday_exposure_snapshot_analysis_module_builder <- new("AnalysisModuleFactory",name="ResultsDayExposureSnapShotModule",ppmdl_class="ResultsDayExposureSnapShotBatchGatherer",visualisations=resultsday_exposure_snapshot_panels)