sourceTo("../analysis_modules/visualisation_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_module.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/ppmodel_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/trade_feature_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

#1. Number of hits and misses by long/shrt and traded/not-traded

results_day_panel <- new('Panel')
results_day_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
results_day_panel@visualns <- list(new("FrequencyPlot",aggregate_what="PsnHit",aggregate_by=c("Tag","PsnLong","PsnTraded"),subset_by=c('PsnLong',"PsnTraded"),subset_with=list(TRUE,TRUE),aggregate_fn=sum,y_label="Count",title='Long/Traded',x_label_variable="Tag"),
								   new("FrequencyPlot",aggregate_what="PsnHit",aggregate_by=c("Tag","PsnLong","PsnTraded"),subset_by=c('PsnLong',"PsnTraded"),subset_with=list(FALSE,TRUE),aggregate_fn=sum,y_label="Count",title='Short/Traded',x_label_variable="Tag"),
								   new("FrequencyPlot",aggregate_what="PsnHit",aggregate_by=c("Tag","PsnLong","PsnTraded"),subset_by=c('PsnLong',"PsnTraded"),subset_with=list(TRUE,FALSE),aggregate_fn=sum,y_label="Count",title='Long/Untraded',x_label_variable="Tag"),
								   new("FrequencyPlot",aggregate_what="PsnHit",aggregate_by=c("Tag","PsnLong","PsnTraded"),subset_by=c('PsnLong',"PsnTraded"),subset_with=list(FALSE,FALSE),aggregate_fn=sum,y_label="Count",title='Short/Untraded',x_label_variable="Tag"))
								   #new("FrequencyPlot",aggregate_what="PsnHit",aggregate_by=c("Tag","PsnLong","PsnTraded"),subset_by=c('PsnLong',"PsnTraded","Tag"),subset_with=list(list(TRUE,TRUE,'Hit'),list(TRUE,TRUE,'Miss')),aggregate_fn=sum,y_label="Hit/miss ratio",title='Long positions',x_label="Traded/Untraded",x_label_variable="PsnTraded",visuln_comp=data_aggregate_ratio_by_subset),
								   #new("FrequencyPlot",aggregate_what="PsnHit",aggregate_by=c("Tag","PsnLong","PsnTraded"),subset_by=c('PsnLong',"PsnTraded","Tag"),subset_with=list(list(FALSE,FALSE,'Hit'),list(FALSE,FALSE,'Miss')),aggregate_fn=sum,y_label="Hit/miss ratio",title='Short positions',x_label="Traded/Untraded",x_label_variable="PsnTraded",visuln_comp=data_aggregate_ratio_by_subset))		
psn_visualisations <- list(results_day_panel)

results_day_panel <- new('Panel')
results_day_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
results_day_panel@visualns <- list(new("FrequencyPlot",aggregate_what="PsnHit",aggregate_by=c("Tag","PsnLong","PsnTraded"),subset_by=c('PsnLong',"PsnTraded"),subset_with=list(TRUE,TRUE),aggregate_fn=sum,y_label="Count",title='Long/Traded',x_label_variable="Tag"),
								   new("FrequencyPlot",aggregate_what="PsnHit",aggregate_by=c("Tag","PsnLong","PsnTraded"),subset_by=c('PsnLong',"PsnTraded"),subset_with=list(FALSE,TRUE),aggregate_fn=sum,y_label="Count",title='Short/Traded',x_label_variable="Tag"),
								   new("FrequencyPlot",aggregate_what="PsnHit",aggregate_by=c("Tag","PsnLong","PsnTraded"),subset_by=c('PsnLong',"PsnTraded"),subset_with=list(TRUE,FALSE),aggregate_fn=sum,y_label="Count",title='Long/Untraded',x_label_variable="Tag"),
								   new("FrequencyPlot",aggregate_what="PsnHit",aggregate_by=c("Tag","PsnLong","PsnTraded"),subset_by=c('PsnLong',"PsnTraded"),subset_with=list(FALSE,FALSE),aggregate_fn=sum,y_label="Count",title='Short/Untraded',x_label_variable="Tag"))
								   #new("FrequencyPlot",aggregate_what="PsnHit",aggregate_by=c("Tag","PsnLong","PsnTraded"),subset_by=c('PsnLong',"PsnTraded","Tag"),subset_with=list(list(TRUE,TRUE,'Hit'),list(TRUE,TRUE,'Miss')),aggregate_fn=sum,y_label="Hit/miss ratio",title='Long positions',x_label="Traded/Untraded",x_label_variable="PsnTraded",visuln_comp=data_aggregate_ratio_by_subset),
								   #new("FrequencyPlot",aggregate_what="PsnHit",aggregate_by=c("Tag","PsnLong","PsnTraded"),subset_by=c('PsnLong',"PsnTraded","Tag"),subset_with=list(list(FALSE,FALSE,'Hit'),list(FALSE,FALSE,'Miss')),aggregate_fn=sum,y_label="Hit/miss ratio",title='Short positions',x_label="Traded/Untraded",x_label_variable="PsnTraded",visuln_comp=data_aggregate_ratio_by_subset))		
psn_visualisations[[2]] <- results_day_panel

resultsdaypsn_module_builder <- new("AnalysisModuleFactory",name = "ResultsDayPsnModule",ppmdl_class = "ResultsDayPsnGatherer",visualisations = psn_visualisations)
