sourceTo("../analysis_modules/visualisation_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_module.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/ppmodel_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/trade_feature_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/module_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

# Assumes that module is being fed by a pre-processor module that produces the stats for a 3 month slice sampled monthly.
# The succesive months should be indexed by the ppmodel index with 3, refering to the most recent month.

psn_visualisations <- list()

for(ppmodel_subset in c(3,1)){

	# Number of hits and misses by:
	#1. Long/shrt 

	results_day_panel <- new('Panel')
	results_day_panel@parameters <- 'mfrow=c(2,1),mar=c(6,4,2.5,2.5)'
	results_day_panel@visualns <- list(new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","PsnLong","ppModelIndex"),subset_by=c('PsnLong',"ppModelIndex"),subset_with=list(TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Long',subset_fn=list(function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag",psn_level=TRUE),
									   new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","PsnLong","ppModelIndex"),subset_by=c('PsnLong',"ppModelIndex"),subset_with=list(FALSE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Short',subset_fn=list(function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag",psn_level=TRUE))
	psn_visualisations[[length(psn_visualisations)+1]] <- results_day_panel

	#2. and traded/not-traded

	results_day_panel <- new('Panel')
	results_day_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
	results_day_panel@visualns <- list(new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","PsnLong","PsnTraded","ppModelIndex"),subset_by=c('PsnLong',"PsnTraded","ppModelIndex"),subset_with=list(TRUE,TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Long/Traded',subset_fn=list(function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag",psn_level=TRUE),
									   new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","PsnLong","PsnTraded","ppModelIndex"),subset_by=c('PsnLong',"PsnTraded","ppModelIndex"),subset_with=list(FALSE,TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Short/Traded',subset_fn=list(function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag",psn_level=TRUE),
									   new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","PsnLong","PsnTraded","ppModelIndex"),subset_by=c('PsnLong',"PsnTraded","ppModelIndex"),subset_with=list(TRUE,FALSE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Long/Untraded',subset_fn=list(function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag",psn_level=TRUE),
									   new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","PsnLong","PsnTraded","ppModelIndex"),subset_by=c('PsnLong',"PsnTraded","ppModelIndex"),subset_with=list(FALSE,FALSE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Short/Untraded',subset_fn=list(function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag",psn_level=TRUE))
	psn_visualisations[[length(psn_visualisations)+1]] <- results_day_panel

	#3. Win loss ratios

	results_day_panel <- new('Panel')
	results_day_panel@parameters <- 'mfrow=c(2,3),mar=c(6,4,2.5,2.5)'
	results_day_panel@visualns <- list(new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("Tag","PsnLong","ppModelIndex"),subset_by=c("Tag","PsnLong","ppModelIndex"),subset_with=list(list('Hit',TRUE,ppmodel_subset),list('Miss',TRUE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Long",subset_fn=list(function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset,psn_level=TRUE),
								       new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("Tag","PsnLong","ppModelIndex"),subset_by=c("Tag","PsnLong","ppModelIndex"),subset_with=list(list('Hit',FALSE,ppmodel_subset),list('Miss',FALSE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Short",subset_fn=list(function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset,psn_level=TRUE),
								       new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("Tag","PsnLong","PsnTraded","ppModelIndex"),subset_by=c("Tag","PsnLong","PsnTraded","ppModelIndex"),subset_with=list(list('Hit',TRUE,TRUE,ppmodel_subset),list('Miss',TRUE,TRUE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Long Traded",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset,psn_level=TRUE),
								       new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("Tag","PsnLong","PsnTraded","ppModelIndex"),subset_by=c("Tag","PsnLong","PsnTraded","ppModelIndex"),subset_with=list(list('Hit',FALSE,TRUE,ppmodel_subset),list('Miss',FALSE,TRUE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Short Traded",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset,psn_level=TRUE),
								       new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("Tag","PsnLong","PsnTraded","ppModelIndex"),subset_by=c("Tag","PsnLong","PsnTraded","ppModelIndex"),subset_with=list(list('Hit',TRUE,FALSE,ppmodel_subset),list('Miss',TRUE,FALSE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Long Untraded",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset,psn_level=TRUE),
								       new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("Tag","PsnLong","PsnTraded","ppModelIndex"),subset_by=c("Tag","PsnLong","PsnTraded","ppModelIndex"),subset_with=list(list('Hit',FALSE,FALSE,ppmodel_subset),list('Miss',FALSE,FALSE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Short Untraded",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset,psn_level=TRUE))		
	psn_visualisations[[length(psn_visualisations)+1]] <- results_day_panel

}

#Tabulate data:

event_daypsn_panel_comp_name <- "HitRateByCategory"

event_daypsn_panel_comp <- function(module_data_list,name){
	output <- list()
	data_names <- c()
	#1. Hit rate for long/short position traded/untraded on day 0
	#	Uses panels 2 (increases) and 3 (decreases), ratios in element 5 (long) and 6 (short)
	nme <- "Position hit rate."
	column_names <- c('STRING|Side','STRING|Context','INT|Psns 1m','QUANTITY_1DP|Psns 3m','PERCENT_1DP|Hit rate 1m','PERCENT_1DP|Hit rate 3m','QUANTITY_1DP|Win loss 1m','QUANTITY_1DP|Win loss 3m')
	tryCatch({
			output[[length(output)+1]] <- data.frame(Side='Long',
				                                     Description='Overall',
				                                     Positions1M = element_picker(module_data_list,1,1,'Hit')+element_picker(module_data_list,1,1,'Miss'),
				                                     Positions3M = (element_picker(module_data_list,4,1,'Hit')+element_picker(module_data_list,4,1,'Miss'))/3,
				                                     Hits1M=(element_picker(module_data_list,1,1,'Hit')/(element_picker(module_data_list,1,1,'Hit')+element_picker(module_data_list,1,1,'Miss'))),
				                                     Hits3M=(element_picker(module_data_list,4,1,'Hit')/(element_picker(module_data_list,4,1,'Hit')+element_picker(module_data_list,4,1,'Miss'))),
									                 Winloss1m=(element_picker(module_data_list,3,1,1)),
									                 Winloss3m=(element_picker(module_data_list,6,1,1)))
			output[[length(output)]] <- rbind(output[[length(output)]], data.frame(Side='Long',
				                                     Description='Traded',
				                                     Positions1M = element_picker(module_data_list,2,1,'Hit')+element_picker(module_data_list,2,1,'Miss'),
				                                     Positions3M = (element_picker(module_data_list,5,1,'Hit')+element_picker(module_data_list,5,1,'Miss'))/3,
				                                     Hits1M=(element_picker(module_data_list,2,1,'Hit')/(element_picker(module_data_list,2,1,'Hit')+element_picker(module_data_list,2,1,'Miss'))),
				                                     Hits3M=(element_picker(module_data_list,5,1,'Hit')/(element_picker(module_data_list,5,1,'Hit')+element_picker(module_data_list,5,1,'Miss'))),
									                 Winloss1m=(element_picker(module_data_list,3,3,1)),
									                 Winloss3m=(element_picker(module_data_list,6,3,1))))
			output[[length(output)]] <- rbind(output[[length(output)]], data.frame(Side='Long',
				                                     Description='Untraded',
				                                     Positions1M = element_picker(module_data_list,2,3,'Hit')+element_picker(module_data_list,2,3,'Miss'),
				                                     Positions3M = (element_picker(module_data_list,5,3,'Hit')+element_picker(module_data_list,5,3,'Miss'))/3,
				                                     Hits1M=(element_picker(module_data_list,2,3,'Hit')/(element_picker(module_data_list,2,3,'Hit')+element_picker(module_data_list,2,3,'Miss'))),
				                                     Hits3M=(element_picker(module_data_list,5,3,'Hit')/(element_picker(module_data_list,5,3,'Hit')+element_picker(module_data_list,5,3,'Miss'))),
									                 Winloss1m=(element_picker(module_data_list,3,5,1)),
									                 Winloss3m=(element_picker(module_data_list,6,5,1))))
			output[[length(output)]] <- rbind(output[[length(output)]], data.frame(Side='Short',
				                                     Description='Overall',
				                                     Positions1M = element_picker(module_data_list,1,2,'Hit')+element_picker(module_data_list,1,2,'Miss'),
				                                     Positions3M = (element_picker(module_data_list,4,2,'Hit')+element_picker(module_data_list,4,2,'Miss'))/3,
				                                     Hits1M=(element_picker(module_data_list,1,2,'Hit')/(element_picker(module_data_list,1,2,'Hit')+element_picker(module_data_list,1,2,'Miss'))),
				                                     Hits3M=(element_picker(module_data_list,4,2,'Hit')/(element_picker(module_data_list,4,2,'Hit')+element_picker(module_data_list,4,2,'Miss'))),
									                 Winloss1m=(element_picker(module_data_list,3,2,1)),
									                 Winloss3m=(element_picker(module_data_list,6,2,1))))
			output[[length(output)]] <- rbind(output[[length(output)]], data.frame(Side='Short',
				                                     Description='Traded',
				                                     Positions1M = element_picker(module_data_list,2,2,'Hit')+element_picker(module_data_list,2,2,'Miss'),
				                                     Positions3M = (element_picker(module_data_list,5,2,'Hit')+element_picker(module_data_list,5,2,'Miss'))/3,
				                                     Hits1M=(element_picker(module_data_list,2,2,'Hit')/(element_picker(module_data_list,2,2,'Hit')+element_picker(module_data_list,2,2,'Miss'))),
				                                     Hits3M=(element_picker(module_data_list,5,2,'Hit')/(element_picker(module_data_list,5,2,'Hit')+element_picker(module_data_list,5,2,'Miss'))),
									                 Winloss1m=(element_picker(module_data_list,3,4,1)),
									                 Winloss3m=(element_picker(module_data_list,6,4,1))))
			output[[length(output)]] <- rbind(output[[length(output)]], data.frame(Side='Short',
				                                     Description='Untraded',
				                                     Positions1M = element_picker(module_data_list,2,4,'Hit')+element_picker(module_data_list,2,4,'Miss'),
				                                     Positions3M = (element_picker(module_data_list,5,4,'Hit')+element_picker(module_data_list,5,4,'Miss'))/3,
				                                     Hits1M=(element_picker(module_data_list,2,4,'Hit')/(element_picker(module_data_list,2,4,'Hit')+element_picker(module_data_list,2,4,'Miss'))),
				                                     Hits3M=(element_picker(module_data_list,5,4,'Hit')/(element_picker(module_data_list,5,4,'Hit')+element_picker(module_data_list,5,4,'Miss'))),
									                 Winloss1m=(element_picker(module_data_list,3,6,1)),
									                 Winloss3m=(element_picker(module_data_list,6,6,1))))
			data_names <- c(data_names,nme)
			}, error= function(cond){
				message(paste("Error applying panel computation",name,"in",nme,":",cond))
			})
	tryCatch({
			colnames(output[[length(output)]]) <- column_names
			},error = function(cond){
				message("Failed to set panel computation frame column schema.")
			})
	names(output) <- data_names
	return(output)
}

results_daypsn_analysis_module_builder <- new("AnalysisModuleFactory",name = "ResultsDayPsnModule",ppmdl_class = "ResultsDayPsnGatherer",visualisations = psn_visualisations,panel_computation=event_daypsn_panel_comp,computation_name=event_daypsn_panel_comp_name)
primary_placing_psn_analysis_module_builder <- new("AnalysisModuleFactory",name = "PrimaryPlacingPsnModule",ppmdl_class = "PrimaryPlacingPsnGatherer",visualisations = psn_visualisations,panel_computation=event_daypsn_panel_comp,computation_name=event_daypsn_panel_comp_name)
secondary_placing_psn_analysis_module_builder <- new("AnalysisModuleFactory",name = "SecondaryPlacingPsnModule",ppmdl_class = "SecondaryPlacingPsnGatherer",visualisations = psn_visualisations,panel_computation=event_daypsn_panel_comp,computation_name=event_daypsn_panel_comp_name)
