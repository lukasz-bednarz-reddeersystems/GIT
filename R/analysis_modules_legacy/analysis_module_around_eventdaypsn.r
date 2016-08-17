sourceTo("../analysis_modules_legacy/visualisation_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules_legacy/analysis_module.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/ppmodel_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/trade_feature_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules_legacy/module_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

# Assumes that module is being fed by a pre-processor module that produces the stats for a 3 month slice sampled monthly.
# The succesive months should be indexed by the ppmodel index with 3, refering to the most recent month.

evnt_rtn_visualistaions <- list()

for(ppmodel_subset in c(3,1)){

	# Number of up/down over the month:
	#1. Long/shrt 

	results_day_panel <- new('Panel')
	results_day_panel@parameters <- 'mfrow=c(2,1),mar=c(6,4,2.5,2.5)'
	results_day_panel@visualns <- list(new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","PsnLong","ppModelIndex"),subset_by=c('PsnLong',"ppModelIndex"),subset_with=list(TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Long',subset_fn=list(function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag"),
									   new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","PsnLong","ppModelIndex"),subset_by=c('PsnLong',"ppModelIndex"),subset_with=list(FALSE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Short',subset_fn=list(function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag"))
	evnt_rtn_visualistaions[[length(evnt_rtn_visualistaions)+1]] <- results_day_panel

	#2. and traded/not-traded around event

	results_day_panel <- new('Panel')
	results_day_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
	results_day_panel@visualns <- list(new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","PsnLong","PsnTraded","ppModelIndex"),subset_by=c('PsnLong',"PsnTraded","ppModelIndex"),subset_with=list(TRUE,TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Long/Traded around event',subset_fn=list(function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag"),
									   new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","PsnLong","PsnTraded","ppModelIndex"),subset_by=c('PsnLong',"PsnTraded","ppModelIndex"),subset_with=list(FALSE,TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Short/Traded around event',subset_fn=list(function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag"),
									   new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","PsnLong","PsnTraded","ppModelIndex"),subset_by=c('PsnLong',"PsnTraded","ppModelIndex"),subset_with=list(TRUE,FALSE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Long/Untraded around event',subset_fn=list(function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag"),
									   new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","PsnLong","PsnTraded","ppModelIndex"),subset_by=c('PsnLong',"PsnTraded","ppModelIndex"),subset_with=list(FALSE,FALSE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Short/Untraded around event',subset_fn=list(function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag"))
	evnt_rtn_visualistaions[[length(evnt_rtn_visualistaions)+1]] <- results_day_panel

	#3. Win loss ratios around event

	results_day_panel <- new('Panel')
	results_day_panel@parameters <- 'mfrow=c(2,3),mar=c(6,4,2.5,2.5)'
	results_day_panel@visualns <- list(new("FrequencyPlot",aggregate_what="Total.PL",aggregate_by=c("Tag","PsnLong","ppModelIndex"),subset_by=c("Tag","PsnLong","ppModelIndex"),subset_with=list(list('Up',TRUE,ppmodel_subset),list('Down',TRUE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Long (around event)",subset_fn=list(function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset),
								       new("FrequencyPlot",aggregate_what="Total.PL",aggregate_by=c("Tag","PsnLong","ppModelIndex"),subset_by=c("Tag","PsnLong","ppModelIndex"),subset_with=list(list('Up',FALSE,ppmodel_subset),list('Down',FALSE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Short (around event)",subset_fn=list(function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset),
								       new("FrequencyPlot",aggregate_what="Total.PL",aggregate_by=c("Tag","PsnLong","PsnTraded","ppModelIndex"),subset_by=c("Tag","PsnLong","PsnTraded","ppModelIndex"),subset_with=list(list('Up',TRUE,TRUE,ppmodel_subset),list('Down',TRUE,TRUE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Long Traded (around event)",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset),
								       new("FrequencyPlot",aggregate_what="Total.PL",aggregate_by=c("Tag","PsnLong","PsnTraded","ppModelIndex"),subset_by=c("Tag","PsnLong","PsnTraded","ppModelIndex"),subset_with=list(list('Up',FALSE,TRUE,ppmodel_subset),list('Down',FALSE,TRUE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Short Traded (around event)",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset),
								       new("FrequencyPlot",aggregate_what="Total.PL",aggregate_by=c("Tag","PsnLong","PsnTraded","ppModelIndex"),subset_by=c("Tag","PsnLong","PsnTraded","ppModelIndex"),subset_with=list(list('Up',TRUE,FALSE,ppmodel_subset),list('Down',TRUE,FALSE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Long Untraded (around event)",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset),
								       new("FrequencyPlot",aggregate_what="Total.PL",aggregate_by=c("Tag","PsnLong","PsnTraded","ppModelIndex"),subset_by=c("Tag","PsnLong","PsnTraded","ppModelIndex"),subset_with=list(list('Up',FALSE,FALSE,ppmodel_subset),list('Down',FALSE,FALSE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Short Untraded (around event)",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset))		
	evnt_rtn_visualistaions[[length(evnt_rtn_visualistaions)+1]] <- results_day_panel

}

#Tabulate data:

around_event_panel_comp_name <- "HitRateByCategory"

around_event_panel_comp <- function(module_data_list,name){
	output <- list()
	data_names <- c()
	#1. Up rate for long/short position traded/untraded on day 0
	#	Uses panels 2 (increases) and 3 (decreases), ratios in element 5 (long) and 6 (short)
	nme <- "Position Up rate."
	column_names <- c('STRING|Side','STRING|Context','INT|Psns 1m','QUANTITY_1DP|Psns 3m','PERCENT_1DP|Psns. up 1m','PERCENT_1DP|Psns. up 3m','QUANTITY_1DP|Win loss 1m','QUANTITY_1DP|Win loss 3m')
	tryCatch({
			output[[length(output)+1]] <- data.frame(Side='Long',
				                                     Description='Overall',
				                                     Positions1M = element_picker(module_data_list,1,1,'Up')+element_picker(module_data_list,1,1,'Down'),
				                                     Positions3M = (element_picker(module_data_list,4,1,'Up')+element_picker(module_data_list,4,1,'Down'))/3,
				                                     Hits1M=(element_picker(module_data_list,1,1,'Up')/(element_picker(module_data_list,1,1,'Up')+element_picker(module_data_list,1,1,'Down'))),
				                                     Hits3M=(element_picker(module_data_list,4,1,'Up')/(element_picker(module_data_list,4,1,'Up')+element_picker(module_data_list,4,1,'Down'))),
									                 Winloss1m=(element_picker(module_data_list,3,1,1)),
									                 Winloss3m=(element_picker(module_data_list,6,1,1)))
			output[[length(output)]] <- rbind(output[[length(output)]], data.frame(Side='Long',
				                                     Description='Traded',
				                                     Positions1M = element_picker(module_data_list,2,1,'Up')+element_picker(module_data_list,2,1,'Down'),
				                                     Positions3M = (element_picker(module_data_list,5,1,'Up')+element_picker(module_data_list,5,1,'Down'))/3,
				                                     Hits1M=(element_picker(module_data_list,2,1,'Up')/(element_picker(module_data_list,2,1,'Up')+element_picker(module_data_list,2,1,'Down'))),
				                                     Hits3M=(element_picker(module_data_list,5,1,'Up')/(element_picker(module_data_list,5,1,'Up')+element_picker(module_data_list,5,1,'Down'))),
									                 Winloss1m=(element_picker(module_data_list,3,3,1)),
									                 Winloss3m=(element_picker(module_data_list,6,3,1))))
			output[[length(output)]] <- rbind(output[[length(output)]], data.frame(Side='Long',
				                                     Description='Untraded',
				                                     Positions1M = element_picker(module_data_list,2,3,'Up')+element_picker(module_data_list,2,3,'Down'),
				                                     Positions3M = (element_picker(module_data_list,5,3,'Up')+element_picker(module_data_list,5,3,'Down'))/3,
				                                     Hits1M=(element_picker(module_data_list,2,3,'Up')/(element_picker(module_data_list,2,3,'Up')+element_picker(module_data_list,2,3,'Down'))),
				                                     Hits3M=(element_picker(module_data_list,5,3,'Up')/(element_picker(module_data_list,5,3,'Up')+element_picker(module_data_list,5,3,'Down'))),
									                 Winloss1m=(element_picker(module_data_list,3,5,1)),
									                 Winloss3m=(element_picker(module_data_list,6,5,1))))
			output[[length(output)]] <- rbind(output[[length(output)]], data.frame(Side='Short',
				                                     Description='Overall',
				                                     Positions1M = element_picker(module_data_list,1,2,'Up')+element_picker(module_data_list,1,2,'Down'),
				                                     Positions3M = (element_picker(module_data_list,4,2,'Up')+element_picker(module_data_list,4,2,'Down'))/3,
				                                     Hits1M=(element_picker(module_data_list,1,2,'Up')/(element_picker(module_data_list,1,2,'Up')+element_picker(module_data_list,1,2,'Down'))),
				                                     Hits3M=(element_picker(module_data_list,4,2,'Up')/(element_picker(module_data_list,4,2,'Up')+element_picker(module_data_list,4,2,'Down'))),
									                 Winloss1m=(element_picker(module_data_list,3,2,1)),
									                 Winloss3m=(element_picker(module_data_list,6,2,1))))
			output[[length(output)]] <- rbind(output[[length(output)]], data.frame(Side='Short',
				                                     Description='Traded',
				                                     Positions1M = element_picker(module_data_list,2,2,'Up')+element_picker(module_data_list,2,2,'Down'),
				                                     Positions3M = (element_picker(module_data_list,5,2,'Up')+element_picker(module_data_list,5,2,'Down'))/3,
				                                     Hits1M=(element_picker(module_data_list,2,2,'Up')/(element_picker(module_data_list,2,2,'Up')+element_picker(module_data_list,2,2,'Down'))),
				                                     Hits3M=(element_picker(module_data_list,5,2,'Up')/(element_picker(module_data_list,5,2,'Up')+element_picker(module_data_list,5,2,'Down'))),
									                 Winloss1m=(element_picker(module_data_list,3,4,1)),
									                 Winloss3m=(element_picker(module_data_list,6,4,1))))
			output[[length(output)]] <- rbind(output[[length(output)]], data.frame(Side='Short',
				                                     Description='Untraded',
				                                     Positions1M = element_picker(module_data_list,2,4,'Up')+element_picker(module_data_list,2,4,'Down'),
				                                     Positions3M = (element_picker(module_data_list,5,4,'Up')+element_picker(module_data_list,5,4,'Down'))/3,
				                                     Hits1M=(element_picker(module_data_list,2,4,'Up')/(element_picker(module_data_list,2,4,'Up')+element_picker(module_data_list,2,4,'Down'))),
				                                     Hits3M=(element_picker(module_data_list,5,4,'Up')/(element_picker(module_data_list,5,4,'Up')+element_picker(module_data_list,5,4,'Down'))),
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

rtn_around_prmryplacing_analysis_module_builder <- new("AnalysisModuleFactory",name = "PrimaryPlacingPsnModule",ppmdl_class = "PrimaryPlacingPsnGatherer",visualisations = evnt_rtn_visualistaions,panel_computation=around_event_panel_comp,computation_name=around_event_panel_comp_name)
rtn_around_sndryplacing_analysis_module_builder <- new("AnalysisModuleFactory",name = "SecondaryPlacingPsnModule",ppmdl_class = "SecondaryPlacingPsnGatherer",visualisations = evnt_rtn_visualistaions,panel_computation=around_event_panel_comp,computation_name=around_event_panel_comp_name)
