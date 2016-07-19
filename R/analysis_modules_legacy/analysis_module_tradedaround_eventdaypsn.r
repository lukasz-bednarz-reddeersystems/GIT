sourceTo("../analysis_modules/visualisation_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_module.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/ppmodel_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/trade_feature_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/module_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

# Assumes that module is being fed by a pre-processor module that produces the stats for a 3 month slice sampled monthly.
# The succesive months should be indexed by the ppmodel index with 3, refering to the most recent month.

evnt_trded_rtn_visualistaions <- list()

for(ppmodel_subset in c(3,1)){

	#1. long/short, before/after, on/offside

	results_day_panel <- new('Panel')
	results_day_panel@parameters <- 'mfrow=c(3,3),mar=c(6,4,2.5,2.5)'
	results_day_panel@visualns <- list(new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_by=c("BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_with=list(TRUE,TRUE,TRUE,TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Long offside (before event)',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag"),
									   new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_by=c("BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_with=list(TRUE,FALSE,TRUE,TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Short offside (before event)',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag"),
									   new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_by=c("BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_with=list(TRUE,TRUE,FALSE,TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Long onside (before event)',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag"),
									   new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_by=c("BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_with=list(TRUE,FALSE,FALSE,TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Long onside (before event)',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag"),
									   new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_by=c("BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_with=list(FALSE,TRUE,TRUE,TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Long offside (after event)',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag"),
									   new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_by=c("BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_with=list(FALSE,FALSE,TRUE,TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Short offside (after event)',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag"),
									   new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_by=c("BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_with=list(FALSE,TRUE,FALSE,TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Long onside (after event)',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag"),
									   new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_by=c("BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_with=list(FALSE,FALSE,FALSE,TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Long onside (after event)',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag"))
	evnt_trded_rtn_visualistaions[[length(evnt_trded_rtn_visualistaions)+1]] <- results_day_panel

	#2. Short term PL hit after event

	results_day_panel <- new('Panel')
	results_day_panel@parameters <- 'mfrow=c(3,3),mar=c(6,4,2.5,2.5)'
	results_day_panel@visualns <- list(new("FrequencyPlot",aggregate_what="PtvePnLOutof",aggregate_by=c("BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_by=c("BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_with=list(TRUE,TRUE,TRUE,TRUE,ppmodel_subset),aggregate_fn=mean,y_label="Hit rate",title='Long offside (before event)',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn)),
									   new("FrequencyPlot",aggregate_what="PtvePnLOutof",aggregate_by=c("BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_by=c("BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_with=list(TRUE,FALSE,TRUE,TRUE,ppmodel_subset),aggregate_fn=mean,y_label="Hit rate",title='Short offside (before event)',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn)),
									   new("FrequencyPlot",aggregate_what="PtvePnLOutof",aggregate_by=c("BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_by=c("BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_with=list(TRUE,TRUE,FALSE,TRUE,ppmodel_subset),aggregate_fn=mean,y_label="Hit rate",title='Long onside (before event)',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn)),
									   new("FrequencyPlot",aggregate_what="PtvePnLOutof",aggregate_by=c("BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_by=c("BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_with=list(TRUE,FALSE,FALSE,TRUE,ppmodel_subset),aggregate_fn=mean,y_label="Hit rate",title='Long onside (before event)',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn)),
									   new("FrequencyPlot",aggregate_what="PtvePnLOutof",aggregate_by=c("BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_by=c("BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_with=list(FALSE,TRUE,TRUE,TRUE,ppmodel_subset),aggregate_fn=mean,y_label="Hit rate",title='Long offside (after event)',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn)),
									   new("FrequencyPlot",aggregate_what="PtvePnLOutof",aggregate_by=c("BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_by=c("BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_with=list(FALSE,FALSE,TRUE,TRUE,ppmodel_subset),aggregate_fn=mean,y_label="Hit rate",title='Short offside (after event)',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn)),
									   new("FrequencyPlot",aggregate_what="PtvePnLOutof",aggregate_by=c("BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_by=c("BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_with=list(FALSE,TRUE,FALSE,TRUE,ppmodel_subset),aggregate_fn=mean,y_label="Hit rate",title='Long onside (after event)',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn)),
									   new("FrequencyPlot",aggregate_what="PtvePnLOutof",aggregate_by=c("BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_by=c("BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_with=list(FALSE,FALSE,FALSE,TRUE,ppmodel_subset),aggregate_fn=mean,y_label="Hit rate",title='Long onside (after event)',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn)))
	evnt_trded_rtn_visualistaions[[length(evnt_trded_rtn_visualistaions)+1]] <- results_day_panel


	#3. Win loss ratios around event

	results_day_panel <- new('Panel')
	results_day_panel@parameters <- 'mfrow=c(3,3),mar=c(6,4,2.5,2.5)'
	results_day_panel@visualns <- list(new("FrequencyPlot",aggregate_what="PnLOutof",aggregate_by=c("PtvePnLOutof","BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_by=c("PtvePnLOutof","BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_with=list(list(TRUE,TRUE,TRUE,TRUE,TRUE,ppmodel_subset),list(FALSE,TRUE,TRUE,TRUE,TRUE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Long offside (before event)",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset),
									   new("FrequencyPlot",aggregate_what="PnLOutof",aggregate_by=c("PtvePnLOutof","BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_by=c("PtvePnLOutof","BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_with=list(list(TRUE,TRUE,FALSE,TRUE,TRUE,ppmodel_subset),list(FALSE,TRUE,FALSE,TRUE,TRUE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Short offside (before event)",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset),
									   new("FrequencyPlot",aggregate_what="PnLOutof",aggregate_by=c("PtvePnLOutof","BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_by=c("PtvePnLOutof","BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_with=list(list(TRUE,TRUE,TRUE,FALSE,TRUE,ppmodel_subset),list(FALSE,TRUE,TRUE,FALSE,TRUE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Long onside (before event)",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset),
									   new("FrequencyPlot",aggregate_what="PnLOutof",aggregate_by=c("PtvePnLOutof","BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_by=c("PtvePnLOutof","BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_with=list(list(TRUE,TRUE,FALSE,FALSE,TRUE,ppmodel_subset),list(FALSE,TRUE,FALSE,FALSE,TRUE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Short onside (before event)",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset),
									   new("FrequencyPlot",aggregate_what="PnLOutof",aggregate_by=c("PtvePnLOutof","BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_by=c("PtvePnLOutof","BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_with=list(list(TRUE,FALSE,TRUE,TRUE,TRUE,ppmodel_subset),list(FALSE,FALSE,TRUE,TRUE,TRUE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Long offside (after event)",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset),
									   new("FrequencyPlot",aggregate_what="PnLOutof",aggregate_by=c("PtvePnLOutof","BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_by=c("PtvePnLOutof","BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_with=list(list(TRUE,FALSE,FALSE,TRUE,TRUE,ppmodel_subset),list(FALSE,FALSE,FALSE,TRUE,TRUE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Short offside (after event)",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset),
									   new("FrequencyPlot",aggregate_what="PnLOutof",aggregate_by=c("PtvePnLOutof","BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_by=c("PtvePnLOutof","BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_with=list(list(TRUE,FALSE,TRUE,FALSE,TRUE,ppmodel_subset),list(FALSE,FALSE,TRUE,FALSE,TRUE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Long onside (after event)",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset),
									   new("FrequencyPlot",aggregate_what="PnLOutof",aggregate_by=c("PtvePnLOutof","BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_by=c("PtvePnLOutof","BeforeEvent","PsnLong","PsnOffside","PsnTraded","ppModelIndex"),subset_with=list(list(TRUE,FALSE,FALSE,FALSE,TRUE,ppmodel_subset),list(FALSE,FALSE,FALSE,FALSE,TRUE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Short onside (after event)",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset))		
	evnt_trded_rtn_visualistaions[[length(evnt_trded_rtn_visualistaions)+1]] <- results_day_panel

}

#Tabulate data:

trd_around_event_panel_comp_name <- "HitRateByCategory"

trd_around_event_panel_comp <- function(module_data_list,name){
	output <- list()
	data_names <- c()
	#1. Hit rate for long/short position traded/untraded on day 0
	#	Uses panels 2 (increases) and 3 (decreases), ratios in element 5 (long) and 6 (short)
	nme <- "Position Hit rate."
	column_names <- c('STRING|Side','STRING|Context','INT|Trades 1m','QUANTITY_1DP|Trades 3m','PERCENT_1DP|+ve 5dPL 1m','PERCENT_1DP|+ve 5dPL 3m','QUANTITY_1DP|+/-ve PL 1m','QUANTITY_1DP|+/-ve PL 3m')
	tryCatch({
			panel <- 1
			part  <- 1
			output[[length(output)+1]] <- data.frame(Side='Buy',
				                                     Description='Bef. Offs.',
				                                     Positions1M = element_picker(module_data_list,panel,part,'Hit')+element_picker(module_data_list,panel,part,'Miss'),
				                                     Positions3M = (element_picker(module_data_list,panel+3,part,'Hit')+element_picker(module_data_list,panel+3,part,'Miss'))/3,
				                                     Hits1M=element_picker(module_data_list,panel+1,part,NULL),
				                                     Hits3M=element_picker(module_data_list,panel+4,part,NULL)/3,
									                 Winloss1m=element_picker(module_data_list,panel+2,part,NULL),
									                 Winloss3m=element_picker(module_data_list,panel+5,part,NULL))
			part  <- 2
			output[[length(output)]] <- rbind(output[[length(output)]], data.frame(Side='Sell',
				                                     Description='Bef. Offs.',
				                                     Positions1M = element_picker(module_data_list,panel,part,'Hit')+element_picker(module_data_list,panel,part,'Miss'),
				                                     Positions3M = (element_picker(module_data_list,panel+3,part,'Hit')+element_picker(module_data_list,panel+3,part,'Miss'))/3,
				                                     Hits1M=element_picker(module_data_list,panel+1,part,NULL),
				                                     Hits3M=element_picker(module_data_list,panel+4,part,NULL)/3,
									                 Winloss1m=element_picker(module_data_list,panel+2,part,NULL),
									                 Winloss3m=element_picker(module_data_list,panel+5,part,NULL)))
			part  <- 3
			output[[length(output)]] <- rbind(output[[length(output)]], data.frame(Side='Buy',
				                                     Description='Bef. Ons.',
				                                     Positions1M = element_picker(module_data_list,panel,part,'Hit')+element_picker(module_data_list,panel,part,'Miss'),
				                                     Positions3M = (element_picker(module_data_list,panel+3,part,'Hit')+element_picker(module_data_list,panel+3,part,'Miss'))/3,
				                                     Hits1M=element_picker(module_data_list,panel+1,part,NULL),
				                                     Hits3M=element_picker(module_data_list,panel+4,part,NULL)/3,
									                 Winloss1m=element_picker(module_data_list,panel+2,part,NULL),
									                 Winloss3m=element_picker(module_data_list,panel+5,part,NULL)))
			part  <- 4
			output[[length(output)]] <- rbind(output[[length(output)]], data.frame(Side='Sell',
				                                     Description='Bef. Ons.',
				                                    Positions1M = element_picker(module_data_list,panel,part,'Hit')+element_picker(module_data_list,panel,part,'Miss'),
				                                     Positions3M = (element_picker(module_data_list,panel+3,part,'Hit')+element_picker(module_data_list,panel+3,part,'Miss'))/3,
				                                     Hits1M=element_picker(module_data_list,panel+1,part,NULL),
				                                     Hits3M=element_picker(module_data_list,panel+4,part,NULL)/3,
									                 Winloss1m=element_picker(module_data_list,panel+2,part,NULL),
									                 Winloss3m=element_picker(module_data_list,panel+5,part,NULL)))
			part  <- 5
			output[[length(output)]] <- rbind(output[[length(output)]], data.frame(Side='Buy',
				                                     Description='Aft. Offs.',
				                                     Positions1M = element_picker(module_data_list,panel,part,'Hit')+element_picker(module_data_list,panel,part,'Miss'),
				                                     Positions3M = (element_picker(module_data_list,panel+3,part,'Hit')+element_picker(module_data_list,panel+3,part,'Miss'))/3,
				                                     Hits1M=element_picker(module_data_list,panel+1,part,NULL),
				                                     Hits3M=element_picker(module_data_list,panel+4,part,NULL)/3,
									                 Winloss1m=element_picker(module_data_list,panel+2,part,NULL),
									                 Winloss3m=element_picker(module_data_list,panel+5,part,NULL)))
			part  <- 6
			output[[length(output)]] <- rbind(output[[length(output)]], data.frame(Side='Sell',
				                                     Description='Aft. Offs.',
				                                     Positions1M = element_picker(module_data_list,panel,part,'Hit')+element_picker(module_data_list,panel,part,'Miss'),
				                                     Positions3M = (element_picker(module_data_list,panel+3,part,'Hit')+element_picker(module_data_list,panel+3,part,'Miss'))/3,
				                                     Hits1M=element_picker(module_data_list,panel+1,part,NULL),
				                                     Hits3M=element_picker(module_data_list,panel+4,part,NULL)/3,
									                 Winloss1m=element_picker(module_data_list,panel+2,part,NULL),
									                 Winloss3m=element_picker(module_data_list,panel+5,part,NULL)))
			part  <- 7
			output[[length(output)]] <- rbind(output[[length(output)]], data.frame(Side='Buy',
				                                     Description='Aft. Ons.',
				                                     Positions1M = element_picker(module_data_list,panel,part,'Hit')+element_picker(module_data_list,panel,part,'Miss'),
				                                     Positions3M = (element_picker(module_data_list,panel+3,part,'Hit')+element_picker(module_data_list,panel+3,part,'Miss'))/3,
				                                     Hits1M=element_picker(module_data_list,panel+1,part,NULL),
				                                     Hits3M=element_picker(module_data_list,panel+4,part,NULL)/3,
									                 Winloss1m=element_picker(module_data_list,panel+2,part,NULL),
									                 Winloss3m=element_picker(module_data_list,panel+5,part,NULL)))
			part  <- 8
			output[[length(output)]] <- rbind(output[[length(output)]], data.frame(Side='Sell',
				                                     Description='Aft. Ons.',
				                                     Positions1M = element_picker(module_data_list,panel,part,'Hit')+element_picker(module_data_list,panel,part,'Miss'),
				                                     Positions3M = (element_picker(module_data_list,panel+3,part,'Hit')+element_picker(module_data_list,panel+3,part,'Miss'))/3,
				                                     Hits1M=element_picker(module_data_list,panel+1,part,NULL),
				                                     Hits3M=element_picker(module_data_list,panel+4,part,NULL)/3,
									                 Winloss1m=element_picker(module_data_list,panel+2,part,NULL),
									                 Winloss3m=element_picker(module_data_list,panel+5,part,NULL)))

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

trd_around_prmryplacing_analysis_module_builder <- new("AnalysisModuleFactory",name = "PrimaryPlacingTradeModule",ppmdl_class = "PrimaryPlacingPsnGatherer",visualisations = evnt_trded_rtn_visualistaions,panel_computation=trd_around_event_panel_comp,computation_name=trd_around_event_panel_comp_name)
trd_around_sndryplacing_analysis_module_builder <- new("AnalysisModuleFactory",name = "SecondaryPlacingTradeModule",ppmdl_class = "SecondaryPlacingPsnGatherer",visualisations = evnt_trded_rtn_visualistaions,panel_computation=trd_around_event_panel_comp,computation_name=trd_around_event_panel_comp_name)
