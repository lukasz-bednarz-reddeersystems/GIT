sourceTo("../analysis_modules/visualisation_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_module.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/ppmodel_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/trade_feature_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/module_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

# Assumes that module is being fed by a pre-processor module that produces the stats for a 3 month slice sampled monthly.
# The succesive months should be indexed by the ppmodel index with 3, refering to the most recent month.
# Analysis of the trades taken on results day

traded_psn_visuls <- list()

for(ppmodel_subset in c(3,1)){

	# Number of hits and misses by:
	#1. Psn long/short increase/decrease 

	results_day_panel <- new('Panel')
	results_day_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
	results_day_panel@visualns <- list(new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","PsnLong","Long","PsnTraded","ppModelIndex"),subset_by=c('PsnLong',"Long","PsnTraded","ppModelIndex"),subset_with=list(TRUE,TRUE,TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Long increases',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag"),
									   new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","PsnLong","Long","PsnTraded","ppModelIndex"),subset_by=c('PsnLong',"Long","PsnTraded","ppModelIndex"),subset_with=list(FALSE,FALSE,TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Short increases',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag"),
									   new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","PsnLong","Long","PsnTraded","ppModelIndex"),subset_by=c('PsnLong',"Long","PsnTraded","ppModelIndex"),subset_with=list(TRUE,FALSE,TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Long decreases',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag"),
									   new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","PsnLong","Long","PsnTraded","ppModelIndex"),subset_by=c('PsnLong',"Long","PsnTraded","ppModelIndex"),subset_with=list(FALSE,TRUE,TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Short decreases',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag"))
	traded_psn_visuls[[length(traded_psn_visuls)+1]] <- results_day_panel

	#2. Psn long/short new/old

	results_day_panel <- new('Panel')
	results_day_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
	results_day_panel@visualns <- list(new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","PsnLong","NewPosition","PsnTraded","ppModelIndex"),subset_by=c('PsnLong',"NewPosition","PsnTraded","ppModelIndex"),subset_with=list(TRUE,TRUE,TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Long new',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag"),
									   new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","PsnLong","NewPosition","PsnTraded","ppModelIndex"),subset_by=c('PsnLong',"NewPosition","PsnTraded","ppModelIndex"),subset_with=list(FALSE,TRUE,TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Short new',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag"),
									   new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","PsnLong","NewPosition","PsnTraded","ppModelIndex"),subset_by=c('PsnLong',"NewPosition","PsnTraded","ppModelIndex"),subset_with=list(TRUE,FALSE,TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Long old',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag"),
									   new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","PsnLong","NewPosition","PsnTraded","ppModelIndex"),subset_by=c('PsnLong',"NewPosition","PsnTraded","ppModelIndex"),subset_with=list(FALSE,FALSE,TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Short old',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag"))
	traded_psn_visuls[[length(traded_psn_visuls)+1]] <- results_day_panel

	#3. Psn long/short onside/offside 

	results_day_panel <- new('Panel')
	results_day_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
	results_day_panel@visualns <- list(new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","PsnOffside","PsnLong","PsnTraded","ppModelIndex"),subset_by=c('PsnLong',"PsnOffside","PsnTraded","ppModelIndex"),subset_with=list(TRUE,TRUE,TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Long offside',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag"),
									   new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","PsnOffside","PsnLong","PsnTraded","ppModelIndex"),subset_by=c('PsnLong',"PsnOffside","PsnTraded","ppModelIndex"),subset_with=list(FALSE,TRUE,TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Short offside',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag"),
									   new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","PsnOffside","PsnLong","PsnTraded","ppModelIndex"),subset_by=c('PsnLong',"PsnOffside","PsnTraded","ppModelIndex"),subset_with=list(TRUE,FALSE,TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Long onside',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag"),
									   new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","PsnOffside","PsnLong","PsnTraded","ppModelIndex"),subset_by=c('PsnLong',"PsnOffside","PsnTraded","ppModelIndex"),subset_with=list(FALSE,FALSE,TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Short onside',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag"))
	traded_psn_visuls[[length(traded_psn_visuls)+1]] <- results_day_panel

	#4. long/short +ve/-ve PnL going into trade

	results_day_panel <- new('Panel')
	results_day_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
	results_day_panel@visualns <- list(new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","PtvePnLInto","PsnLong","PsnTraded","ppModelIndex"),subset_by=c('PsnLong',"PtvePnLInto","PsnTraded","ppModelIndex"),subset_with=list(TRUE,TRUE,TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Long +ve PL',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag"),
									   new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","PtvePnLInto","PsnLong","PsnTraded","ppModelIndex"),subset_by=c('PsnLong',"PtvePnLInto","PsnTraded","ppModelIndex"),subset_with=list(FALSE,TRUE,TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Short +ve PL',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag"),
									   new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","PtvePnLInto","PsnLong","PsnTraded","ppModelIndex"),subset_by=c('PsnLong',"PtvePnLInto","PsnTraded","ppModelIndex"),subset_with=list(TRUE,FALSE,TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Long -ve PL',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag"),
									   new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Tag","PtvePnLInto","PsnLong","PsnTraded","ppModelIndex"),subset_by=c('PsnLong',"PtvePnLInto","PsnTraded","ppModelIndex"),subset_with=list(FALSE,FALSE,TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Short -ve PL',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Tag"))
	traded_psn_visuls[[length(traded_psn_visuls)+1]] <- results_day_panel

	#5. Win loss ratios for long/short increases/decreases

	results_day_panel <- new('Panel')
	results_day_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
	results_day_panel@visualns <- list(new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("Tag","PsnLong","Long","PsnTraded","ppModelIndex"),subset_by=c("Tag","PsnLong","Long","PsnTraded","ppModelIndex"),subset_with=list(list('Hit',TRUE,TRUE,TRUE,ppmodel_subset),list('Miss',TRUE,TRUE,TRUE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Long increase",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset),
								       new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("Tag","PsnLong","Long","PsnTraded","ppModelIndex"),subset_by=c("Tag","PsnLong","Long","PsnTraded","ppModelIndex"),subset_with=list(list('Hit',FALSE,FALSE,TRUE,ppmodel_subset),list('Miss',FALSE,FALSE,TRUE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Short increase",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset),
								       new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("Tag","PsnLong","Long","PsnTraded","ppModelIndex"),subset_by=c("Tag","PsnLong","Long","PsnTraded","ppModelIndex"),subset_with=list(list('Hit',TRUE,FALSE,TRUE,ppmodel_subset),list('Miss',TRUE,FALSE,TRUE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Long decrease",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset),
								       new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("Tag","PsnLong","Long","PsnTraded","ppModelIndex"),subset_by=c("Tag","PsnLong","Long","PsnTraded","ppModelIndex"),subset_with=list(list('Hit',FALSE,TRUE,TRUE,ppmodel_subset),list('Miss',FALSE,TRUE,TRUE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Short decrease",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset))		
	traded_psn_visuls[[length(traded_psn_visuls)+1]] <- results_day_panel

	#6. Win loss ratios for long/short new/old

	results_day_panel <- new('Panel')
	results_day_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
	results_day_panel@visualns <- list(new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("Tag","NewPosition","PsnLong","PsnTraded","ppModelIndex"),subset_by=c("Tag","NewPosition","PsnLong","PsnTraded","ppModelIndex"),subset_with=list(list('Hit',TRUE,TRUE,TRUE,ppmodel_subset),list('Miss',TRUE,TRUE,TRUE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="New long",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset),
								       new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("Tag","NewPosition","PsnLong","PsnTraded","ppModelIndex"),subset_by=c("Tag","NewPosition","PsnLong","PsnTraded","ppModelIndex"),subset_with=list(list('Hit',TRUE,FALSE,TRUE,ppmodel_subset),list('Miss',TRUE,FALSE,TRUE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="New short",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset),
								       new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("Tag","NewPosition","PsnLong","PsnTraded","ppModelIndex"),subset_by=c("Tag","NewPosition","PsnLong","PsnTraded","ppModelIndex"),subset_with=list(list('Hit',FALSE,TRUE,TRUE,ppmodel_subset),list('Miss',FALSE,TRUE,TRUE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Old long",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset),
								       new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("Tag","NewPosition","PsnLong","PsnTraded","ppModelIndex"),subset_by=c("Tag","NewPosition","PsnLong","PsnTraded","ppModelIndex"),subset_with=list(list('Hit',FALSE,FALSE,TRUE,ppmodel_subset),list('Miss',FALSE,FALSE,TRUE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Old short",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset))		
	traded_psn_visuls[[length(traded_psn_visuls)+1]] <- results_day_panel

	#7. Win loss ratios for long/short onside/offside 

	results_day_panel <- new('Panel')
	results_day_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
	results_day_panel@visualns <- list(new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("Tag","PsnOffside","PsnLong","PsnTraded","ppModelIndex"),subset_by=c("Tag","PsnOffside","PsnLong","PsnTraded","ppModelIndex"),subset_with=list(list('Hit',TRUE,TRUE,TRUE,ppmodel_subset),list('Miss',TRUE,TRUE,TRUE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Long offside",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset),
								       new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("Tag","PsnOffside","PsnLong","PsnTraded","ppModelIndex"),subset_by=c("Tag","PsnOffside","PsnLong","PsnTraded","ppModelIndex"),subset_with=list(list('Hit',TRUE,FALSE,TRUE,ppmodel_subset),list('Miss',TRUE,FALSE,TRUE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Short offside",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset),
								       new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("Tag","PsnOffside","PsnLong","PsnTraded","ppModelIndex"),subset_by=c("Tag","PsnOffside","PsnLong","PsnTraded","ppModelIndex"),subset_with=list(list('Hit',FALSE,TRUE,TRUE,ppmodel_subset),list('Miss',FALSE,TRUE,TRUE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Long onside",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset),
								       new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("Tag","PsnOffside","PsnLong","PsnTraded","ppModelIndex"),subset_by=c("Tag","PsnOffside","PsnLong","PsnTraded","ppModelIndex"),subset_with=list(list('Hit',FALSE,FALSE,TRUE,ppmodel_subset),list('Miss',FALSE,FALSE,TRUE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Short offside",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset))		
	traded_psn_visuls[[length(traded_psn_visuls)+1]] <- results_day_panel

	#8. Win loss ratios for +ve/-ve PnL going into trade

	results_day_panel <- new('Panel')
	results_day_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
	results_day_panel@visualns <- list(new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("Tag","PtvePnLInto","PsnLong","PsnTraded","ppModelIndex"),subset_by=c("Tag","PtvePnLInto","PsnLong","PsnTraded","ppModelIndex"),subset_with=list(list('Hit',TRUE,TRUE,TRUE,ppmodel_subset),list('Miss',TRUE,TRUE,TRUE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Long +ve PL",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset),
								       new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("Tag","PtvePnLInto","PsnLong","PsnTraded","ppModelIndex"),subset_by=c("Tag","PtvePnLInto","PsnLong","PsnTraded","ppModelIndex"),subset_with=list(list('Hit',TRUE,FALSE,TRUE,ppmodel_subset),list('Miss',TRUE,FALSE,TRUE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Short +ve PL",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset),
								       new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("Tag","PtvePnLInto","PsnLong","PsnTraded","ppModelIndex"),subset_by=c("Tag","PtvePnLInto","PsnLong","PsnTraded","ppModelIndex"),subset_with=list(list('Hit',FALSE,TRUE,TRUE,ppmodel_subset),list('Miss',FALSE,TRUE,TRUE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Long -ve PL",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset),
								       new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("Tag","PtvePnLInto","PsnLong","PsnTraded","ppModelIndex"),subset_by=c("Tag","PtvePnLInto","PsnLong","PsnTraded","ppModelIndex"),subset_with=list(list('Hit',FALSE,FALSE,TRUE,ppmodel_subset),list('Miss',FALSE,FALSE,TRUE,ppmodel_subset)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Short -ve PL",subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset))		
	traded_psn_visuls[[length(traded_psn_visuls)+1]] <- results_day_panel

}

#Tabulate data:

results_daypsn_panel_comp_name <- "HitRateByCategory"

results_daypsn_panel_comp <- function(module_data_list,name){
	output <- list()
	data_names <- c()
	#1. Hit rate for long/short position traded/untraded on day 0
	#	Uses panels 2 (increases) and 3 (decreases), ratios in element 5 (long) and 6 (short)
	nme <- "Traded position hit rate."
	column_names <- c('STRING|Side','STRING|Context','INT|Trades 1m','QUANTITY_1DP|Trades 3m','PERCENT_1DP|Hit rate 1m','PERCENT_1DP|Hit rate 3m','QUANTITY_1DP|Win loss 1m','QUANTITY_1DP|Win loss 3m')
	tryCatch({
			output[[length(output)+1]] <- data.frame(Side='Buy',
				                                     Description='Increase',
				                                     Positions1M = element_picker(module_data_list,1,1,'Hit')+element_picker(module_data_list,1,1,'Miss'),
				                                     Positions3M = (element_picker(module_data_list,9,1,'Hit')+element_picker(module_data_list,9,1,'Miss'))/3,
				                                     Hits1M=(element_picker(module_data_list,1,1,'Hit')/(element_picker(module_data_list,1,1,'Hit')+element_picker(module_data_list,1,1,'Miss'))),
				                                     Hits3M=(element_picker(module_data_list,9,1,'Hit')/(element_picker(module_data_list,9,1,'Hit')+element_picker(module_data_list,9,1,'Miss'))),
									                 Winloss1m=(element_picker(module_data_list,5,1,1)),
									                 Winloss3m=(element_picker(module_data_list,13,1,1)))
			output[[length(output)]] <- rbind(output[[length(output)]],data.frame(Side='Buy',
				                                     Description='Decrease',
				                                     Positions1M = element_picker(module_data_list,1,3,'Hit')+element_picker(module_data_list,1,3,'Miss'),
				                                     Positions3M = (element_picker(module_data_list,9,3,'Hit')+element_picker(module_data_list,9,3,'Miss'))/3,
				                                     Hits1M=(element_picker(module_data_list,1,3,'Hit')/(element_picker(module_data_list,1,3,'Hit')+element_picker(module_data_list,1,3,'Miss'))),
				                                     Hits3M=(element_picker(module_data_list,9,3,'Hit')/(element_picker(module_data_list,9,3,'Hit')+element_picker(module_data_list,9,3,'Miss'))),
									                 Winloss1m=(element_picker(module_data_list,5,3,1)),
									                 Winloss3m=(element_picker(module_data_list,13,3,1))))
			output[[length(output)]] <- rbind(output[[length(output)]],data.frame(Side='Sell',
				                                     Description='Increase',
				                                     Positions1M = element_picker(module_data_list,1,2,'Hit')+element_picker(module_data_list,1,2,'Miss'),
				                                     Positions3M = (element_picker(module_data_list,9,2,'Hit')+element_picker(module_data_list,9,2,'Miss'))/3,
				                                     Hits1M=(element_picker(module_data_list,1,2,'Hit')/(element_picker(module_data_list,1,2,'Hit')+element_picker(module_data_list,1,2,'Miss'))),
				                                     Hits3M=(element_picker(module_data_list,9,2,'Hit')/(element_picker(module_data_list,9,2,'Hit')+element_picker(module_data_list,9,2,'Miss'))),
									                 Winloss1m=(element_picker(module_data_list,5,2,1)),
									                 Winloss3m=(element_picker(module_data_list,13,2,1))))
			output[[length(output)]] <- rbind(output[[length(output)]],data.frame(Side='Sell',
				                                     Description='Decrease',
				                                     Positions1M = element_picker(module_data_list,1,4,'Hit')+element_picker(module_data_list,1,4,'Miss'),
				                                     Positions3M = (element_picker(module_data_list,9,4,'Hit')+element_picker(module_data_list,9,4,'Miss'))/3,
				                                     Hits1M=(element_picker(module_data_list,1,4,'Hit')/(element_picker(module_data_list,1,4,'Hit')+element_picker(module_data_list,1,4,'Miss'))),
				                                     Hits3M=(element_picker(module_data_list,9,4,'Hit')/(element_picker(module_data_list,9,4,'Hit')+element_picker(module_data_list,9,4,'Miss'))),
									                 Winloss1m=(element_picker(module_data_list,5,4,1)),
									                 Winloss3m=(element_picker(module_data_list,13,4,1))))
			output[[length(output)]] <- rbind(output[[length(output)]],data.frame(Side='Buy',
				                                     Description='New',
				                                     Positions1M = element_picker(module_data_list,2,1,'Hit')+element_picker(module_data_list,2,1,'Miss'),
				                                     Positions3M = (element_picker(module_data_list,10,1,'Hit')+element_picker(module_data_list,10,1,'Miss'))/3,
				                                     Hits1M=(element_picker(module_data_list,2,1,'Hit')/(element_picker(module_data_list,2,1,'Hit')+element_picker(module_data_list,2,1,'Miss'))),
				                                     Hits3M=(element_picker(module_data_list,10,1,'Hit')/(element_picker(module_data_list,10,1,'Hit')+element_picker(module_data_list,10,1,'Miss'))),
									                 Winloss1m=(element_picker(module_data_list,6,1,1)),
									                 Winloss3m=(element_picker(module_data_list,14,1,1))))
			output[[length(output)]] <- rbind(output[[length(output)]],data.frame(Side='Buy',
				                                     Description='Old',
				                                     Positions1M = element_picker(module_data_list,2,3,'Hit')+element_picker(module_data_list,2,3,'Miss'),
				                                     Positions3M = (element_picker(module_data_list,10,3,'Hit')+element_picker(module_data_list,10,3,'Miss'))/3,
				                                     Hits1M=(element_picker(module_data_list,2,3,'Hit')/(element_picker(module_data_list,2,3,'Hit')+element_picker(module_data_list,2,3,'Miss'))),
				                                     Hits3M=(element_picker(module_data_list,10,3,'Hit')/(element_picker(module_data_list,10,3,'Hit')+element_picker(module_data_list,10,3,'Miss'))),
									                 Winloss1m=(element_picker(module_data_list,6,3,1)),
									                 Winloss3m=(element_picker(module_data_list,14,3,1))))
			output[[length(output)]] <- rbind(output[[length(output)]],data.frame(Side='Sell',
				                                     Description='New',
				                                     Positions1M = element_picker(module_data_list,2,2,'Hit')+element_picker(module_data_list,2,2,'Miss'),
				                                     Positions3M = (element_picker(module_data_list,10,2,'Hit')+element_picker(module_data_list,10,2,'Miss'))/3,
				                                     Hits1M=(element_picker(module_data_list,2,2,'Hit')/(element_picker(module_data_list,2,2,'Hit')+element_picker(module_data_list,2,2,'Miss'))),
				                                     Hits3M=(element_picker(module_data_list,10,2,'Hit')/(element_picker(module_data_list,10,2,'Hit')+element_picker(module_data_list,10,2,'Miss'))),
									                 Winloss1m=(element_picker(module_data_list,6,2,1)),
									                 Winloss3m=(element_picker(module_data_list,14,2,1))))
			output[[length(output)]] <- rbind(output[[length(output)]],data.frame(Side='Sell',
				                                     Description='Old',
				                                     Positions1M = element_picker(module_data_list,2,4,'Hit')+element_picker(module_data_list,2,4,'Miss'),
				                                     Positions3M = (element_picker(module_data_list,10,4,'Hit')+element_picker(module_data_list,10,4,'Miss'))/3,
				                                     Hits1M=(element_picker(module_data_list,2,4,'Hit')/(element_picker(module_data_list,2,4,'Hit')+element_picker(module_data_list,2,4,'Miss'))),
				                                     Hits3M=(element_picker(module_data_list,10,4,'Hit')/(element_picker(module_data_list,10,4,'Hit')+element_picker(module_data_list,10,4,'Miss'))),
									                 Winloss1m=(element_picker(module_data_list,6,4,1)),
									                 Winloss3m=(element_picker(module_data_list,14,4,1))))
			output[[length(output)]] <- rbind(output[[length(output)]],data.frame(Side='Buy',
				                                     Description='Offside',
				                                     Positions1M = element_picker(module_data_list,3,1,'Hit')+element_picker(module_data_list,3,1,'Miss'),
				                                     Positions3M = (element_picker(module_data_list,11,1,'Hit')+element_picker(module_data_list,11,1,'Miss'))/3,
				                                     Hits1M=(element_picker(module_data_list,3,1,'Hit')/(element_picker(module_data_list,3,1,'Hit')+element_picker(module_data_list,3,1,'Miss'))),
				                                     Hits3M=(element_picker(module_data_list,11,1,'Hit')/(element_picker(module_data_list,11,1,'Hit')+element_picker(module_data_list,11,1,'Miss'))),
									                 Winloss1m=(element_picker(module_data_list,7,1,1)),
									                 Winloss3m=(element_picker(module_data_list,15,1,1))))
			output[[length(output)]] <- rbind(output[[length(output)]],data.frame(Side='Buy',
				                                     Description='Onside',
				                                     Positions1M = element_picker(module_data_list,3,3,'Hit')+element_picker(module_data_list,3,3,'Miss'),
				                                     Positions3M = (element_picker(module_data_list,11,3,'Hit')+element_picker(module_data_list,11,3,'Miss'))/3,
				                                     Hits1M=(element_picker(module_data_list,3,3,'Hit')/(element_picker(module_data_list,3,3,'Hit')+element_picker(module_data_list,3,3,'Miss'))),
				                                     Hits3M=(element_picker(module_data_list,11,3,'Hit')/(element_picker(module_data_list,11,3,'Hit')+element_picker(module_data_list,11,3,'Miss'))),
									                 Winloss1m=(element_picker(module_data_list,7,3,1)),
									                 Winloss3m=(element_picker(module_data_list,15,3,1))))
			output[[length(output)]] <- rbind(output[[length(output)]],data.frame(Side='Sell',
				                                     Description='Offside',
				                                     Positions1M = element_picker(module_data_list,3,2,'Hit')+element_picker(module_data_list,3,2,'Miss'),
				                                     Positions3M = (element_picker(module_data_list,11,2,'Hit')+element_picker(module_data_list,11,2,'Miss'))/3,
				                                     Hits1M=(element_picker(module_data_list,3,2,'Hit')/(element_picker(module_data_list,3,2,'Hit')+element_picker(module_data_list,3,2,'Miss'))),
				                                     Hits3M=(element_picker(module_data_list,11,2,'Hit')/(element_picker(module_data_list,11,2,'Hit')+element_picker(module_data_list,11,2,'Miss'))),
									                 Winloss1m=(element_picker(module_data_list,7,2,1)),
									                 Winloss3m=(element_picker(module_data_list,15,2,1))))
			output[[length(output)]] <- rbind(output[[length(output)]],data.frame(Side='Sell',
				                                     Description='Onside',
				                                     Positions1M = element_picker(module_data_list,3,4,'Hit')+element_picker(module_data_list,3,4,'Miss'),
				                                     Positions3M = (element_picker(module_data_list,11,4,'Hit')+element_picker(module_data_list,11,4,'Miss'))/3,
				                                     Hits1M=(element_picker(module_data_list,3,4,'Hit')/(element_picker(module_data_list,3,4,'Hit')+element_picker(module_data_list,3,4,'Miss'))),
				                                     Hits3M=(element_picker(module_data_list,11,4,'Hit')/(element_picker(module_data_list,11,4,'Hit')+element_picker(module_data_list,11,4,'Miss'))),
									                 Winloss1m=(element_picker(module_data_list,7,4,1)),
									                 Winloss3m=(element_picker(module_data_list,15,4,1))))
			output[[length(output)]] <- rbind(output[[length(output)]],data.frame(Side='Buy',
				                                     Description='+ve PL in',
				                                     Positions1M = element_picker(module_data_list,4,1,'Hit')+element_picker(module_data_list,4,1,'Miss'),
				                                     Positions3M = (element_picker(module_data_list,12,1,'Hit')+element_picker(module_data_list,12,1,'Miss'))/3,
				                                     Hits1M=(element_picker(module_data_list,4,1,'Hit')/(element_picker(module_data_list,4,1,'Hit')+element_picker(module_data_list,4,1,'Miss'))),
				                                     Hits3M=(element_picker(module_data_list,12,1,'Hit')/(element_picker(module_data_list,12,1,'Hit')+element_picker(module_data_list,12,1,'Miss'))),
									                 Winloss1m=(element_picker(module_data_list,8,1,1)),
									                 Winloss3m=(element_picker(module_data_list,16,1,1))))
			output[[length(output)]] <- rbind(output[[length(output)]],data.frame(Side='Buy',
				                                     Description='-ve PL in',
				                                     Positions1M = element_picker(module_data_list,4,3,'Hit')+element_picker(module_data_list,4,3,'Miss'),
				                                     Positions3M = (element_picker(module_data_list,12,3,'Hit')+element_picker(module_data_list,12,3,'Miss'))/3,
				                                     Hits1M=(element_picker(module_data_list,4,3,'Hit')/(element_picker(module_data_list,4,3,'Hit')+element_picker(module_data_list,4,3,'Miss'))),
				                                     Hits3M=(element_picker(module_data_list,12,3,'Hit')/(element_picker(module_data_list,12,3,'Hit')+element_picker(module_data_list,12,3,'Miss'))),
									                 Winloss1m=(element_picker(module_data_list,8,3,1)),
									                 Winloss3m=(element_picker(module_data_list,16,3,1))))
			output[[length(output)]] <- rbind(output[[length(output)]],data.frame(Side='Sell',
				                                     Description='+ve PL in',
				                                     Positions1M = element_picker(module_data_list,4,2,'Hit')+element_picker(module_data_list,4,2,'Miss'),
				                                     Positions3M = (element_picker(module_data_list,12,2,'Hit')+element_picker(module_data_list,12,2,'Miss'))/3,
				                                     Hits1M=(element_picker(module_data_list,4,2,'Hit')/(element_picker(module_data_list,4,2,'Hit')+element_picker(module_data_list,4,2,'Miss'))),
				                                     Hits3M=(element_picker(module_data_list,12,2,'Hit')/(element_picker(module_data_list,12,2,'Hit')+element_picker(module_data_list,12,2,'Miss'))),
									                 Winloss1m=(element_picker(module_data_list,8,2,1)),
									                 Winloss3m=(element_picker(module_data_list,16,2,1))))
			output[[length(output)]] <- rbind(output[[length(output)]],data.frame(Side='Sell',
				                                     Description='-ve PL in',
				                                     Positions1M = element_picker(module_data_list,4,4,'Hit')+element_picker(module_data_list,4,4,'Miss'),
				                                     Positions3M = (element_picker(module_data_list,12,4,'Hit')+element_picker(module_data_list,12,4,'Miss'))/3,
				                                     Hits1M=(element_picker(module_data_list,4,4,'Hit')/(element_picker(module_data_list,4,4,'Hit')+element_picker(module_data_list,4,4,'Miss'))),
				                                     Hits3M=(element_picker(module_data_list,12,4,'Hit')/(element_picker(module_data_list,12,4,'Hit')+element_picker(module_data_list,12,4,'Miss'))),
									                 Winloss1m=(element_picker(module_data_list,8,4,1)),
									                 Winloss3m=(element_picker(module_data_list,16,4,1))))
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

results_daypsntraded_analysis_module_builder <- new("AnalysisModuleFactory",name = "ResultsDayPsnTradedModule",ppmdl_class = "ResultsDayPsnGatherer",visualisations = traded_psn_visuls,panel_computation=results_daypsn_panel_comp,computation_name=results_daypsn_panel_comp_name)
