#Definition file for a report analysis module to compute 
#trading performance around stock results day.

sourceTo("../analysis_modules_legacy/visualisation_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules_legacy/analysis_module.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/ppmodel_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/trade_feature_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules_legacy/module_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

#Create two row, two column panel to generate plots for total buys and sells on each day
#pre/post results.

#1. Number of buys in long strategies and sells in short strategies on each day around the results.

results_day_panel <- new('Panel')
results_day_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
results_day_panel@visualns <- list(new("FrequencyPlot",aggregate_what="Long",aggregate_by=c("DaysSinceLastResults","PsnLong"),subset_by=c('PsnLong'),subset_with=list(TRUE),aggregate_fn=sum,y_label="Number of buys",x_label="Days since last results",title="Long positions",x_label_variable="DaysSinceLastResults"),
								   new("FrequencyPlot",aggregate_what="Long",aggregate_by=c("DaysSinceLastResults","PsnLong"),subset_by=c('PsnLong'),subset_with=list(FALSE),aggregate_fn=function(x)sum(!x),y_label="Number of sells",x_label="Days since last results",title="Short positions",x_label_variable="DaysSinceLastResults"),
								   new("FrequencyPlot",aggregate_what="Long",aggregate_by=c("DaysToNextResults","PsnLong"),subset_by=c('PsnLong'),subset_with=list(TRUE),aggregate_fn=sum,y_label="Number of buys",x_label="Days to next results",x_label_variable="DaysToNextResults"),
								   new("FrequencyPlot",aggregate_what="Long",aggregate_by=c("DaysToNextResults","PsnLong"),subset_by=c('PsnLong'),subset_with=list(FALSE),aggregate_fn=function(x)sum(!x),y_label="Number of sells",x_label="Days to next results",x_label_variable="DaysToNextResults"))		
rd_visualisations <- list(results_day_panel)

#2. Hits/misses counts for buys in long strategies and sells in short strategies on each day post results.
#	Hits/misses ratio for buys in long strategies and sells in short strategies on each day post results.

results_day_panel <- new('Panel')
results_day_panel@parameters <- 'mfrow=c(3,2),mar=c(6,4,2.5,2.5)'
results_day_panel@visualns <- list(new("FrequencyPlot",aggregate_what="Long",aggregate_by=c("DaysSinceLastResults","PsnLong","Hit1D"),subset_by=c('PsnLong','Hit1D'),subset_with=list(TRUE,TRUE),aggregate_fn=sum,y_label="Number of hits",x_label="Days since last results",title="Long position increases",x_label_variable="DaysSinceLastResults"),
								   new("FrequencyPlot",aggregate_what="Long",aggregate_by=c("DaysSinceLastResults","PsnLong","Hit1D"),subset_by=c('PsnLong','Hit1D'),subset_with=list(FALSE,TRUE),aggregate_fn=function(x)sum(!x),y_label="Number of hits",x_label="Days since last results",title="Short position increases",x_label_variable="DaysSinceLastResults"),
								   new("FrequencyPlot",aggregate_what="Long",aggregate_by=c("DaysSinceLastResults","PsnLong","Hit1D"),subset_by=c('PsnLong','Hit1D'),subset_with=list(TRUE,FALSE),aggregate_fn=sum,y_label="Number of misses",x_label="Days since last results",x_label_variable="DaysSinceLastResults"),
								   new("FrequencyPlot",aggregate_what="Long",aggregate_by=c("DaysSinceLastResults","PsnLong","Hit1D"),subset_by=c('PsnLong','Hit1D'),subset_with=list(FALSE,FALSE),aggregate_fn=function(x)sum(!x),y_label="Number of misses",x_label="Days since last results",x_label_variable="DaysSinceLastResults"),
								   new("FrequencyPlot",aggregate_what="Long",aggregate_by=c("DaysSinceLastResults","PsnLong","Hit1D"),subset_by=c('PsnLong','Hit1D'),subset_with=list(list(TRUE,TRUE),list(TRUE,FALSE)),aggregate_fn=sum,y_label="Hit/miss ratio",x_label="Days since last results",x_label_variable="DaysSinceLastResults",visuln_comp=data_aggregate_ratio_by_subset),
								   new("FrequencyPlot",aggregate_what="Long",aggregate_by=c("DaysSinceLastResults","PsnLong","Hit1D"),subset_by=c('PsnLong','Hit1D'),subset_with=list(list(FALSE,TRUE),list(FALSE,FALSE)),aggregate_fn=function(x)sum(!x),y_label="Hit/miss ratio",x_label="Days since last results",x_label_variable="DaysSinceLastResults",visuln_comp=data_aggregate_ratio_by_subset))		
rd_visualisations[[2]] <- results_day_panel

# Hits/misses counts for sells in long strategies and buys in short strategies on each day post results.

results_day_panel <- new('Panel')
results_day_panel@parameters <- 'mfrow=c(3,2),mar=c(6,4,2.5,2.5)'
results_day_panel@visualns <- list(new("FrequencyPlot",aggregate_what="Long",aggregate_by=c("DaysSinceLastResults","PsnLong","Hit1D"),subset_by=c('PsnLong','Hit1D'),subset_with=list(TRUE,TRUE),aggregate_fn=function(x)sum(!x),y_label="Number of hits",x_label="Days since last results",title="Long position decreases",x_label_variable="DaysSinceLastResults"),
								   new("FrequencyPlot",aggregate_what="Long",aggregate_by=c("DaysSinceLastResults","PsnLong","Hit1D"),subset_by=c('PsnLong','Hit1D'),subset_with=list(FALSE,TRUE),aggregate_fn=sum,y_label="Number of hits",x_label="Days since last results",title="Short position decreases",x_label_variable="DaysSinceLastResults"),
								   new("FrequencyPlot",aggregate_what="Long",aggregate_by=c("DaysSinceLastResults","PsnLong","Hit1D"),subset_by=c('PsnLong','Hit1D'),subset_with=list(TRUE,FALSE),aggregate_fn=function(x)sum(!x),y_label="Number of misses",x_label="Days since last results",x_label_variable="DaysSinceLastResults"),
								   new("FrequencyPlot",aggregate_what="Long",aggregate_by=c("DaysSinceLastResults","PsnLong","Hit1D"),subset_by=c('PsnLong','Hit1D'),subset_with=list(FALSE,FALSE),aggregate_fn=sum,y_label="Number of misses",x_label="Days since last results",x_label_variable="DaysSinceLastResults"),
								   new("FrequencyPlot",aggregate_what="Long",aggregate_by=c("DaysSinceLastResults","PsnLong","Hit1D"),subset_by=c('PsnLong','Hit1D'),subset_with=list(list(TRUE,TRUE),list(TRUE,FALSE)),aggregate_fn=function(x)sum(!x),y_label="Hit/miss ratio",x_label="Days since last results",x_label_variable="DaysSinceLastResults",visuln_comp=data_aggregate_ratio_by_subset),
								   new("FrequencyPlot",aggregate_what="Long",aggregate_by=c("DaysSinceLastResults","PsnLong","Hit1D"),subset_by=c('PsnLong','Hit1D'),subset_with=list(list(FALSE,TRUE),list(FALSE,FALSE)),aggregate_fn=sum,y_label="Hit/miss ratio",x_label="Days since last results",x_label_variable="DaysSinceLastResults",visuln_comp=data_aggregate_ratio_by_subset))		
rd_visualisations[[3]] <- results_day_panel

#3.	Repeat ratio for:
#		existing/new psns 

results_day_panel <- new('Panel')
results_day_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
results_day_panel@visualns <- list(new("FrequencyPlot",aggregate_what="Long",aggregate_by=c("DaysSinceLastResults",'PsnLong',"Hit1D","NewPosition"),subset_by=c('PsnLong','Hit1D','NewPosition'),subset_with=list(list(TRUE,TRUE,TRUE),list(TRUE,FALSE,TRUE)),aggregate_fn=function(x)sum(length(x)),y_label="Hit/miss ratio (new psn)",x_label="Days since last results",title="Long positions",x_label_variable="DaysSinceLastResults",visuln_comp=data_aggregate_ratio_by_subset),
								   new("FrequencyPlot",aggregate_what="Long",aggregate_by=c("DaysSinceLastResults",'PsnLong',"Hit1D","NewPosition"),subset_by=c('PsnLong','Hit1D','NewPosition'),subset_with=list(list(FALSE,TRUE,TRUE),list(FALSE,FALSE,TRUE)),aggregate_fn=function(x)sum(length(x)),y_label="Hit/miss ratio (new psn)",x_label="Days since last results",title="Short positions",x_label_variable="DaysSinceLastResults",visuln_comp=data_aggregate_ratio_by_subset),
								   new("FrequencyPlot",aggregate_what="Long",aggregate_by=c("DaysSinceLastResults",'PsnLong',"Hit1D","NewPosition"),subset_by=c('PsnLong','Hit1D','NewPosition'),subset_with=list(list(TRUE,TRUE,FALSE),list(TRUE,FALSE,FALSE)),aggregate_fn=function(x)sum(length(x)),y_label="Hit/miss ratio (old psn)",x_label="Days since last results",x_label_variable="DaysSinceLastResults",visuln_comp=data_aggregate_ratio_by_subset),
								   new("FrequencyPlot",aggregate_what="Long",aggregate_by=c("DaysSinceLastResults",'PsnLong',"Hit1D","NewPosition"),subset_by=c('PsnLong','Hit1D','NewPosition'),subset_with=list(list(FALSE,TRUE,FALSE),list(FALSE,FALSE,FALSE)),aggregate_fn=function(x)sum(length(x)),y_label="Hit/miss ratio (old psn)",x_label="Days since last results",x_label_variable="DaysSinceLastResults",visuln_comp=data_aggregate_ratio_by_subset))		
rd_visualisations[[4]] <- results_day_panel

#4.     Position level:
#		Position onside/offside
#       +ve/-ve PnL going into trade

results_day_panel <- new('Panel')
results_day_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
results_day_panel@visualns <- list(new("FrequencyPlot",aggregate_what="Long",aggregate_by=c("DaysSinceLastResults","Hit1D","PsnOffside"),subset_by=c("Hit1D","PsnOffside"),subset_with=list(list(TRUE,TRUE),list(FALSE,TRUE)),aggregate_fn=function(x)sum(length(x)),y_label="Hit/miss ration",x_label="Days since last results",title="Onside positions",x_label_variable="DaysSinceLastResults",visuln_comp=data_aggregate_ratio_by_subset),
								   new("FrequencyPlot",aggregate_what="Long",aggregate_by=c("DaysSinceLastResults","Hit1D","PsnOffside"),subset_by=c("Hit1D","PsnOffside"),subset_with=list(list(TRUE,FALSE),list(FALSE,FALSE)),aggregate_fn=function(x)sum(length(x)),y_label="Hit/miss ratio",x_label="Days since last results",title="Offside positions",x_label_variable="DaysSinceLastResults",visuln_comp=data_aggregate_ratio_by_subset),
								   new("FrequencyPlot",aggregate_what="Long",aggregate_by=c("DaysSinceLastResults","Hit1D","PtvePnLInto"),subset_by=c('Hit1D','PtvePnLInto'),subset_with=list(list(TRUE,TRUE),list(FALSE,TRUE)),aggregate_fn=sum,y_label="Hit/miss ratio",x_label="Days since last results",title="Positive pre-5d PnL",x_label_variable="DaysSinceLastResults",visuln_comp=data_aggregate_ratio_by_subset),
								   new("FrequencyPlot",aggregate_what="Long",aggregate_by=c("DaysSinceLastResults","Hit1D","PtvePnLInto"),subset_by=c('Hit1D','PtvePnLInto'),subset_with=list(list(TRUE,FALSE),list(FALSE,FALSE)),aggregate_fn=function(x)sum(!x),y_label="Hit/miss ratio",x_label="Days since last results",title="Negative pre-5d PnL",x_label_variable="DaysSinceLastResults",visuln_comp=data_aggregate_ratio_by_subset))		
rd_visualisations[[5]] <- results_day_panel								

#Panel computation to create summary data for each panel if required. 
#Takes as input, list of lists containing all panel data
#plus the computation name.
#Hence can arbitraily combine/filter data from the panels above
results_day_panel_comp_name <- "HitRateByCategory"

results_day_panel_comp <- function(module_data_list,name){
	output <- list()
	data_names <- c()
	#1. Hit rate for long/short position increases/decreases on day 0
	#	Uses panels 2 (increases) and 3 (decreases), ratios in element 5 (long) and 6 (short)
	nme <- "Hit rate by position increase/decrease."
	tryCatch({
			output[[length(output)+1]] <- data.frame(LongIncrease=convert_to_rate(element_picker(module_data_list,2,5,'0')),
									  LongDecrease=convert_to_rate(element_picker(module_data_list,3,5,'0')),
									  ShortIncrease=convert_to_rate(element_picker(module_data_list,2,6,'0')),
									  ShortDecrease=convert_to_rate(element_picker(module_data_list,3,6,'0')))
			data_names <- c(data_names,nme)
			rownames(output[[length(output)]]) <- name
			}, error= function(cond){
				message(paste("Error applying panel computation",name,"in",nme,":",cond))
			})
	#2. Hit rate for long/short old/new positions
	#   Uses panel 4, ratios in elements 1 (new, long), 2 (new, short)
	#   3 (old, long), 4 (old short)
	nme <- "Hit rate by old/new positions."
	tryCatch({
			output[[length(output)+1]] <- data.frame(NewLong=convert_to_rate(element_picker(module_data_list,4,1,'0')),
									  NewShort=convert_to_rate(element_picker(module_data_list,4,2,'0')),
									  OldLong=convert_to_rate(element_picker(module_data_list,4,3,'0')),
									  OldShort=convert_to_rate(element_picker(module_data_list,4,4,'0')))
			data_names <- c(data_names,nme)
			rownames(output[[length(output)]]) <- name
			}, error= function(cond){
				message(paste("Error applying panel computation",name,"in",nme,":",cond))
			})
	#3. Hit rate for onside/offside positions
	#   Uses panel 5 elements 1 (onside), 2 (offside),
	nme <- "Hit rate by onside/offside."
	tryCatch({
			output[[length(output)+1]] <- data.frame(Onside=convert_to_rate(element_picker(module_data_list,5,1,'0')),
									  Offside=convert_to_rate(element_picker(module_data_list,5,2,'0')))
			data_names <- c(data_names,nme)
			rownames(output[[length(output)]]) <- name
			}, error= function(cond){
				message(paste("Error applying panel computation",name,"in",nme,":",cond))
			})
	#4. Hit rate for PnL into trade
	#   Uses panel 5 elements 3 (+ve PnL into) and 4 (-ve PnL into)
	nme <- "Hit rate by PnL."
	tryCatch({
			output[[length(output)+1]] <- data.frame(PtvePnl=convert_to_rate(element_picker(module_data_list,5,3,'0')),
									  NtvePnl=convert_to_rate(element_picker(module_data_list,5,4,'0')))
			data_names <- c(data_names,nme)
			rownames(output[[length(output)]]) <- name
			}, error= function(cond){
				message(paste("Error applying panel computation",name,"in",nme,":",cond))
			})
	names(output) <- data_names
	return(output)
}

results_day_analysis_module_builder <- new("AnalysisModuleFactory",name = "ResultsDayModule",ppmdl_class = "ResultsDayBatchGatherer",visualisations = rd_visualisations,panel_computation=results_day_panel_comp,computation_name=results_day_panel_comp_name)
