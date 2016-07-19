#Definition file for a report analysis module to 
#examine the relationship between position size and
#return and volatility of return.

sourceTo("../analysis_modules/visualisation_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_module.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/ppmodel_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/trade_feature_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/module_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

#Volatility and return (abs/rel) on positions by size decile overall and by strategy
#Return/volatility spread by size decile
#Vol/adjusted return spread by size decile 

psnsz_visualisations <- list()

	#1. Overall absolute return by position size quartile.

	#size_panel <- new('Panel')
	#size_panel@parameters <- 'mfrow=c(2,1),mar=c(6,4,2.5,2.5)'
	#size_panel@visualns <- list(new("FrequencyPlot",aggregate_what="PsnReturn",aggregate_by=list(c("Instrument","Strategy","PsnLong","DecileNumber","ppModelIndex"),c("PsnLong","DecileNumber","ppModelIndex")),subset_by=c("PsnLong","ppModelIndex"),subset_with=list(TRUE,ppmodel_subset),aggregate_fn=list(gm_mean_bps,mean),y_label="Av. Mnthly Return (bps)",x_label="Size Decile",title="Rtn by size (Long)",subset_fn=list(function(a,b)a==b,ppmodel_subset_fn),x_label_variable="DecileNumber",visuln_comp=compound_aggregate_and_subset_unique),
	#						    new("FrequencyPlot",aggregate_what="PsnReturn",aggregate_by=list(c("Instrument","Strategy","PsnLong","DecileNumber","ppModelIndex"),c("PsnLong","DecileNumber","ppModelIndex")),subset_by=c("PsnLong","ppModelIndex"),subset_with=list(FALSE,ppmodel_subset),aggregate_fn=list(gm_mean_bps,mean),y_label="Av. Mnthly Return (bps)",x_label="Size Decile",title="Rtn by size (Short)",subset_fn=list(function(a,b)a==b,ppmodel_subset_fn),x_label_variable="DecileNumber",visuln_comp=compound_aggregate_and_subset_unique))		
	#psnsz_visualisations[[length(psnsz_visualisations)+1]] <- size_panel

	#2. Overall relative return by position size quartile

	#size_panel <- new('Panel')
	#size_panel@parameters <- 'mfrow=c(2,1),mar=c(6,4,2.5,2.5)'
	#size_panel@visualns <- list(new("FrequencyPlot",aggregate_what="RelativeReturn",aggregate_by=list(c("Instrument","Strategy","PsnLong","DecileNumber","ppModelIndex"),c("PsnLong","DecileNumber","ppModelIndex")),subset_by=c("PsnLong","ppModelIndex"),subset_with=list(TRUE,ppmodel_subset),aggregate_fn=list(gm_mean_bps,mean),y_label="Av. Mnthly Rel. Return (bps)",x_label="Size Decile",title="Rtn by size (Long)",subset_fn=list(function(a,b)a==b,ppmodel_subset_fn),x_label_variable="DecileNumber",visuln_comp=compound_aggregate_and_subset_unique),
	#						    new("FrequencyPlot",aggregate_what="RelativeReturn",aggregate_by=list(c("Instrument","Strategy","PsnLong","DecileNumber","ppModelIndex"),c("PsnLong","DecileNumber","ppModelIndex")),subset_by=c("PsnLong","ppModelIndex"),subset_with=list(FALSE,ppmodel_subset),aggregate_fn=list(gm_mean_bps,mean),y_label="Av. Mnthly Rel. Return (bps)",x_label="Size Decile",title="Rtn by size (Short)",subset_fn=list(function(a,b)a==b,ppmodel_subset_fn),x_label_variable="DecileNumber",visuln_comp=compound_aggregate_and_subset_unique))		
	#psnsz_visualisations[[length(psnsz_visualisations)+1]] <- size_panel

	#3.  Position volatility by size decile

	#size_panel <- new('Panel')
	#size_panel@parameters <- 'mfrow=c(2,1),mar=c(6,4,2.5,2.5)'
	#size_panel@visualns <- list(new("FrequencyPlot",aggregate_what="PsnRtnVol",aggregate_by=c("PsnLong","DecileNumber","ppModelIndex"),subset_by=c("PsnLong","ppModelIndex"),subset_with=list(TRUE,ppmodel_subset),aggregate_fn=function(x)sqrt(mean(x^2)),y_label="Av. Mnthly Vol. (bps)",x_label="Size Decile",title="Vol. by size (Long)",subset_fn=list(function(a,b)a==b,ppmodel_subset_fn),x_label_variable="DecileNumber"),
	#						    new("FrequencyPlot",aggregate_what="PsnRtnVol",aggregate_by=c("PsnLong","DecileNumber","ppModelIndex"),subset_by=c("PsnLong","ppModelIndex"),subset_with=list(FALSE,ppmodel_subset),aggregate_fn=function(x)sqrt(mean(x^2)),y_label="Av. Mnthly Vol. (bps)",x_label="Size Decile",title="Vol. by size (Short)",subset_fn=list(function(a,b)a==b,ppmodel_subset_fn),x_label_variable="DecileNumber"))		
	#psnsz_visualisations[[length(psnsz_visualisations)+1]] <- size_panel

	#4. Position count in each decile

	size_panel <- new('Panel')
	size_panel@parameters <- 'mfrow=c(2,1),mar=c(6,4,2.5,2.5)'
	size_panel@visualns <- list(new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("PsnLong","DecileNumber","ppModelIndex"),subset_by=c("PsnLong","ppModelIndex"),subset_with=list(TRUE,3),aggregate_fn=sum,y_label="Count",x_label="Size Decile",title="Number positions (Long)",x_label_variable="DecileNumber",psn_level=TRUE),
							    new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("PsnLong","DecileNumber","ppModelIndex"),subset_by=c("PsnLong","ppModelIndex"),subset_with=list(FALSE,3),aggregate_fn=sum,y_label="Count",x_label="Size Decile",title="Number positions (Short)",x_label_variable="DecileNumber",psn_level=TRUE))		
	psnsz_visualisations[[length(psnsz_visualisations)+1]] <- size_panel

	#5. Hit rate by size decile

	size_panel <- new('Panel')
	size_panel@parameters <- 'mfrow=c(2,1),mar=c(6,4,2.5,2.5)'
	size_panel@visualns <- list(new("FrequencyPlot",aggregate_what="PsnUp",aggregate_by=c("PsnLong","DecileNumber","ppModelIndex"),subset_by=c("PsnLong","ppModelIndex"),subset_with=list(TRUE,3),aggregate_fn=mean,y_label="Monthly hit rate",x_label="Size Decile",title="Position increase rate (Long)",x_label_variable="DecileNumber",psn_level=TRUE),
							    new("FrequencyPlot",aggregate_what="PsnUp",aggregate_by=c("PsnLong","DecileNumber","ppModelIndex"),subset_by=c("PsnLong","ppModelIndex"),subset_with=list(FALSE,3),aggregate_fn=mean,y_label="Monthly hit rate",x_label="Size Decile",title="Position increase rate (Short)",x_label_variable="DecileNumber",psn_level=TRUE))		
	psnsz_visualisations[[length(psnsz_visualisations)+1]] <- size_panel

	#6. Win/loss ratio for positions in each size decile

	size_panel <- new('Panel')
	size_panel@parameters <- 'mfrow=c(2,3),mar=c(6,4,2.5,2.5)'
	size_panel@visualns <- list(new("FrequencyPlot",aggregate_what="Total.PL",aggregate_by=c("Tag","PsnLong","DecileNumber","ppModelIndex"),subset_by=c("Tag","PsnLong","ppModelIndex"),subset_with=list(list('Hit',TRUE,3),list('Miss',TRUE,3)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Long",x_label_variable="DecileNumber",visuln_comp=data_aggregate_ratio_by_subset,psn_level=TRUE),
								new("FrequencyPlot",aggregate_what="Total.PL",aggregate_by=c("Tag","PsnLong","DecileNumber","ppModelIndex"),subset_by=c("Tag","PsnLong","ppModelIndex"),subset_with=list(list('Hit',FALSE,3),list('Miss',FALSE,3)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Short",x_label_variable="DecileNumber",visuln_comp=data_aggregate_ratio_by_subset,psn_level=TRUE))		
	psnsz_visualisations[[length(psnsz_visualisations)+1]] <- size_panel

	size_panel <- new('Panel')
	size_panel@parameters <- 'mfrow=c(2,1),mar=c(6,4,2.5,2.5)'
	size_panel@visualns <- list(new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("PsnLong","DecileNumber"),subset_by=c("PsnLong"),subset_with=list(TRUE),aggregate_fn=sum,y_label="Count",x_label="Size Decile",title="Number positions (Long)",x_label_variable="DecileNumber",psn_level=TRUE),
							    new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("PsnLong","DecileNumber","ppModelIndex"),subset_by=c("PsnLong"),subset_with=list(FALSE),aggregate_fn=sum,y_label="Count",x_label="Size Decile",title="Number positions (Short)",x_label_variable="DecileNumber",psn_level=TRUE))		
	psnsz_visualisations[[length(psnsz_visualisations)+1]] <- size_panel

	size_panel <- new('Panel')
	size_panel@parameters <- 'mfrow=c(2,1),mar=c(6,4,2.5,2.5)'
	size_panel@visualns <- list(new("FrequencyPlot",aggregate_what="PsnUp",aggregate_by=c("PsnLong","DecileNumber"),subset_by=c("PsnLong"),subset_with=list(TRUE),aggregate_fn=mean,y_label="Monthly hit rate",x_label="Size Decile",title="Position increase rate (Long)",x_label_variable="DecileNumber",psn_level=TRUE),
							    new("FrequencyPlot",aggregate_what="PsnUp",aggregate_by=c("PsnLong","DecileNumber"),subset_by=c("PsnLong"),subset_with=list(FALSE),aggregate_fn=mean,y_label="Monthly hit rate",x_label="Size Decile",title="Position increase rate (Short)",x_label_variable="DecileNumber",psn_level=TRUE))		
	psnsz_visualisations[[length(psnsz_visualisations)+1]] <- size_panel

	size_panel <- new('Panel')
	size_panel@parameters <- 'mfrow=c(2,3),mar=c(6,4,2.5,2.5)'
	size_panel@visualns <- list(new("FrequencyPlot",aggregate_what="Total.PL",aggregate_by=c("Tag","PsnLong","DecileNumber"),subset_by=c("Tag","PsnLong"),subset_with=list(list('Hit',TRUE),list('Miss',TRUE)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Long",x_label_variable="DecileNumber",visuln_comp=data_aggregate_ratio_by_subset,psn_level=TRUE),
								new("FrequencyPlot",aggregate_what="Total.PL",aggregate_by=c("Tag","PsnLong","DecileNumber"),subset_by=c("Tag","PsnLong"),subset_with=list(list('Hit',FALSE),list('Miss',FALSE)),aggregate_fn=function(x)mean(abs(x)),y_label="Win/Loss ratio",title="Short",x_label_variable="DecileNumber",visuln_comp=data_aggregate_ratio_by_subset,psn_level=TRUE))		
	psnsz_visualisations[[length(psnsz_visualisations)+1]] <- size_panel

#Tabulate data:

sizing_panel_comp_name <- "Position size hit analysis."

sizing_panel_comp <- function(module_data_list,name){
	output <- list()
	data_names <- c()
	nme <- "Position hit rate by size."
	column_names <- c('STRING|Side','STRING|Context','INT|Psns 1m','QUANTITY_1DP|Psns 3m','PERCENT_1DP|Up 1m','PERCENT_1DP|Av.Up (3m)','QUANTITY_1DP|Win loss 1m','QUANTITY_1DP|Win loss 3m')
	tryCatch({
			output[[length(output)+1]] <- data.frame(Side='Long',
				                                     Description='Large positions',
				                                     Positions1M = element_picker(module_data_list,1,1,ncol(module_data_list[[1]][[1]])),
				                                     Positions3M = element_picker(module_data_list,4,1,ncol(module_data_list[[4]][[1]]))/3,
				                                     Hits1M= element_picker(module_data_list,2,1,ncol(module_data_list[[2]][[1]])),
				                                     Hits3M=element_picker(module_data_list,5,1,ncol(module_data_list[[5]][[1]])),
									                 Winloss1m=element_picker(module_data_list,3,1,ncol(module_data_list[[3]][[1]])),
									                 Winloss3m=element_picker(module_data_list,6,1,ncol(module_data_list[[6]][[1]])))
			output[[length(output)]] <- rbind(output[[length(output)]],data.frame(Side='Long',
				                                     Description='Overall',
				                                     Positions1M = element_picker(module_data_list,1,1,NULL),
				                                     Positions3M = element_picker(module_data_list,4,1,NULL)/3,
				                                     Hits1M= element_picker(module_data_list,2,1,NULL,fn=mean),
				                                     Hits3M=element_picker(module_data_list,5,1,NULL,fn=mean),
									                 Winloss1m=element_picker(module_data_list,3,1,NULL,fn=mean),
									                 Winloss3m=element_picker(module_data_list,6,1,NULL,fn=mean)))
			output[[length(output)]] <- rbind(output[[length(output)]],data.frame(Side='Short',
				                                     Description='Large positions',
				                                     Positions1M = element_picker(module_data_list,1,2,ncol(module_data_list[[1]][[2]])),
				                                     Positions3M = element_picker(module_data_list,4,2,ncol(module_data_list[[4]][[2]]))/3,
				                                     Hits1M= element_picker(module_data_list,2,2,ncol(module_data_list[[2]][[2]])),
				                                     Hits3M=element_picker(module_data_list,5,2,ncol(module_data_list[[5]][[2]])),
									                 Winloss1m=element_picker(module_data_list,3,2,ncol(module_data_list[[3]][[2]])),
									                 Winloss3m=element_picker(module_data_list,6,2,ncol(module_data_list[[6]][[2]]))))
			output[[length(output)]] <- rbind(output[[length(output)]],data.frame(Side='Short',
				                                     Description='Overall',
				                                     Positions1M = element_picker(module_data_list,1,2,NULL),
				                                     Positions3M = element_picker(module_data_list,4,2,NULL)/3,
				                                     Hits1M= element_picker(module_data_list,2,2,NULL,fn=mean),
				                                     Hits3M=element_picker(module_data_list,5,2,NULL,fn=mean),
									                 Winloss1m=element_picker(module_data_list,3,2,NULL,fn=mean),
									                 Winloss3m=element_picker(module_data_list,6,2,NULL,fn=mean)))
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

sizing_analysis_module_builder <- new("AnalysisModuleFactory",name ="SizingModule",ppmdl_class = "PositionSizeBatchQuantiler",visualisations = psnsz_visualisations,panel_computation=sizing_panel_comp,computation_name=sizing_panel_comp_name)