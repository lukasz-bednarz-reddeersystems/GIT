sourceTo("../analysis_modules/visualisation_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_module.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/ppmodel_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/trade_feature_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/module_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

# Assumes that module is being fed by a pre-processor module that produces the stats for a 3 month slice sampled monthly.
# The succesive months should be indexed by the ppmodel index with 3, refering to the most recent month.

pet_names_visualistaions <- list()

for(ppmodel_subset in c(3,1)){

	#1. Counts of pet/non-pet psns for long/short onside/offside 

	results_day_panel <- new('Panel')
	results_day_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
	results_day_panel@visualns <- list(new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Pet","PsnLong","PsnOffside","ppModelIndex"),subset_by=c("PsnLong","PsnOffside","ppModelIndex"),subset_with=list(TRUE,TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Long offside',subset_fn=list(function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Pet",psn_level=TRUE),
									   new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Pet","PsnLong","PsnOffside","ppModelIndex"),subset_by=c("PsnLong","PsnOffside","ppModelIndex"),subset_with=list(FALSE,TRUE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Short offside',subset_fn=list(function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Pet",psn_level=TRUE),
									   new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Pet","PsnLong","PsnOffside","ppModelIndex"),subset_by=c("PsnLong","PsnOffside","ppModelIndex"),subset_with=list(TRUE,FALSE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Long onside',subset_fn=list(function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Pet",psn_level=TRUE),
									   new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("Pet","PsnLong","PsnOffside","ppModelIndex"),subset_by=c("PsnLong","PsnOffside","ppModelIndex"),subset_with=list(FALSE,FALSE,ppmodel_subset),aggregate_fn=sum,y_label="Count",title='Short onside',subset_fn=list(function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Pet",psn_level=TRUE))
	pet_names_visualistaions[[length(pet_names_visualistaions)+1]] <- results_day_panel

	#2. Average NLegs and Leg span for pet/non-pet and long/short offside/onside

	results_day_panel@parameters <- 'mfrow=c(3,3),mar=c(6,4,2.5,2.5)'
	results_day_panel@visualns <- list(new("FrequencyPlot",aggregate_what="NLegs",aggregate_by=c("Pet","PsnLong","PsnOffside","ppModelIndex"),subset_by=c("PsnLong","PsnOffside","ppModelIndex"),subset_with=list(TRUE,TRUE,ppmodel_subset),aggregate_fn=mean,y_label="Average NLegs",title='Long offside',subset_fn=list(function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Pet",psn_level=TRUE),
									   new("FrequencyPlot",aggregate_what="NLegs",aggregate_by=c("Pet","PsnLong","PsnOffside","ppModelIndex"),subset_by=c("PsnLong","PsnOffside","ppModelIndex"),subset_with=list(FALSE,TRUE,ppmodel_subset),aggregate_fn=mean,y_label="Average NLegs",title='Short offside',subset_fn=list(function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Pet",psn_level=TRUE),
									   new("FrequencyPlot",aggregate_what="NLegs",aggregate_by=c("Pet","PsnLong","PsnOffside","ppModelIndex"),subset_by=c("PsnLong","PsnOffside","ppModelIndex"),subset_with=list(TRUE,FALSE,ppmodel_subset),aggregate_fn=mean,y_label="Average NLegs",title='Long onside',subset_fn=list(function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Pet",psn_level=TRUE),
									   new("FrequencyPlot",aggregate_what="NLegs",aggregate_by=c("Pet","PsnLong","PsnOffside","ppModelIndex"),subset_by=c("PsnLong","PsnOffside","ppModelIndex"),subset_with=list(FALSE,FALSE,ppmodel_subset),aggregate_fn=mean,y_label="Average NLegs",title='Short onside',subset_fn=list(function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Pet",psn_level=TRUE),
									   new("FrequencyPlot",aggregate_what="LegSpan",aggregate_by=c("Pet","PsnLong","PsnOffside","ppModelIndex"),subset_by=c("PsnLong","PsnOffside","ppModelIndex"),subset_with=list(TRUE,TRUE,ppmodel_subset),aggregate_fn=mean,y_label="Average LegSpan",title='Long offside',subset_fn=list(function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Pet",psn_level=TRUE),
									   new("FrequencyPlot",aggregate_what="LegSpan",aggregate_by=c("Pet","PsnLong","PsnOffside","ppModelIndex"),subset_by=c("PsnLong","PsnOffside","ppModelIndex"),subset_with=list(FALSE,TRUE,ppmodel_subset),aggregate_fn=mean,y_label="Average LegSpan",title='Short offside',subset_fn=list(function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Pet",psn_level=TRUE),
									   new("FrequencyPlot",aggregate_what="LegSpan",aggregate_by=c("Pet","PsnLong","PsnOffside","ppModelIndex"),subset_by=c("PsnLong","PsnOffside","ppModelIndex"),subset_with=list(TRUE,FALSE,ppmodel_subset),aggregate_fn=mean,y_label="Average LegSpan",title='Long onside',subset_fn=list(function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Pet",psn_level=TRUE),
									   new("FrequencyPlot",aggregate_what="LegSpan",aggregate_by=c("Pet","PsnLong","PsnOffside","ppModelIndex"),subset_by=c("PsnLong","PsnOffside","ppModelIndex"),subset_with=list(FALSE,FALSE,ppmodel_subset),aggregate_fn=mean,y_label="Average LegSpan",title='Short onside',subset_fn=list(function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Pet",psn_level=TRUE))
	pet_names_visualistaions[[length(pet_names_visualistaions)+1]] <- results_day_panel

	#3. Psn hit analysis for long/short, pet/non-pet, offside/onside

	results_day_panel <- new('Panel')
	results_day_panel@parameters <- 'mfrow=c(2,3),mar=c(6,4,2.5,2.5)'
	results_day_panel@visualns <- list(new("FrequencyPlot",aggregate_what="PsnUp",aggregate_by=c("Pet","PsnLong","PsnOffside","ppModelIndex"),subset_by=c("PsnLong","PsnOffside","ppModelIndex"),subset_with=list(TRUE,TRUE,ppmodel_subset),aggregate_fn=mean,y_label="Hit rate",title='Long offside',subset_fn=list(function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Pet",psn_level=TRUE),
									   new("FrequencyPlot",aggregate_what="PsnUp",aggregate_by=c("Pet","PsnLong","PsnOffside","ppModelIndex"),subset_by=c("PsnLong","PsnOffside","ppModelIndex"),subset_with=list(FALSE,TRUE,ppmodel_subset),aggregate_fn=mean,y_label="Hit rate",title='Short offside',subset_fn=list(function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Pet",psn_level=TRUE),
									   new("FrequencyPlot",aggregate_what="PsnUp",aggregate_by=c("Pet","PsnLong","PsnOffside","ppModelIndex"),subset_by=c("PsnLong","PsnOffside","ppModelIndex"),subset_with=list(TRUE,FALSE,ppmodel_subset),aggregate_fn=mean,y_label="Hit rate",title='Long onside',subset_fn=list(function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Pet",psn_level=TRUE),
									   new("FrequencyPlot",aggregate_what="PsnUp",aggregate_by=c("Pet","PsnLong","PsnOffside","ppModelIndex"),subset_by=c("PsnLong","PsnOffside","ppModelIndex"),subset_with=list(FALSE,FALSE,ppmodel_subset),aggregate_fn=mean,y_label="Hit rate",title='Long onside',subset_fn=list(function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),x_label_variable="Pet",psn_level=TRUE),
									   new("FrequencyPlot",aggregate_what="PsnUp",aggregate_by=c("PsnLong","ppModelIndex"),subset_by=c("PsnLong","ppModelIndex"),subset_with=list(TRUE,ppmodel_subset),aggregate_fn=mean,y_label="Hit rate",title='Long overall',subset_fn=list(function(a,b)a==b,ppmodel_subset_fn),psn_level=TRUE),
									   new("FrequencyPlot",aggregate_what="PsnUp",aggregate_by=c("PsnLong","ppModelIndex"),subset_by=c("PsnLong","ppModelIndex"),subset_with=list(FALSE,ppmodel_subset),aggregate_fn=mean,y_label="Hit rate",title='Short overall',subset_fn=list(function(a,b)a==b,ppmodel_subset_fn),psn_level=TRUE))
	pet_names_visualistaions[[length(pet_names_visualistaions)+1]] <- results_day_panel

	#4. Psn win/loss analysis for long/short, pet/non-pet, offside/onside

	results_day_panel <- new('Panel')
	results_day_panel@parameters <- 'mfrow=c(2,3),mar=c(6,4,2.5,2.5)'
	results_day_panel@visualns <- list(new("FrequencyPlot",aggregate_what="Total.PL",aggregate_by=c("Pet","PsnUp","PsnLong","PsnOffside","ppModelIndex"),subset_by=c("PsnUp","PsnLong","PsnOffside","ppModelIndex"),subset_with=list(list(TRUE,TRUE,TRUE,ppmodel_subset),list(FALSE,TRUE,TRUE,ppmodel_subset)),aggregate_fn=function(x)abs(mean(x,na.rm=TRUE)),y_label="Win loss",title='Long offside',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset,x_label_variable="Pet",psn_level=TRUE),
									   new("FrequencyPlot",aggregate_what="Total.PL",aggregate_by=c("Pet","PsnUp","PsnLong","PsnOffside","ppModelIndex"),subset_by=c("PsnUp","PsnLong","PsnOffside","ppModelIndex"),subset_with=list(list(TRUE,FALSE,TRUE,ppmodel_subset),list(FALSE,FALSE,TRUE,ppmodel_subset)),aggregate_fn=function(x)abs(mean(x,na.rm=TRUE)),y_label="Win loss",title='Short offside',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset,x_label_variable="Pet",psn_level=TRUE),
									   new("FrequencyPlot",aggregate_what="Total.PL",aggregate_by=c("Pet","PsnUp","PsnLong","PsnOffside","ppModelIndex"),subset_by=c("PsnUp","PsnLong","PsnOffside","ppModelIndex"),subset_with=list(list(TRUE,TRUE,FALSE,ppmodel_subset),list(FALSE,TRUE,FALSE,ppmodel_subset)),aggregate_fn=function(x)abs(mean(x,na.rm=TRUE)),y_label="Win loss",title='Long onside',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset,x_label_variable="Pet",psn_level=TRUE),
									   new("FrequencyPlot",aggregate_what="Total.PL",aggregate_by=c("Pet","PsnUp","PsnLong","PsnOffside","ppModelIndex"),subset_by=c("PsnUp","PsnLong","PsnOffside","ppModelIndex"),subset_with=list(list(TRUE,FALSE,FALSE,ppmodel_subset),list(FALSE,FALSE,FALSE,ppmodel_subset)),aggregate_fn=function(x)abs(mean(x,na.rm=TRUE)),y_label="Win loss",title='Short onside',subset_fn=list(function(a,b)a==b,function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset,x_label_variable="Pet",psn_level=TRUE),
									   new("FrequencyPlot",aggregate_what="Total.PL",aggregate_by=c("PsnUp","PsnLong","ppModelIndex"),subset_by=c("PsnUp","PsnLong","ppModelIndex"),subset_with=list(list(TRUE,TRUE,ppmodel_subset),list(FALSE,TRUE,ppmodel_subset)),aggregate_fn=function(x)abs(mean(x,na.rm=TRUE)),y_label="Win loss",title='Long overall',subset_fn=list(function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset,psn_level=TRUE),
									   new("FrequencyPlot",aggregate_what="Total.PL",aggregate_by=c("PsnUp","PsnLong","ppModelIndex"),subset_by=c("PsnUp","PsnLong","ppModelIndex"),subset_with=list(list(TRUE,FALSE,ppmodel_subset),list(FALSE,FALSE,ppmodel_subset)),aggregate_fn=function(x)abs(mean(x,na.rm=TRUE)),y_label="Win loss",title='Short overall',subset_fn=list(function(a,b)a==b,function(a,b)a==b,ppmodel_subset_fn),visuln_comp=data_aggregate_ratio_by_subset,psn_level=TRUE))
	pet_names_visualistaions[[length(pet_names_visualistaions)+1]] <- results_day_panel

}

#Tabulate data:

pet_names_panel_comp_name <- "HitRateByCategory"

pet_names_panel_comp <- function(module_data_list,name){
	output <- list()
	data_names <- c()
	#1. Up rate for long/short position traded/untraded on day 0
	#	Uses panels 2 (increases) and 3 (decreases), ratios in element 5 (long) and 6 (short)
	nme <- "Position Up rate."
	column_names <- c('STRING|Side','STRING|Context','INT|Psns 1m','QUANTITY_1DP|Psns 3m','PERCENT_1DP|PsnsUp 1m','PERCENT_1DP|PsnsUp 3m','QUANTITY_1DP|Win loss 1m','QUANTITY_1DP|Win loss PL 3m')
	tryCatch({
			panel <- 1
			output[[length(output)+1]] <- data.frame(Side='Long',
				                                     Description='Overall',
				                                     Positions1M = element_picker(module_data_list,panel,1,'Pet')+element_picker(module_data_list,panel,1,'NotPet'),
				                                     Positions3M = (element_picker(module_data_list,panel+4,1,'Pet')+element_picker(module_data_list,panel+4,1,'NotPet'))/3,
				                                     Hits1M=element_picker(module_data_list,panel+2,5,NULL),
				                                     Hits3M=element_picker(module_data_list,panel+6,5,NULL)/3,
									                 Winloss1m=element_picker(module_data_list,panel+3,5,NULL),
									                 Winloss3m=element_picker(module_data_list,panel+7,5,NULL))
			output[[length(output)]] <- rbind(output[[length(output)]], data.frame(Side='Short',
				                                     Description='Overall',
				                                     Positions1M = element_picker(module_data_list,panel,2,'Pet')+element_picker(module_data_list,panel,2,'NotPet'),
				                                     Positions3M = (element_picker(module_data_list,panel+4,2,'Pet')+element_picker(module_data_list,panel+4,2,'NotPet'))/3,
				                                     Hits1M=element_picker(module_data_list,panel+2,6,NULL),
				                                     Hits3M=element_picker(module_data_list,panel+6,6,NULL)/3,
									                 Winloss1m=element_picker(module_data_list,panel+3,6,NULL),
									                 Winloss3m=element_picker(module_data_list,panel+7,6,NULL)))
			part  <- 1
			output[[length(output)]] <- rbind(output[[length(output)]], data.frame(Side='Long',
				                                     Description='Pet. Offs.',
				                                     Positions1M = element_picker(module_data_list,panel,part,'Pet'),
				                                     Positions3M = element_picker(module_data_list,panel,part,'Pet')/3,
				                                     Hits1M=element_picker(module_data_list,panel+2,part,'Pet'),
				                                     Hits3M=element_picker(module_data_list,panel+6,part,'Pet')/3,
									                 Winloss1m=element_picker(module_data_list,panel+3,part,'Pet'),
									                 Winloss3m=element_picker(module_data_list,panel+7,part,'Pet')))
			output[[length(output)]] <- rbind(output[[length(output)]], data.frame(Side='Long',
				                                     Description='NotPet. Offs.',
				                                     Positions1M = element_picker(module_data_list,panel,part,'NotPet'),
				                                     Positions3M = element_picker(module_data_list,panel,part,'NotPet')/3,
				                                     Hits1M=element_picker(module_data_list,panel+2,part,'NotPet'),
				                                     Hits3M=element_picker(module_data_list,panel+6,part,'NotPet')/3,
									                 Winloss1m=element_picker(module_data_list,panel+3,part,'NotPet'),
									                 Winloss3m=element_picker(module_data_list,panel+7,part,'NotPet')))
			part  <- 2
			output[[length(output)]] <- rbind(output[[length(output)]], data.frame(Side='Short',
				                                     Description='Pet. Offs.',
				                                     Positions1M = element_picker(module_data_list,panel,part,'Pet'),
				                                     Positions3M = element_picker(module_data_list,panel,part,'Pet')/3,
				                                     Hits1M=element_picker(module_data_list,panel+2,part,'Pet'),
				                                     Hits3M=element_picker(module_data_list,panel+6,part,'Pet')/3,
									                 Winloss1m=element_picker(module_data_list,panel+3,part,'Pet'),
									                 Winloss3m=element_picker(module_data_list,panel+7,part,'Pet')))
			output[[length(output)]] <- rbind(output[[length(output)]], data.frame(Side='Short',
				                                     Description='NotPet. Offs.',
				                                     Positions1M = element_picker(module_data_list,panel,part,'NotPet'),
				                                     Positions3M = element_picker(module_data_list,panel,part,'NotPet')/3,
				                                     Hits1M=element_picker(module_data_list,panel+2,part,'NotPet'),
				                                     Hits3M=element_picker(module_data_list,panel+6,part,'NotPet')/3,
									                 Winloss1m=element_picker(module_data_list,panel+3,part,'NotPet'),
									                 Winloss3m=element_picker(module_data_list,panel+7,part,'NotPet')))
			part  <- 3
			output[[length(output)]] <- rbind(output[[length(output)]], data.frame(Side='Long',
				                                     Description='Pet. Ons.',
				                                     Positions1M = element_picker(module_data_list,panel,part,'Pet'),
				                                     Positions3M = element_picker(module_data_list,panel,part,'Pet')/3,
				                                     Hits1M=element_picker(module_data_list,panel+2,part,'Pet'),
				                                     Hits3M=element_picker(module_data_list,panel+6,part,'Pet')/3,
									                 Winloss1m=element_picker(module_data_list,panel+3,part,'Pet'),
									                 Winloss3m=element_picker(module_data_list,panel+7,part,'Pet')))
			output[[length(output)]] <- rbind(output[[length(output)]], data.frame(Side='Long',
				                                     Description='NotPet. Ons.',
				                                     Positions1M = element_picker(module_data_list,panel,part,'NotPet'),
				                                     Positions3M = element_picker(module_data_list,panel,part,'NotPet')/3,
				                                     Hits1M=element_picker(module_data_list,panel+2,part,'NotPet'),
				                                     Hits3M=element_picker(module_data_list,panel+6,part,'NotPet')/3,
									                 Winloss1m=element_picker(module_data_list,panel+3,part,'NotPet'),
									                 Winloss3m=element_picker(module_data_list,panel+7,part,'NotPet')))
			part  <- 4
			output[[length(output)]] <- rbind(output[[length(output)]], data.frame(Side='Short',
				                                     Description='Pet. Ons.',
				                                     Positions1M = element_picker(module_data_list,panel,part,'Pet'),
				                                     Positions3M = element_picker(module_data_list,panel,part,'Pet')/3,
				                                     Hits1M=element_picker(module_data_list,panel+2,part,'Pet'),
				                                     Hits3M=element_picker(module_data_list,panel+6,part,'Pet')/3,
									                 Winloss1m=element_picker(module_data_list,panel+3,part,'Pet'),
									                 Winloss3m=element_picker(module_data_list,panel+7,part,'Pet')))
			output[[length(output)]] <- rbind(output[[length(output)]], data.frame(Side='Short',
				                                     Description='NotPet. Ons.',
				                                     Positions1M = element_picker(module_data_list,panel,part,'NotPet'),
				                                     Positions3M = element_picker(module_data_list,panel,part,'NotPet')/3,
				                                     Hits1M=element_picker(module_data_list,panel+2,part,'NotPet'),
				                                     Hits3M=element_picker(module_data_list,panel+6,part,'NotPet')/3,
									                 Winloss1m=element_picker(module_data_list,panel+3,part,'NotPet'),
									                 Winloss3m=element_picker(module_data_list,panel+7,part,'NotPet')))
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

pet_names_analysis_module_builder <- new("AnalysisModuleFactory",name = "PetNamesModule",ppmdl_class = "PetNamesPsnGatherer",visualisations = pet_names_visualistaions,panel_computation=pet_names_panel_comp,computation_name=pet_names_panel_comp_name)

