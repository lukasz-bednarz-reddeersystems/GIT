sourceTo("../analysis_modules_legacy/analysis_module.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../reporting/report_module_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

#contexts are the combinations of parameters that define each context to which
#the analysis computation should be applied. 
#for each context the hit rate and win loss ratio is computed.
frequencyplot_scorecard_panel_builder <- function(contexts,ppmodel_subsets=NULL,parameters='mfrow=c(2,1),mar=c(6,4,2.5,2.5)'){
	if(length(ppmodel_subsets)==0){
		ppmodel_subsets <- c(1)
		periodic <- FALSE
	}
	else{
		periodic <- TRUE
	}
	context_names <- names(contexts)
	panel_list <- list()
	for(context in context_names){
		vsn_list <- list()
		computations <- names(contexts[[context]])
		panel <- new('Panel')
		panel@parameters <- parameters
		for(computation in computations){
			for(ppmodel_subset in ppmodel_subsets){
				params <- contexts[[context]][[computation]]
				if(periodic){
					params[['aggregate_by']] <- c(params[['aggregate_by']],'ppModelIndex')
					params[['subset_by']]    <- c(params[['subset_by']],'ppModelIndex')
					params[['subset_with']]  <- subset_with_append(params[['subset_with']],ppmodel_subset)
					params[['subset_fn']]    <- c(params[['subset_fn']],ppmodel_subset_fn)
				}
				vsn <- new("FrequencyPlot",
							aggregate_what		=	params[['aggregate_what']],
							aggregate_by		=	params[['aggregate_by']],
							aggregate_fn		=	params[['aggregate_fn']],
							subset_by			=	params[['subset_by']],
							subset_with			=	params[['subset_with']],
							subset_fn 	        =	params[['subset_fn']],
							rotate_x_labels		=	value_or_default(params[['rotate_x_labels']],FALSE),
							x_label_variable	=	value_or_default(params[['x_label_variable']],character()),
							x_label 			=	value_or_default(params[['x_label']],character()),
							y_label			 	=	value_or_default(params[['y_label']],character()),
							title				=	value_or_default(params[['title']],character()),
							psn_level 			=	value_or_default(params[['psn_level']],FALSE),
							visuln_comp			=	value_or_default(params[['visuln_comp']],data_aggregate_and_subset))
				vsn_list[[length(vsn_list)+1]] <- vsn		
			}		
		}
		panel@visualns <- vsn_list
 		panel_list[[length(panel_list)+1]] <- panel
	}
	return(panel_list)
}

#state_context_map: for each configuration of the subset_by variables in the panel definition, 
#the state context map provides a context description (item name) with a list containing the location
#of the relevant data in the panel visualisations list. 
#the map is a list of lists for each context, where the list for each context contains a list locations of the data for each row of the scorecard, 
#where those location lists must be named by the score card column.
scorecard <- function(module_data_list,caller_name,name,column_names,state_context_map,up_tag,down_tag,item,winloss){
	output <- list()
	contexts <- names(state_context_map)
	for(c in 1:length(contexts)){
		location <- state_context_map[[contexts[c]]]
		tryCatch({
			if(c==1){
				output[[length(output)+1]] <- data.frame(description = contexts[c],
				                                     item1M = element_picker(module_data_list,location[[paste(item,'1M',sep="")]][[1]],location[[paste(item,'1M',sep="")]][[2]],up_tag)+element_picker(module_data_list,location[[paste(item,'1M',sep="")]][[1]],location[[paste(item,'1M',sep="")]][[2]],down_tag),
				                                     item3M = (element_picker(module_data_list,location[[paste(item,'3M',sep="")]][[1]],location[[paste(item,'3M',sep="")]][[2]],up_tag)+element_picker(module_data_list,location[[paste(item,'3M',sep="")]][[1]],location[[paste(item,'3M',sep="")]][[2]],down_tag))/3,
				                                     hits1M = element_picker(module_data_list,location[[paste(item,'1M',sep="")]][[1]],location[[paste(item,'1M',sep="")]][[2]],up_tag)/(element_picker(module_data_list,location[[paste(item,'1M',sep="")]][[1]],location[[paste(item,'1M',sep="")]][[2]],up_tag)+element_picker(module_data_list,location[[paste(item,'1M',sep="")]][[1]],location[[paste(item,'1M',sep="")]][[2]],down_tag)),
				                                     hits3M = element_picker(module_data_list,location[[paste(item,'3M',sep="")]][[1]],location[[paste(item,'3M',sep="")]][[2]],up_tag)/(element_picker(module_data_list,location[[paste(item,'3M',sep="")]][[1]],location[[paste(item,'3M',sep="")]][[2]],up_tag)+element_picker(module_data_list,location[[paste(item,'3M',sep="")]][[1]],location[[paste(item,'3M',sep="")]][[2]],down_tag)),
									                 winloss1m = element_picker(module_data_list,location[[paste(winloss,'1M',sep="")]][[1]],location[[paste(winloss,'1M',sep="")]][[2]],NULL,fn=mean),
									                 winloss3m = element_picker(module_data_list,location[[paste(winloss,'3M',sep="")]][[1]],location[[paste(winloss,'3M',sep="")]][[2]],NULL,fn=mean))	
			}
			else{
				output[[length(output)]] <- rbind(output[[length(output)]], data.frame(description = contexts[c],
				                                     item1M = element_picker(module_data_list,location[[paste(item,'1M',sep="")]][[1]],location[[paste(item,'1M',sep="")]][[2]],up_tag)+element_picker(module_data_list,location[[paste(item,'1M',sep="")]][[1]],location[[paste(item,'1M',sep="")]][[2]],down_tag),
				                                     item3M = (element_picker(module_data_list,location[[paste(item,'3M',sep="")]][[1]],location[[paste(item,'3M',sep="")]][[2]],up_tag)+element_picker(module_data_list,location[[paste(item,'3M',sep="")]][[1]],location[[paste(item,'3M',sep="")]][[2]],down_tag))/3,
				                                     hits1M = element_picker(module_data_list,location[[paste(item,'1M',sep="")]][[1]],location[[paste(item,'1M',sep="")]][[2]],up_tag)/(element_picker(module_data_list,location[[paste(item,'1M',sep="")]][[1]],location[[paste(item,'1M',sep="")]][[2]],up_tag)+element_picker(module_data_list,location[[paste(item,'1M',sep="")]][[1]],location[[paste(item,'1M',sep="")]][[2]],down_tag)),
				                                     hits3M = element_picker(module_data_list,location[[paste(item,'3M',sep="")]][[1]],location[[paste(item,'3M',sep="")]][[2]],up_tag)/(element_picker(module_data_list,location[[paste(item,'3M',sep="")]][[1]],location[[paste(item,'3M',sep="")]][[2]],up_tag)+element_picker(module_data_list,location[[paste(item,'3M',sep="")]][[1]],location[[paste(item,'3M',sep="")]][[2]],down_tag)),
									                 winloss1m = element_picker(module_data_list,location[[paste(winloss,'1M',sep="")]][[1]],location[[paste(winloss,'1M',sep="")]][[2]],NULL,fn=mean),
									                 winloss3m = element_picker(module_data_list,location[[paste(winloss,'3M',sep="")]][[1]],location[[paste(winloss,'3M',sep="")]][[2]],NULL,fn=mean)))	
			}
			
		}, error= function(cond){
			message(paste("From",caller_name))
			message(paste("Error applying panel computation",name,"in context",contexts[c],":",cond))
		})
	}
	tryCatch({
			colnames(output[[length(output)]]) <- column_names
			},error = function(cond){
				message("Failed to set panel computation frame column schema.")
			})
	names(output) <- name
	return(output)
}

position_level_scorecard <- function(module_data_list,caller_name,state_context_map){
	column_names <-  c('STRING|Context','INT|Psns 1m','QUANTITY_1DP|Psns 3m','PERCENT_1DP|Psns. up 1m','PERCENT_1DP|Psns. up 3m','QUANTITY_1DP|Win loss 1m','QUANTITY_1DP|Win loss 3m')
	name         <- "Position Score Card"
	return(scorecard(module_data_list,caller_name,name,column_names,state_context_map,'Up','Down','Positions','Winloss'))
}

trade_level_scorecard <- function(module_data_list,caller_name,state_context_map){
	column_names <-  c('STRING|Context','INT|Trades 1m','QUANTITY_1DP|Trades 3m','PERCENT_1DP|Hits 1m','PERCENT_1DP|Hits 3m','QUANTITY_1DP|Win loss 1m','QUANTITY_1DP|Win loss 3m')
	name         <- "Trade Score Card"
	return(scorecard(module_data_list,caller_name,name,column_names,state_context_map,'Hit','Miss','Trades','Winloss'))
}


snapshot_panel_builder <- function(contexts,ppmodel_subsets=NULL,parameters='mfrow=c(2,1),mar=c(6,4,2.5,2.5)',plot_path=NULL){
	if(length(ppmodel_subsets)==0){
		ppmodel_subsets <- c(1)
		periodic <- FALSE
	}
	else{
		periodic <- TRUE
	}
	context_names <- names(contexts)
	panel_list <- list()
	for(context in context_names){
		vsn_list <- list()
		computations <- names(contexts[[context]])
		panel <- new('Panel')
		panel@parameters <- parameters
		if(length(plot_path)>0){
			panel@plot_to_file <- TRUE
			panel@file_path    <- plot_path
			#panel@file   	   <- paste('SnapShot_',which(context_names==context),".jpg",sep="")
			panel@file   	   <- paste('SnapShot_',gsub(" ","_",context),".jpg",sep="")
		}
		for(computation in computations){
			for(ppmodel_subset in ppmodel_subsets){
				params <- contexts[[context]][[computation]]
				if(periodic){
					stop("Snapshots dont handle periodic data at present.")
				}
				vsn <- new("SnapShot",
							aggregate_by		=	params[['aggregate_by']],
							subset_by			=	params[['subset_by']],
							subset_with			=	params[['subset_with']],
							x_label 			=	value_or_default(params[['x_label']],character()),
							y_label			 	=	value_or_default(params[['y_label']],character()),
							title				=	value_or_default(params[['title']],character()))
				vsn_list[[length(vsn_list)+1]] <- vsn		
			}		
		}
		panel@visualns <- vsn_list
 		panel_list[[length(panel_list)+1]] <- panel
	}
	return(panel_list)
}