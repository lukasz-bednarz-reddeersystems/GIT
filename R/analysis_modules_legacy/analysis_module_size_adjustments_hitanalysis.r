#Definition file for a report analysis module to compute 
#the effectiveness of size changes to positions.

sourceTo("../analysis_modules_legacy/visualisation_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules_legacy/analysis_module.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/ppmodel_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/trade_feature_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules_legacy/module_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

sart_visualisations <- list()

#Over the month
#1. Number of trades increasing position for +ve/-ve PnL in 

size_adj_panel <- new('Panel')
size_adj_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
size_adj_panel@visualns <- list(new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_with=list(FALSE,TRUE,TRUE,TRUE,3),aggregate_fn=sum,y_label="Count",title="Long increases +ve Pnl"),
	                            new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_with=list(FALSE,FALSE,FALSE,TRUE,3),aggregate_fn=sum,y_label="Count",title="Short increases +ve Pnl"),
								new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_with=list(FALSE,TRUE,TRUE,FALSE,3),aggregate_fn=sum,y_label="Count",title="Long increases -ve Pnl"),
								new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_with=list(FALSE,FALSE,FALSE,FALSE,3),aggregate_fn=sum,y_label="Count",title="Short increases -ve Pnl"))		
sart_visualisations[[length(sart_visualisations)+1]] <- size_adj_panel

#2. Number of trades decreasing position for +ve/-ve PnL in 

size_adj_panel <- new('Panel')
size_adj_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
size_adj_panel@visualns <- list(new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_with=list(FALSE,TRUE,FALSE,TRUE,3),aggregate_fn=sum,y_label="Count",title="Long decreases +ve Pnl"),
								new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_with=list(FALSE,FALSE,TRUE,TRUE,3),aggregate_fn=sum,y_label="Count",title="Short decreases +ve Pnl"),
								new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_with=list(FALSE,TRUE,FALSE,FALSE,3),aggregate_fn=sum,y_label="Count",title="Long decreases -ve Pnl"),
								new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_with=list(FALSE,FALSE,TRUE,FALSE,3),aggregate_fn=sum,y_label="Count",title="Short decreases -ve Pnl"))		
sart_visualisations[[length(sart_visualisations)+1]] <- size_adj_panel

#3. Hit rate of trades increasing position for for +ve/-ve PnL in 

size_adj_panel <- new('Panel')
size_adj_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
size_adj_panel@visualns <- list(new("FrequencyPlot",aggregate_what="Hit1D",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_with=list(FALSE,TRUE,TRUE,TRUE,3),aggregate_fn=mean,y_label="Hit rate",title="Long increases +ve Pnl"),
	                            new("FrequencyPlot",aggregate_what="Hit1D",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_with=list(FALSE,FALSE,FALSE,TRUE,3),aggregate_fn=mean,y_label="Hit rate",title="Short increases +ve Pnl"),
								new("FrequencyPlot",aggregate_what="Hit1D",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_with=list(FALSE,TRUE,TRUE,FALSE,3),aggregate_fn=mean,y_label="Hit rate",title="Long increases -ve Pnl"),
								new("FrequencyPlot",aggregate_what="Hit1D",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_with=list(FALSE,FALSE,FALSE,FALSE,3),aggregate_fn=mean,y_label="Hit rate",title="Short increases -ve Pnl"))		
sart_visualisations[[length(sart_visualisations)+1]] <- size_adj_panel


#4. Hit rate of trades decreasing position for +ve/-ve PnL in 

size_adj_panel <- new('Panel')
size_adj_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
size_adj_panel@visualns <- list(new("FrequencyPlot",aggregate_what="Hit1D",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_with=list(FALSE,TRUE,FALSE,TRUE,3),aggregate_fn=mean,y_label="Hit rate",title="Long decreases +ve Pnl"),
								new("FrequencyPlot",aggregate_what="Hit1D",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_with=list(FALSE,FALSE,TRUE,TRUE,3),aggregate_fn=mean,y_label="Hit rate",title="Short decreases +ve Pnl"),
								new("FrequencyPlot",aggregate_what="Hit1D",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_with=list(FALSE,TRUE,FALSE,FALSE,3),aggregate_fn=mean,y_label="Hit rate",title="Long decreases -ve Pnl"),
								new("FrequencyPlot",aggregate_what="Hit1D",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","ppModelIndex"),subset_with=list(FALSE,FALSE,TRUE,FALSE,3),aggregate_fn=mean,y_label="Hit rate",title="Short decreases -ve Pnl"))		
sart_visualisations[[length(sart_visualisations)+1]] <- size_adj_panel

#5. Win/loss ratio for trades increasing position for +ve/-ve PnL in 

size_adj_panel <- new('Panel')
size_adj_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
size_adj_panel@visualns <- list(new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag","ppModelIndex"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag","ppModelIndex"),subset_with=list(list(FALSE,TRUE,TRUE,TRUE,"Hit",3),list(FALSE,TRUE,TRUE,TRUE,"Miss",3)),aggregate_fn=function(x)mean(abs(x)),y_label="Count",title="Long increases +ve Pnl",visuln_comp=data_aggregate_ratio_by_subset),
	                            new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag","ppModelIndex"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag","ppModelIndex"),subset_with=list(list(FALSE,FALSE,FALSE,TRUE,"Hit",3),list(FALSE,FALSE,FALSE,TRUE,"Miss",3)),aggregate_fn=function(x)mean(abs(x)),y_label="Count",title="Short increases +ve Pnl",visuln_comp=data_aggregate_ratio_by_subset),
								new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag","ppModelIndex"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag","ppModelIndex"),subset_with=list(list(FALSE,TRUE,TRUE,FALSE,"Hit",3),list(FALSE,TRUE,TRUE,FALSE,"Miss",3)),aggregate_fn=function(x)mean(abs(x)),y_label="Count",title="Long increases -ve Pnl",visuln_comp=data_aggregate_ratio_by_subset),
								new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag","ppModelIndex"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag","ppModelIndex"),subset_with=list(list(FALSE,FALSE,FALSE,FALSE,"Hit",3),list(FALSE,FALSE,FALSE,FALSE,"Miss",3)),aggregate_fn=function(x)mean(abs(x)),y_label="Count",title="Short increases -ve Pnl",visuln_comp=data_aggregate_ratio_by_subset))		
sart_visualisations[[length(sart_visualisations)+1]] <- size_adj_panel

#6. Win/loss ratio for trades decreasing position for +ve/-ve PnL in 

size_adj_panel <- new('Panel')
size_adj_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
size_adj_panel@visualns <- list(new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag","ppModelIndex"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag","ppModelIndex"),subset_with=list(list(FALSE,TRUE,FALSE,TRUE,"Hit",3),list(FALSE,TRUE,FALSE,TRUE,"Miss",3)),aggregate_fn=function(x)mean(abs(x)),y_label="Count",title="Long increases +ve Pnl",visuln_comp=data_aggregate_ratio_by_subset),
	                            new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag","ppModelIndex"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag","ppModelIndex"),subset_with=list(list(FALSE,FALSE,TRUE,TRUE,"Hit",3),list(FALSE,FALSE,TRUE,TRUE,"Miss",3)),aggregate_fn=function(x)mean(abs(x)),y_label="Count",title="Short increases +ve Pnl",visuln_comp=data_aggregate_ratio_by_subset),
								new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag","ppModelIndex"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag","ppModelIndex"),subset_with=list(list(FALSE,TRUE,FALSE,FALSE,"Hit",3),list(FALSE,TRUE,FALSE,FALSE,"Miss",3)),aggregate_fn=function(x)mean(abs(x)),y_label="Count",title="Long increases -ve Pnl",visuln_comp=data_aggregate_ratio_by_subset),
								new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag","ppModelIndex"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag","ppModelIndex"),subset_with=list(list(FALSE,FALSE,TRUE,FALSE,"Hit",3),list(FALSE,FALSE,TRUE,FALSE,"Miss",3)),aggregate_fn=function(x)mean(abs(x)),y_label="Count",title="Short increases -ve Pnl",visuln_comp=data_aggregate_ratio_by_subset))		
sart_visualisations[[length(sart_visualisations)+1]] <- size_adj_panel

#Overall:
#7. Number of trades increasing position for +ve/-ve PnL in 

size_adj_panel <- new('Panel')
size_adj_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
size_adj_panel@visualns <- list(new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,TRUE,TRUE,TRUE),aggregate_fn=sum,y_label="Count",title="Long increases +ve Pnl"),
	                            new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,FALSE,FALSE,TRUE),aggregate_fn=sum,y_label="Count",title="Short increases +ve Pnl"),
								new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,TRUE,TRUE,FALSE),aggregate_fn=sum,y_label="Count",title="Long increases -ve Pnl"),
								new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,FALSE,FALSE,FALSE),aggregate_fn=sum,y_label="Count",title="Short increases -ve Pnl"))		
sart_visualisations[[length(sart_visualisations)+1]] <- size_adj_panel

#8. Number of trades decreasing position for +ve/-ve PnL in 

size_adj_panel <- new('Panel')
size_adj_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
size_adj_panel@visualns <- list(new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,TRUE,FALSE,TRUE),aggregate_fn=sum,y_label="Count",title="Long decreases +ve Pnl"),
								new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,FALSE,TRUE,TRUE),aggregate_fn=sum,y_label="Count",title="Short decreases +ve Pnl"),
								new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,TRUE,FALSE,FALSE),aggregate_fn=sum,y_label="Count",title="Long decreases -ve Pnl"),
								new("FrequencyPlot",aggregate_what="Indicator",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,FALSE,TRUE,FALSE),aggregate_fn=sum,y_label="Count",title="Short decreases -ve Pnl"))		
sart_visualisations[[length(sart_visualisations)+1]] <- size_adj_panel

#9. Hit rate of trades increasing position for for +ve/-ve PnL in 

size_adj_panel <- new('Panel')
size_adj_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
size_adj_panel@visualns <- list(new("FrequencyPlot",aggregate_what="Hit1D",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,TRUE,TRUE,TRUE),aggregate_fn=mean,y_label="Hit rate",title="Long increases +ve Pnl"),
	                            new("FrequencyPlot",aggregate_what="Hit1D",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,FALSE,FALSE,TRUE),aggregate_fn=mean,y_label="Hit rate",title="Short increases +ve Pnl"),
								new("FrequencyPlot",aggregate_what="Hit1D",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,TRUE,TRUE,FALSE),aggregate_fn=mean,y_label="Hit rate",title="Long increases -ve Pnl"),
								new("FrequencyPlot",aggregate_what="Hit1D",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,FALSE,FALSE,FALSE),aggregate_fn=mean,y_label="Hit rate",title="Short increases -ve Pnl"))		
sart_visualisations[[length(sart_visualisations)+1]] <- size_adj_panel


#10. Hit rate of trades decreasing position for +ve/-ve PnL in 

size_adj_panel <- new('Panel')
size_adj_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
size_adj_panel@visualns <- list(new("FrequencyPlot",aggregate_what="Hit1D",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,TRUE,FALSE,TRUE),aggregate_fn=mean,y_label="Hit rate",title="Long decreases +ve Pnl"),
								new("FrequencyPlot",aggregate_what="Hit1D",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,FALSE,TRUE,TRUE),aggregate_fn=mean,y_label="Hit rate",title="Short decreases +ve Pnl"),
								new("FrequencyPlot",aggregate_what="Hit1D",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,TRUE,FALSE,FALSE),aggregate_fn=mean,y_label="Hit rate",title="Long decreases -ve Pnl"),
								new("FrequencyPlot",aggregate_what="Hit1D",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,FALSE,TRUE,FALSE),aggregate_fn=mean,y_label="Hit rate",title="Short decreases -ve Pnl"))		
sart_visualisations[[length(sart_visualisations)+1]] <- size_adj_panel

#11. Win/loss ratio for trades increasing position for +ve/-ve PnL in 

size_adj_panel <- new('Panel')
size_adj_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
size_adj_panel@visualns <- list(new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag"),subset_with=list(list(FALSE,TRUE,TRUE,TRUE,"Hit"),list(FALSE,TRUE,TRUE,TRUE,"Miss")),aggregate_fn=function(x)mean(abs(x)),y_label="Count",title="Long increases +ve Pnl",visuln_comp=data_aggregate_ratio_by_subset),
	                            new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag"),subset_with=list(list(FALSE,FALSE,FALSE,TRUE,"Hit"),list(FALSE,FALSE,FALSE,TRUE,"Miss")),aggregate_fn=function(x)mean(abs(x)),y_label="Count",title="Short increases +ve Pnl",visuln_comp=data_aggregate_ratio_by_subset),
								new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag"),subset_with=list(list(FALSE,TRUE,TRUE,FALSE,"Hit"),list(FALSE,TRUE,TRUE,FALSE,"Miss")),aggregate_fn=function(x)mean(abs(x)),y_label="Count",title="Long increases -ve Pnl",visuln_comp=data_aggregate_ratio_by_subset),
								new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag"),subset_with=list(list(FALSE,FALSE,FALSE,FALSE,"Hit"),list(FALSE,FALSE,FALSE,FALSE,"Miss")),aggregate_fn=function(x)mean(abs(x)),y_label="Count",title="Short increases -ve Pnl",visuln_comp=data_aggregate_ratio_by_subset))		
sart_visualisations[[length(sart_visualisations)+1]] <- size_adj_panel

#12. Win/loss ratio for trades decreasing position for +ve/-ve PnL in 

size_adj_panel <- new('Panel')
size_adj_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
size_adj_panel@visualns <- list(new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag"),subset_with=list(list(FALSE,TRUE,FALSE,TRUE,"Hit"),list(FALSE,TRUE,FALSE,TRUE,"Miss")),aggregate_fn=function(x)mean(abs(x)),y_label="Count",title="Long increases +ve Pnl",visuln_comp=data_aggregate_ratio_by_subset),
	                            new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag"),subset_with=list(list(FALSE,FALSE,TRUE,TRUE,"Hit"),list(FALSE,FALSE,TRUE,TRUE,"Miss")),aggregate_fn=function(x)mean(abs(x)),y_label="Count",title="Short increases +ve Pnl",visuln_comp=data_aggregate_ratio_by_subset),
								new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag"),subset_with=list(list(FALSE,TRUE,FALSE,FALSE,"Hit"),list(FALSE,TRUE,FALSE,FALSE,"Miss")),aggregate_fn=function(x)mean(abs(x)),y_label="Count",title="Long increases -ve Pnl",visuln_comp=data_aggregate_ratio_by_subset),
								new("FrequencyPlot",aggregate_what="TodayPL",aggregate_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto","Tag"),subset_with=list(list(FALSE,FALSE,TRUE,FALSE,"Hit"),list(FALSE,FALSE,TRUE,FALSE,"Miss")),aggregate_fn=function(x)mean(abs(x)),y_label="Count",title="Short increases -ve Pnl",visuln_comp=data_aggregate_ratio_by_subset))		
sart_visualisations[[length(sart_visualisations)+1]] <- size_adj_panel

sr_panel_comp_name <- "Position size adjustment hit analysis."
sr_panel_comp <- function(module_data_list,name){
	output <- list()
	data_names <- c()
	nme <- "Position hit rate by size."
	column_names <- c('STRING|Side','STRING|Context','INT|Trades 1m','QUANTITY_1DP|Trades 3m','PERCENT_1DP|Hit rate 1m','PERCENT_1DP|Hit rate 3m','QUANTITY_1DP|Win loss 1m','QUANTITY_1DP|Win loss 3m')
	tryCatch({
			panel <- 1
			description <- 1 
			output[[length(output)+1]] <- data.frame(Side='Long',
				                                     Description='Increase +ve PL',
				                                     Positions1M = element_picker(module_data_list,panel,description,1),
				                                     Positions3M = element_picker(module_data_list,panel+6,description,1)/3,
				                                     Hits1M= element_picker(module_data_list,panel+2,description,1),
				                                     Hits3M=element_picker(module_data_list,panel+8,description,1),
									                 Winloss1m=element_picker(module_data_list,panel+4,description,1),
									                 Winloss3m=element_picker(module_data_list,panel+10,description,1))
			description <- 2
			output[[length(output)]] <- rbind(output[[length(output)]],data.frame(Side='Short',
				                                     Description='Increase +ve PL',
				                                     Positions1M = element_picker(module_data_list,panel,description,1),
				                                     Positions3M = element_picker(module_data_list,panel+6,description,1)/3,
				                                     Hits1M= element_picker(module_data_list,panel+2,description,1),
				                                     Hits3M=element_picker(module_data_list,panel+8,description,1),
									                 Winloss1m=element_picker(module_data_list,panel+4,description,1),
									                 Winloss3m=element_picker(module_data_list,panel+10,description,1)))
			description <- 3
			output[[length(output)]] <- rbind(output[[length(output)]],data.frame(Side='Long',
				                                     Description='Increase -ve PL',
				                                     Positions1M = element_picker(module_data_list,panel,description,1),
				                                     Positions3M = element_picker(module_data_list,panel+6,description,1)/3,
				                                     Hits1M= element_picker(module_data_list,panel+2,description,1),
				                                     Hits3M=element_picker(module_data_list,panel+8,description,1),
									                 Winloss1m=element_picker(module_data_list,panel+4,description,1),
									                 Winloss3m=element_picker(module_data_list,panel+10,description,1)))
			description <- 4
			output[[length(output)]] <- rbind(output[[length(output)]],data.frame(Side='Short',
				                                     Description='Increase -ve PL',
				                                     Positions1M = element_picker(module_data_list,panel,description,1),
				                                     Positions3M = element_picker(module_data_list,panel+6,description,1)/3,
				                                     Hits1M= element_picker(module_data_list,panel+2,description,1),
				                                     Hits3M=element_picker(module_data_list,panel+8,description,1),
									                 Winloss1m=element_picker(module_data_list,panel+4,description,1),
									                 Winloss3m=element_picker(module_data_list,panel+10,description,1)))
			panel <- 2
			description <- 1
			output[[length(output)]] <- rbind(output[[length(output)]],data.frame(Side='Long',
				                                     Description='Decrease +ve PL',
				                                     Positions1M = element_picker(module_data_list,panel,description,1),
				                                     Positions3M = element_picker(module_data_list,panel+6,description,1)/3,
				                                     Hits1M= element_picker(module_data_list,panel+2,description,1),
				                                     Hits3M=element_picker(module_data_list,panel+8,description,1),
									                 Winloss1m=element_picker(module_data_list,panel+4,description,1),
									                 Winloss3m=element_picker(module_data_list,panel+10,description,1)))
			description <- 2
			output[[length(output)]] <- rbind(output[[length(output)]],data.frame(Side='Short',
				                                     Description='Decrease +ve PL',
				                                     Positions1M = element_picker(module_data_list,panel,description,1),
				                                     Positions3M = element_picker(module_data_list,panel+6,description,1)/3,
				                                     Hits1M= element_picker(module_data_list,panel+2,description,1),
				                                     Hits3M=element_picker(module_data_list,panel+8,description,1),
									                 Winloss1m=element_picker(module_data_list,panel+4,description,1),
									                 Winloss3m=element_picker(module_data_list,panel+10,description,1)))
			description <- 3
			output[[length(output)]] <- rbind(output[[length(output)]],data.frame(Side='Long',
				                                     Description='Decrease -ve PL',
				                                     Positions1M = element_picker(module_data_list,panel,description,1),
				                                     Positions3M = element_picker(module_data_list,panel+6,description,1)/3,
				                                     Hits1M= element_picker(module_data_list,panel+2,description,1),
				                                     Hits3M=element_picker(module_data_list,panel+8,description,1),
									                 Winloss1m=element_picker(module_data_list,panel+4,description,1),
									                 Winloss3m=element_picker(module_data_list,panel+10,description,1)))
			description <- 4
			output[[length(output)]] <- rbind(output[[length(output)]],data.frame(Side='Short',
				                                     Description='Decrease -ve PL',
				                                     Positions1M = element_picker(module_data_list,panel,description,1),
				                                     Positions3M = element_picker(module_data_list,panel+6,description,1)/3,
				                                     Hits1M= element_picker(module_data_list,panel+2,description,1),
				                                     Hits3M=element_picker(module_data_list,panel+8,description,1),
									                 Winloss1m=element_picker(module_data_list,panel+4,description,1),
									                 Winloss3m=element_picker(module_data_list,panel+10,description,1)))

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

size_adjustment_ratio_analysis_module_builder <- new("AnalysisModuleFactory",name = "SizeAdjustmentModule",ppmdl_class = "SizeAdjustmentBatchGatherer",visualisations = sart_visualisations, panel_computation = sr_panel_comp, computation_name= sr_panel_comp_name)