#Definition file for a report analysis module to compute 
#the effectiveness of size changes to positions.

sourceTo("../analysis_modules_legacy/visualisation_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules_legacy/analysis_module.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/ppmodel_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/trade_feature_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

#1.Compute the PnL ratio into the trade and out of the trade for
#+/-ve Pnl in to the trade and position increases/decreases.

#2.Compute the PnL ratio in a passive position in the underlying
#stock and subtract to determine value added by active position change
#around the trade. 

#1. PnL Into/Outof trade for each strategy for +/-ve Pnl, and 
#	increase/decrease
#	i. Long position increases +/- PnL in, short position increases +/- PnL in.
#	ii.Long position decreases +/- PnL in, short position decreases +/- PnL in.

size_adj_panel <- new('Panel')
size_adj_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
size_adj_panel@visualns <- list(new("FrequencyPlot",aggregate_what="AbsPnlRatio",aggregate_by=c("Strategy","NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,TRUE,TRUE,TRUE),aggregate_fn=mean,y_label="Abs PnL Ratio",x_label="Strategy",title="Long increases +ve Pnl",x_label_variable="Strategy"),
								new("FrequencyPlot",aggregate_what="AbsPnlRatio",aggregate_by=c("Strategy","NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,TRUE,TRUE,FALSE),aggregate_fn=mean,y_label="Abs PnL Ratio",x_label="Strategy",title="Long increases -ve Pnl",x_label_variable="Strategy"),
								new("FrequencyPlot",aggregate_what="AbsPnlRatio",aggregate_by=c("Strategy","NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,FALSE,FALSE,TRUE),aggregate_fn=mean,y_label="Abs PnL Ratio",x_label="Strategy",title="Short increases +ve Pnl",x_label_variable="Strategy"),
								new("FrequencyPlot",aggregate_what="AbsPnlRatio",aggregate_by=c("Strategy","NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,FALSE,FALSE,FALSE),aggregate_fn=mean,y_label="Abs PnL Ratio",x_label="Strategy",title="Short increases -ve Pnl",x_label_variable="Strategy"))		
sa_visualisations <- list(size_adj_panel)

size_adj_panel <- new('Panel')
size_adj_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
size_adj_panel@visualns <- list(new("FrequencyPlot",aggregate_what="AbsPnlRatio",aggregate_by=c("Strategy","NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,TRUE,FALSE,TRUE),aggregate_fn=mean,y_label="Abs PnL Ratio",x_label="Strategy",title="Long decreases +ve Pnl",x_label_variable="Strategy"),
								new("FrequencyPlot",aggregate_what="AbsPnlRatio",aggregate_by=c("Strategy","NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,TRUE,FALSE,FALSE),aggregate_fn=mean,y_label="Abs PnL Ratio",x_label="Strategy",title="Long decreases -ve Pnl",x_label_variable="Strategy"),
								new("FrequencyPlot",aggregate_what="AbsPnlRatio",aggregate_by=c("Strategy","NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,FALSE,TRUE,TRUE),aggregate_fn=mean,y_label="Abs PnL Ratio",x_label="Strategy",title="Short decreases +ve Pnl",x_label_variable="Strategy"),
								new("FrequencyPlot",aggregate_what="AbsPnlRatio",aggregate_by=c("Strategy","NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,FALSE,TRUE,FALSE),aggregate_fn=mean,y_label="Abs PnL Ratio",x_label="Strategy",title="Short decreases -ve Pnl",x_label_variable="Strategy"))		
sa_visualisations[[2]] <- size_adj_panel

#2. As 1, but for relative PnL ratio

size_adj_panel <- new('Panel')
size_adj_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
size_adj_panel@visualns <- list(new("FrequencyPlot",aggregate_what="RelPnlRatio",aggregate_by=c("Strategy","NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,TRUE,TRUE,TRUE),aggregate_fn=mean,y_label="Rel PnL Ratio",x_label="Strategy",title="Long increases +ve Pnl",x_label_variable="Strategy"),
								new("FrequencyPlot",aggregate_what="RelPnlRatio",aggregate_by=c("Strategy","NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,TRUE,TRUE,FALSE),aggregate_fn=mean,y_label="Rel PnL Ratio",x_label="Strategy",title="Long increases -ve Pnl",x_label_variable="Strategy"),
								new("FrequencyPlot",aggregate_what="RelPnlRatio",aggregate_by=c("Strategy","NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,FALSE,FALSE,TRUE),aggregate_fn=mean,y_label="Rel PnL Ratio",x_label="Strategy",title="Short increases +ve Pnl",x_label_variable="Strategy"),
								new("FrequencyPlot",aggregate_what="RelPnlRatio",aggregate_by=c("Strategy","NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,FALSE,FALSE,FALSE),aggregate_fn=mean,y_label="Rel PnL Ratio",x_label="Strategy",title="Short increases -ve Pnl",x_label_variable="Strategy"))		
sa_visualisations[[3]] <- size_adj_panel

size_adj_panel <- new('Panel')
size_adj_panel@parameters <- 'mfrow=c(2,2),mar=c(6,4,2.5,2.5)'
size_adj_panel@visualns <- list(new("FrequencyPlot",aggregate_what="RelPnlRatio",aggregate_by=c("Strategy","NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,TRUE,FALSE,TRUE),aggregate_fn=mean,y_label="Rel PnL Ratio",x_label="Strategy",title="Long decreases +ve Pnl",x_label_variable="Strategy"),
								new("FrequencyPlot",aggregate_what="RelPnlRatio",aggregate_by=c("Strategy","NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,TRUE,FALSE,FALSE),aggregate_fn=mean,y_label="Rel PnL Ratio",x_label="Strategy",title="Long decreases -ve Pnl",x_label_variable="Strategy"),
								new("FrequencyPlot",aggregate_what="RelPnlRatio",aggregate_by=c("Strategy","NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,FALSE,TRUE,TRUE),aggregate_fn=mean,y_label="Rel PnL Ratio",x_label="Strategy",title="Short decreases +ve Pnl",x_label_variable="Strategy"),
								new("FrequencyPlot",aggregate_what="RelPnlRatio",aggregate_by=c("Strategy","NewPosition","PsnLong","Long","PtvePnLInto"),subset_by=c("NewPosition","PsnLong","Long","PtvePnLInto"),subset_with=list(FALSE,FALSE,TRUE,FALSE),aggregate_fn=mean,y_label="Rel PnL Ratio",x_label="Strategy",title="Short decreases -ve Pnl",x_label_variable="Strategy"))		
sa_visualisations[[4]] <- size_adj_panel

size_adjustment_analysis_module_builder <- new("AnalysisModuleFactory",name = "SizeAdjustmentModule",ppmdl_class = "SizeAdjustmentBatchGatherer",visualisations = sa_visualisations)