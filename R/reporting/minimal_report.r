library(knitr)
library(xtable)
sourceTo("../lib/frame_to_xml.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/analysis_objectstore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_module_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("raid_data_import.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

trader   <- 11
to_date  <- Sys.Date()
rep_modules  <- list(results_daypsn_analysis_module_builder,
                     results_daypsntraded_analysis_module_builder,
                     sizing_analysis_module_builder,
                     size_adjustment_ratio_analysis_module_builder,
                     primary_placing_psn_analysis_module_builder,
                     trd_around_prmryplacing_analysis_module_builder,
                     secondary_placing_psn_analysis_module_builder,
                     trd_around_sndryplacing_analysis_module_builder,
                     pet_names_analysis_module_builder,
                     pet_names_traded_analysis_module_builder,
                     holding_period_analysis_module_builder,
                     price_snapshot_analysis_module_builder,
                     stop_loss_analysis_module_builder,
                     profit_target_analysis_module_builder,
                     average_down_analysis_module_builder,
                     level_snapshot_analysis_module_builder,
                     entry_level_analysis_module_builder)

get_summary <- function(trader,to_date){
  key_func <- function(){dated_three_monthly_lookback(trader,as.character(to_date))}
  kv <- key_func()
  #last month data
  start <- kv[1,2]+1
  end <- kv[1,3]
  data <- list()
  data[[1]] <- get_trader_performance(trader,start,end)
  data[[2]] <- get_portfolio_analysis(trader,start,end)
  data[[3]] <- get_top_positions(trader,start,end)
  data[[4]] <- get_bottom_positions(trader,start,end)
  #full interval data
  start <- kv[nrow(kv),2]+1
  data[[5]] <- get_trader_performance(trader,start,end)
  data[[6]] <- get_portfolio_analysis(trader,start,end)
  data[[7]] <- get_top_positions(trader,start,end)
  data[[8]] <- get_bottom_positions(trader,start,end)
  return(data)
}

run_modules <- function(trader,to_date,rep_modules,force=FALSE){
  key_func <- function(){dated_three_monthly_lookback(trader,as.character(to_date))}
  key_hash <- as.character(murmur3.32(as.character(key_func())))
  kv <- key_func()
  hrname <- paste(kv[[1,1]],"_",as.character(min(kv[['start']])),"_",as.character(max(kv[['end']])),sep="")
  
  cnt <- 1
  modules <- list()
  report_data <- list()
  for(run_module in rep_modules){
    analysis <- createAnalysisModule(run_module,key_func)
    analysis <- updateAnalysisModel(analysis)
    
    if(length(intersect(class(analysis)[[1]],c("HoldingPeriodSnapshotModule","TradeLevelSnapshotModule")))>0){
      analysis <- togglePlotNone(analysis)  
    }
    analysis <- runAnalysisModule(analysis)
    
    d<-getModuleData(analysis)
    data <- d[[length(d)]][[1]]
    report_data[[cnt]] <- tryCatch({
                                    format_for_report(data,sort_on=1)
                                   }, error=function(cond){
                                    message(paste("Data formatting failed on:",class(analysis)[[1]],cond)) 
                                   })
    cnt <- cnt+1
    analysis <- NULL
  }
  return(report_data)
}

format_for_report <- function(data,sort_on=NULL){
  raw_names <- colnames(data)
  names_types <- parse_column_names(raw_names)
  nm <- names_types[[1]]
  ty <- names_types[[2]]
  for(col in 1:ncol(data)){
    data[raw_names[col]] <- unlist(Map(format_value,ty[[col]],data[raw_names[col]][[1]])) 
  }
  colnames(data) <- unlist(nm)
  if(length(sort_on)>0)data[order(data[,sort_on]),]
  return(data)
}

summary_data <- get_summary(trader,to_date)
for(i in 1:length(summary_data)){
  summary_data[[i]] <- format_for_report(summary_data[[i]],sort_on=1)
}
perf_sum <- data.frame(summary_data[[1]][,1],summary_data[[5]][,1],summary_data[[1]][,2],summary_data[[5]][,2],summary_data[[1]][,4],summary_data[[5]][,4],summary_data[[1]][,5],summary_data[[5]][,5])
colnames(perf_sum) <- c(paste(colnames(summary_data[[1]])[1],"1m",sep=""),paste(colnames(summary_data[[1]])[1],"3m",sep=""),paste(colnames(summary_data[[1]])[2],"1m",sep=""),paste(colnames(summary_data[[1]])[2],"3m",sep=""),paste(colnames(summary_data[[1]])[4],"1m",sep=""),paste(colnames(summary_data[[1]])[4],"3m",sep=""),paste(colnames(summary_data[[1]])[5],"1m",sep=""),paste(colnames(summary_data[[1]])[5],"3m",sep=""))

report_data <- run_modules(trader,to_date,rep_modules)
knit('minimal_report.Rnw')