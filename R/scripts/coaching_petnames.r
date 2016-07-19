setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules/analysis_module_position_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("coaching_review_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(plotly)

trader   <- 11
dates <- c("2016-01-01")

kf <- function()dated_three_monthly_lookback(trader,dates[1])
history_data <- analysis_module_load_mutiple(trader,dates,history_analysis_module_builder,dated_three_monthly_lookback)
plot_in_viewer <- TRUE
instruments <- unique(history_data$Instrument)

# Pet names --------------------------------------------------------------------
flat_data <- count_flats(history_data)
bubble_data <- build_bubble_data(flat_data[[2]],history_data,'TodayPL',function(x)sum(x,na.rm=TRUE),function(x)sd(x,na.rm=TRUE))
bubble_hist <- plot_ly(bubble_data,x=times_flat,y=n_instruments,mode="markers",color=value,size=alt_value,opacity=0.35)
bubble_hist <- add_trace(bubble_hist,bubble_data,x=times_flat,y=n_instruments,type="bar",opacity=1)
if(plot_in_viewer){
  bubble_hist
}

revisit_data <- create_revisit_data(flat_data[[1]],history_data)
revisit_bucket_data <- build_revisit_plot_data(revisit_data,flat_data[[2]],function(x)sum(x,na.rm=TRUE))
rvp <- plot_ly(data.frame(Visits=1:3,PL=c(revisit_bucket_data[(revisit_bucket_data$TotalVisits==1)&(revisit_bucket_data$Visit==1),'TodayPL'],0,0)),x=Visits,y=PL,type="bar")
rvp <- add_trace(rvp,x=1:3,y=c(0,revisit_bucket_data[(revisit_bucket_data$TotalVisits==2)&(revisit_bucket_data$Visit==1),'TodayPL'],0))
rvp <- add_trace(rvp,x=1:3,y=c(0,revisit_bucket_data[(revisit_bucket_data$TotalVisits==2)&(revisit_bucket_data$Visit==2),'TodayPL'],0))
rvp <- add_trace(rvp,x=1:3,y=c(0,0,revisit_bucket_data[(revisit_bucket_data$TotalVisits==3)&(revisit_bucket_data$Visit==1),'TodayPL']))
rvp <- add_trace(rvp,x=1:3,y=c(0,0,revisit_bucket_data[(revisit_bucket_data$TotalVisits==3)&(revisit_bucket_data$Visit==2),'TodayPL']))
rvp <- add_trace(rvp,x=1:3,y=c(0,0,revisit_bucket_data[(revisit_bucket_data$TotalVisits==3)&(revisit_bucket_data$Visit==3),'TodayPL']))
rvp <- layout(rvp,xaxis=list(title="Visits"),yaxis=list(title="Total PL"))

rvt <- plot_ly(data.frame(Visits=1:3,PL=c(revisit_bucket_data[(revisit_bucket_data$TotalVisits==1)&(revisit_bucket_data$Visit==1),'IsTrade'],0,0)),x=Visits,y=PL,type="bar")
rvt <- add_trace(rvt,x=1:3,y=c(0,revisit_bucket_data[(revisit_bucket_data$TotalVisits==2)&(revisit_bucket_data$Visit==1),'IsTrade'],0))
rvt <- add_trace(rvt,x=1:3,y=c(0,revisit_bucket_data[(revisit_bucket_data$TotalVisits==2)&(revisit_bucket_data$Visit==2),'IsTrade'],0))
rvt <- add_trace(rvt,x=1:3,y=c(0,0,revisit_bucket_data[(revisit_bucket_data$TotalVisits==3)&(revisit_bucket_data$Visit==1),'IsTrade']))
rvt <- add_trace(rvt,x=1:3,y=c(0,0,revisit_bucket_data[(revisit_bucket_data$TotalVisits==3)&(revisit_bucket_data$Visit==2),'IsTrade']))
rvt <- add_trace(rvt,x=1:3,y=c(0,0,revisit_bucket_data[(revisit_bucket_data$TotalVisits==3)&(revisit_bucket_data$Visit==3),'IsTrade']))
rvt <- layout(rvt,xaxis=list(title="Visits"),yaxis=list(title="N Trades"))

rv <- subplot(rvp,rvt,nrows=2)
if(plot_in_viewer){
  rv
}