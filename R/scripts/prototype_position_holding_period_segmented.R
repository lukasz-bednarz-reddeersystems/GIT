setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules/analysis_module_position_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(plotly)

rep_module <- history_analysis_module_builder
trader   <- 11
key_func <- function(){dated_three_monthly_lookback(trader,'2016-01-01')}
analysis <- analysis_module_request(key_func,history_analysis_module_builder)
history_data <- analysis@ppmdl@modeldata@data

#Segment position age
psn_dates <- history_data[!is.na(history_data$TradeID),c('Instrument','TradeDate')]
instruments <- unique(psn_dates$Instrument)
first <- TRUE
for(ins in instruments){
  if(first){
    min_dates <- data.frame(Instrument = ins,TradeDate = min(psn_dates[psn_dates$Instrument==ins,'TradeDate'],na.rm=TRUE))
    first <- FALSE
  }  
  else{
    min_dates <- rbind(min_dates,data.frame(Instrument = ins,TradeDate = min(psn_dates[psn_dates$Instrument==ins,'TradeDate'],na.rm=TRUE)))
  }
}
colnames(min_dates) <- c('Instrument','MinDate')
history_data <- merge(history_data,min_dates,by='Instrument')
history_data$PsnAge <- history_data$TradeDate-history_data$MinDate
category_fn <- function(ages){
  rval <- ages
  rval[ages<=0] <- 0
  rval[ages>0&ages<=10] <- 1
  rval[ages>10&ages<=20] <- 2
  rval[ages>20&ages<=30] <- 3
  rval[ages>30&ages<=40] <- 4
  rval[ages>40&ages<=50] <- 5
  rval[ages>50] <- 6
  rval <- as.numeric(rval)
  return(rval)
}
history_data$PsnAgeCategory <- category_fn(history_data$PsnAge)

#PL in each age bucket
bucket_stats <- function(history_data,colname,tser_fn,xsec_fn){
  history_data <- unique(history_data[c(colname,'Instrument','PsnAgeCategory')])
  history_data <- history_data[history_data$PsnAgeCategory>0,]
  #timeseries aggregate
  history_pl <- aggregate(history_data[colname],list(Instrument=history_data$Instrument,PsnAgeCategory=history_data$PsnAgeCategory),tser_fn)
  #xsectional aggregate
  bucket_totals <- aggregate(history_pl[colname],list(PsnAgeCategory=history_pl$PsnAgeCategory),xsec_fn)
  return(list(bucket_totals,history_pl))
}
bstats <- bucket_stats(history_data,'TodayPL',function(x)sum(x,na.rm=TRUE),function(x)sum(x,na.rm=TRUE))
bucket_totals <- bstats[[1]]

p <- plot_ly(x=bucket_totals$PsnAgeCategory,y=bucket_totals$TodayPL,type="bar")
p <- layout(p,xaxis=list(title="Age Category"),yaxis=list(title="Total PL"))
p

#Cumulative Pl upto psn age at some point (and after)
cumulative_stats <- function(history_data,colname,tser_fn,xsec_fn){
  history_break_point <- list()
  break_point_totals <- list()
  history_data <- history_data[history_data$PsnAgeCategory>0,]
  for(age_category in 1:max(history_data$PsnAgeCategory)){
    break_point <- unique(history_data[c(colname,'Instrument','PsnAgeCategory')])
    break_point$Less <- (break_point$PsnAgeCategory <= age_category)
    history_break_point[[age_category]] <- aggregate(break_point[colname],list(Instrument=break_point$Instrument,Less=break_point$Less),tser_fn)
    break_point_totals[[age_category]]  <- aggregate(history_break_point[[age_category]][colname],list(PsnAgeCategory=history_break_point[[age_category]]$Less),xsec_fn)
  }
  return(list(history_break_point,break_point_totals))
}
cstats <- cumulative_stats(history_data,'TodayPL',function(x)sum(x,na.rm=TRUE),function(x)sum(x,na.rm=TRUE))
break_point_totals <- cstats[[2]]

bp <- plot_ly(x=1:6,y=unlist(Map(function(z)z$TodayPL[nrow(z)],break_point_totals)),type="bar")
bp <- add_trace(bp,x=1:6,y=unlist(Map(function(z)z$TodayPL[1],break_point_totals))[1:5],type="bar")
layout(bp,barmode="stack",yaxis=list(title="Total PL"))
bp

#Market relative PL
colnames(min_dates) <- c('Instrument','TradeDate')
initial_holdings <- unique(merge(history_data,min_dates,by=c('Instrument','TradeDate')))
initial_holdings <- unique(initial_holdings[c('MarketValue','Instrument','ClosePrice')])
colnames(initial_holdings) <- c('EarliestMarketValue','Instrument','EarliestPrice')
history_data <- merge(history_data,initial_holdings,by=c('Instrument'),all.x=TRUE)
history_data$EarliestHolding <- history_data$EarliestMarketValue/history_data$EarliestPrice
history_data$CurrentPassiveValue <- history_data$EarliestHolding*history_data$ClosePrice
history_data$CurrentPassivePL <- history_data$CurrentPassiveValue - history_data$EarliestMarketValue
l <- length(history_data$CurrentPassiveValue)
history_data$PassiveTodayPL <- c(NA,history_data$CurrentPassiveValue[2:l]*(history_data$ClosePrice[2:l]/history_data$PriorClosePrice[2:l])-history_data$CurrentPassiveValue[2:l])
history_data$ActiveTodayPL <- history_data$TodayPL - history_data$PassiveTodayPL

relative_bstats <- bucket_stats(history_data,'PassiveTodayPL',function(x)sum(x,na.rm=TRUE),function(x)sum(x,na.rm=TRUE))
relative_bucket_totals <- relative_bstats[[1]]

p <- plot_ly(x=relative_bucket_totals$PsnAgeCategory,y=relative_bucket_totals$PassiveTodayPL,type="bar")
p <- layout(p,xaxis=list(title="Age Category"),yaxis=list(title="Total PL"))
p

pssv_cstats <- cumulative_stats(history_data,'PassiveTodayPL',function(x)sum(x,na.rm=TRUE),function(x)sum(x,na.rm=TRUE))
break_point_totals <- pssv_cstats[[2]]

bp <- plot_ly(x=1:6,y=unlist(Map(function(z)z$PassiveTodayPL[nrow(z)],break_point_totals)),type="bar")
bp <- add_trace(bp,x=1:6,y=unlist(Map(function(z)z$PassiveTodayPL[1],break_point_totals))[1:5],type="bar")
layout(bp,barmode="stack")
bp

relative_bstats <- bucket_stats(history_data,'ActiveTodayPL',function(x)sum(x,na.rm=TRUE),function(x)sum(x,na.rm=TRUE))
relative_bucket_totals <- relative_bstats[[1]]

p <- plot_ly(x=relative_bucket_totals$PsnAgeCategory,y=relative_bucket_totals$ActiveTodayPL,type="bar")
p <- layout(p,xaxis=list(title="Age Category"),yaxis=list(title="Active PL"))
p

#PL if positions offside in last time segment are removed
history_pl <- bstats[[2]]
has_been_offside <- function(history_data,colname){
  history_pl <- aggregate(history_data[colname],list(Instrument=history_data$Instrument,PsnAgeCategory=history_data$PsnAgeCategory),function(x)sum(x,na.rm=TRUE))
  history_pl <- history_pl[history_pl$PsnAgeCategory>0,]
  instruments<- unique(history_pl$Instrument)
  history_pl$OffsideSince <- max(history_pl$PsnAgeCategory,na.rm=TRUE)+1
  for(ins in instruments){
    this_ins <- history_pl$Instrument==ins
    off_dex <- min(history_pl[this_ins&history_pl[colname]<0,'PsnAgeCategory'],na.rm=TRUE)
    history_pl[this_ins,'OffsideSince'] <- off_dex
  }
  history_pl[is.infinite(history_pl$OffsideSince),'OffsideSince'] <- max(history_pl$PsnAgeCategory,na.rm=TRUE)+1
  return(history_pl)
}
offside_data <- has_been_offside(history_pl,'TodayPL')

cumulative_stats_onside <- function(history_data,colname,tser_fn,xsec_fn){
  history_break_point <- list()
  break_point_totals <- list()
  for(age_category in 1:max(history_data$PsnAgeCategory)){
    break_point <- unique(history_data[c(colname,'Instrument','PsnAgeCategory','OffsideSince')])
    break_point$Less <- (break_point$PsnAgeCategory <= age_category)
    history_break_point[[age_category]] <- aggregate(break_point[colname],list(Instrument=break_point$Instrument,Less=break_point$Less),tser_fn)
    break_point_totals[[age_category]]  <- aggregate(history_break_point[[age_category]][colname],list(PsnAgeCategory=history_break_point[[age_category]]$Less),xsec_fn)
  }
  return(list(history_break_point,break_point_totals))
}
cstats_onside <- cumulative_stats_onside(offside_data,'TodayPL',function(x)sum(x,na.rm=TRUE),function(x)sum(x,na.rm=TRUE))
break_point_totals_onside <- cstats_onside[[2]]

bp <- plot_ly(x=1:6,y=unlist(Map(function(z)z$TodayPL[nrow(z)],break_point_totals_onside)),type="bar")
bp <- add_trace(bp,x=1:6,y=unlist(Map(function(z)z$TodayPL[1],break_point_totals_onside))[1:5],type="bar")
bp <- layout(bp,xaxis=list(title="Age Category"),yaxis=list(title="Total PL"))
bp

#Market relative PL if offside positions are removed
history_pl <- relative_bstats[[2]]
offside_data <- has_been_offside(history_pl,'ActiveTodayPL')

cstats_onside <- cumulative_stats_onside(offside_data,'ActiveTodayPL',function(x)sum(x,na.rm=TRUE),function(x)sum(x,na.rm=TRUE))
break_point_totals_onside <- cstats_onside[[2]]

bp <- plot_ly(x=1:6,y=unlist(Map(function(z)z$ActiveTodayPL[nrow(z)],break_point_totals_onside)),type="bar")
bp <- add_trace(bp,x=1:6,y=unlist(Map(function(z)z$ActiveTodayPL[1],break_point_totals_onside))[1:5],type="bar")
bp <- layout(bp,xaxis=list(title="Age Category"),yaxis=list(title="Active PL"))
bp

t<-data.frame(PL=unlist(Map(function(z)z$ActiveTodayPL[nrow(z)],break_point_totals_onside)),Age=1:6)
qplot(factor(Age), data=t, geom="bar", fill=factor(t$PL>0),weight=PL)

#Average PL vs psn age:
pl_frame <- history_data[history_data$PsnAgeCategory>0,]
pl_frame <- unique(pl_frame[c('Instrument','TradeDate','TodayPL','PsnAgeCategory','ActiveTodayPL','PsnAge')])

av_bstats <- bucket_stats(pl_frame,'TodayPL',function(x)mean(x,na.rm=TRUE),function(x)mean(x,na.rm=TRUE))
av_bucket_totals <- av_bstats[[1]]

#av_cstats <- cumulative_stats(pl_frame,'TodayPL',function(x)mean(x,na.rm=TRUE),function(x)sum(x,na.rm=TRUE))
#av_break_point_totals <- av_cstats[[2]]
#plot(unlist(Map(function(z)z$TodayPL[nrow(z)],av_break_point_totals)))

#tseries <- aggregate(pl_frame['TodayPL'],list(TradeDate=pl_frame$TradeDate),function(x)sum(x,na.rm=TRUE))
#plot(tseries$TradeDate,cumsum(tseries$TodayPL))

#tseries <- aggregate(pl_frame['ActiveTodayPL'],list(TradeDate=pl_frame$TradeDate),function(x)sum(x,na.rm=TRUE))
#plot(tseries$TradeDate,cumsum(tseries$ActiveTodayPL))

tseries <- aggregate(pl_frame['TodayPL'],list(Age=pl_frame$PsnAge),function(x)sum(x,na.rm=TRUE))
cumdata <- data.frame(Age=tseries$Age,TotalPL=cumsum(tseries$TodayPL))
p <- plot_ly(cumdata, x = Age, y = TotalPL)
p

tseries <- aggregate(pl_frame['ActiveTodayPL'],list(Age=pl_frame$PsnAge),function(x)sum(x,na.rm=TRUE))
cumdata <- data.frame(Age=tseries$Age,TotalPL=cumsum(tseries$ActiveTodayPL))
p <- plot_ly(cumdata, x = Age, y = TotalPL)
p