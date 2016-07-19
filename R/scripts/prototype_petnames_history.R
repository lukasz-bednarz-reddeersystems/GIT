setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules/analysis_module_position_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(plotly)

trader   <- 11
dates <- c("2016-02-01","2016-01-01","2015-12-01")

first <- TRUE
for(date in dates){
  key_func <- function(){dated_three_monthly_lookback(trader,date)}
  analysis <- analysis_module_request(key_func,history_analysis_module_builder)
  if(first){
    history_data <- analysis@ppmdl@modeldata@data  
  }
  else{
    history_data <- unique(rbind(history_data,analysis@ppmdl@modeldata@data))
  }
}

count_flats <- function(history_data){
  instruments <- unique(history_data$Instrument)
  first <- TRUE
  for(ins in instruments){
    ins_data <- unique(history_data[history_data$Instrument==ins,c('Strategy','TradeDate','MarketValue','TodayPL')])
    ins_data <- aggregate(ins_data[c('TodayPL','MarketValue')],list(TradeDate=ins_data$TradeDate,Strategy=ins_data$Strategy),sum)
    if(nrow(ins_data)>1){
      ins_data <- unique(ins_data[c('TradeDate','MarketValue','TodayPL')])
      flat <- ins_data[c(FALSE,((ins_data$MarketValue[2:nrow(ins_data)]==0|is.na(ins_data$MarketValue[2:nrow(ins_data)]))&(!is.na(ins_data$MarketValue[1:(nrow(ins_data)-1)])&ins_data$MarketValue[1:(nrow(ins_data)-1)]!=0))&(abs(ins_data$TodayPL[2:nrow(ins_data)])>100)),]
      open <- ins_data[c(((ins_data$MarketValue[1:(nrow(ins_data)-1)]==0|is.na(ins_data$MarketValue[1:(nrow(ins_data)-1)]))&(!is.na(ins_data$MarketValue[2:nrow(ins_data)])&ins_data$MarketValue[2:nrow(ins_data)]!=0))&(abs(ins_data$TodayPL[2:nrow(ins_data)])>100),FALSE),]
      fd <- flat[flat$MarketValue==0,'TradeDate']  
    }
    else{
      fd <- ins_data$TradeDate 
      open <- data.frame(TradeDate=NULL)
    }
    if(length(fd)==0)fd <- NA
    if(first){
      flat_dates <- data.frame(Instrument=ins,FlatDate=as.Date(fd))
      flat_counts<- data.frame(Instrument=ins,TimesFlat=nrow(flat_dates[!is.na(flat_dates$FlatDate),]))
      open_counts<- data.frame(Instrument=ins,TimesOpen=nrow(open))
      first <- FALSE
    } 
    else{
      fdf <- data.frame(Instrument=ins,FlatDate=as.Date(fd))
      flat_dates <- rbind(flat_dates,fdf)
      flat_counts<- rbind(flat_counts,data.frame(Instrument=ins,TimesFlat=nrow(fdf[!is.na(fdf$FlatDate),])))
      open_counts<- rbind(open_counts,data.frame(Instrument=ins,TimesOpen=nrow(open)))
    }
  }
  return(list(flat_dates,flat_counts,open_counts))
}
flat_data <- count_flats(history_data)

build_bubble_data <- function(flat_counts,data,colname,fn,altfn=function(x)x){
  data <- merge(data,flat_counts,by=c('Instrument'))
  data <- unique(data[c('Instrument','TimesFlat',colname)])
  fcs <- unique(flat_counts$TimesFlat)
  first <- TRUE
  for(tf in 0:max(fcs)){
    if(first){
      rval <- data.frame(times_flat=tf,n_instruments=nrow(flat_counts[flat_counts$TimesFlat==tf,]),value=fn(data[data$TimesFlat==tf,colname]),alt_value=altfn(data[data$TimesFlat==tf,colname]))
      first <- FALSE
    }
    else{
      rval <- rbind(rval,data.frame(times_flat=tf,n_instruments=nrow(flat_counts[flat_counts$TimesFlat==tf,]),value=fn(data[data$TimesFlat==tf,colname]),alt_value=altfn(data[data$TimesFlat==tf,colname])))
    }
  }
  return(rval)
}

bubble_data <- build_bubble_data(flat_data[[2]],history_data,'TodayPL',function(x)sum(x,na.rm=TRUE),function(x)sd(x,na.rm=TRUE))
p<-plot_ly(bubble_data,x=times_flat,y=n_instruments,mode="markers",color=value,size=alt_value,opacity=0.35)
add_trace(p,bubble_data,x=times_flat,y=n_instruments,type="bar",opacity=1)

history_data$Indicator <- 1
build_mosaic_data <- function(flat_data,data,colname,fn){
  data <- merge(data,flat_data[[2]],by=c('Instrument'))
  data <- merge(data,flat_data[[3]],by=c('Instrument'))
  data <- unique(data[c('Instrument','TimesFlat','TimesOpen',colname)])
  fcs <- unique(flat_data[[2]]$TimesFlat)
  ocs <- unique(flat_data[[3]]$TimesOpen)
  mtx <- matrix(rep(NA,(1+max(fcs))*(1+max(ocs))),nrow=max(fcs)+1,ncol=max(ocs)+1)
  for(f in 0:max(fcs)){
    for(o in 0:max(ocs)){
      mtx[f+1,o+1] <- fn(data[(data$TimesFlat==f)&(data$TimesOpen==o),colname])
    }
  }
  return(mtx)
}
mosaic_data <- build_mosaic_data(flat_data,history_data,'TodayPL',sum)
plot_ly(z=mosaic_data,type="heatmap")

create_revisit_data <- function(flat_dates,history_data){
  instruments <- unique(flat_dates$Instrument)
  history_data$Visit <- NA
  for(ins in instruments){
    instrument_dates <- flat_dates[(flat_dates$Instrument==ins)&!is.na(flat_dates$FlatDate),]
    instrument_dates <- instrument_dates[order(instrument_dates$FlatDate),]
    cnt <- 1
    visit_dates <- sort(c(min(history_data$TradeDate),instrument_dates$FlatDate,max(history_data$TradeDate)))
    for(dr in 2:length(visit_dates)){
      history_data[(history_data$Instrument==ins)&(history_data$TradeDate>=visit_dates[dr-1])&(history_data$TradeDate<=visit_dates[dr]),'Visit'] <- cnt
      cnt <- cnt+1
    }
  }
  return(history_data)
}
revisit_data <- create_revisit_data(flat_data[[1]],history_data)

build_revisit_plot_data <- function(revisit_data,times_flat,fn){
  pl_frame <- unique(revisit_data[c('Instrument','TradeDate','TodayPL','Visit','TradeID')])
  pl_frame$IsTrade <- !is.na(pl_frame$TradeID)
  pl_frame <- unique(pl_frame[c('Instrument','TradeDate','TodayPL','Visit','IsTrade')])
  first <- TRUE
  for(r in 1:(max(times_flat$TimesFlat)+1)){
     psns <- times_flat[times_flat$TimesFlat==(r-1),c('Instrument','TimesFlat')]
     bucket_data <- merge(pl_frame,psns,by=('Instrument'))
     bucket_data <- aggregate(bucket_data[c('TodayPL','IsTrade')],list(Visit=bucket_data$Visit),fn)
     if(first){
       all_bucket_data <- cbind(TotalVisits=r,bucket_data)
       first <- FALSE
     }
     else{
       all_bucket_data <- rbind(all_bucket_data,cbind(TotalVisits=r,bucket_data))
     }
  }
  return(all_bucket_data)
}
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
rv




