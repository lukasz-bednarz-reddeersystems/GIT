setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules/analysis_module_position_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(TeachingDemos)

#rep_module <- history_analysis_module_builder

#trader   <- 11
#to_date  <- Sys.Date()
#key_func <- function(){dated_three_monthly_lookback(trader,as.character(to_date))}
#analysis <- createAnalysisModule(rep_module,key_func)
#debug(analysis@ppmdl@post_comp@compute)
#analysis <- updateAnalysisModel(analysis)
#history_data <- analysis@ppmdl@modeldata@data

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

long_offside_loosing <- subset(history_data,history_data$Age>50&history_data$PsnLong&history_data$Offside&!history_data$PtvePnLOutof)
old_positions <- long_offside_loosing

#plot for most and least profitable positions in the period
ins_most <- old_positions$Instrument[old_positions$Total.PL==max(old_positions$Total.PL)]
ins_least<- old_positions$Instrument[old_positions$Total.PL==min(old_positions$Total.PL)]

#focus context for old long positions
focus_long <- subset(history_data,(history_data$Age>60)&history_data$Offside&history_data$PsnLong&history_data$Long&(!history_data$PtvePnLOutof))

add_trades_text <- function(trades,data,dates){
  for(t in 1:nrow(trades)){
    if(trades$Long[t]){
      l <- 'B'
    }
    else{
      l <- 'S'
    }
    text(trades$TradeDate[t],data[dates==trades$TradeDate[t]],labels=l,offset=0.5)
  }
}

add_trades <- function(trades,data,dates){
  for(t in 1:nrow(trades)){
    if(trades$Long[t]){
      col <- 'blue'
      bg <- 'blue'
      pch <- 24
    }
    else{
      col <- 'red'
      bg <- 'red'
      pch <- 25
    }
    points(trades$TradeDate[t],data[dates==trades$TradeDate[t]][[1]],col=col,pch=pch,bg=bg,cex=2)
  }
}

add_name <- function(name,data){
  text(data$TradeDate[round(length(data$TradeDate)/2)],max(data$ClosePrice),labels=name)
}

format_labels <- function(values){
  labels <- unlist(Map(function(x)sprintf("$%d",round(x)),values))
  return(labels)
}

add_delta_subplot <- function(data_in,x,y,s=c(0.5,0.5),y_offset=NULL,x_offset=NULL){
  lbls <- format_labels(data_in)
  sbplt <- list()
  sbplt[[1]]<-data_in/max(data_in)
  sbplt[[2]]<-lbls
  yoff <- max(sbplt[[1]])/2
  plt_helper <- function(data_in){midpoints <- barplot(data_in[[1]],ylim=c(0,1),axes=FALSE,las=2,font=2)
                                  text(midpoints, yoff, labels=data_in[[2]], srt=90,font=2)}
  if(length(y_offset)>0)y <- y+y_offset
  if(length(x_offset)>0)x <- x+x_offset
  subplot(plt_helper(sbplt),x,y,size=s)
}

add_levels <- function(data){
  points(data$TradeDate,data$StopLoss,col="red",pch="-",cex=4) 
  points(data$TradeDate,data$ProfitTarget,col="blue",pch="-",cex=4) 
}

add_events <- function(data,events,value_col='ClosePrice'){
  all_events <- rep(list('None'),length(data$TradeDate))
  names(all_events) <- as.character(data$TradeDate)
  for(event_name in events){
    event_dates <- data$TradeDate[data[[event_name]]]
    if(length(event_dates)>0){
      lbl <- gsub("\\."," ",event_name)
      all_events[as.character(event_dates)][[1]] <- c(all_events[as.character(event_dates)][[1]],lbl) 
    }
  }
  offset <- (max(data[[value_col]],na.rm=TRUE)-min(data[[value_col]],na.rm=TRUE))/10
  event_indicies <- which(unlist(Map(length,all_events))>1)
  for(i in event_indicies){
    value <- data[data$TradeDate==as.Date(names(all_events)[i]),value_col]
    evt <- all_events[[i]][all_events[[i]]!='None']
    lbl <- paste(evt,collapse=", ")
    if(length(value)>0&!is.na(value)){
      text(as.Date(names(all_events)[i]),value+offset,labels=lbl,font=2,cex=1.5) 
      points(as.Date(names(all_events)[i]),value,col="black",pch="*",cex=4)
    }
  }
}

plot_psn_history <- function(ins,data,scale=c(1,1),adjust_plots=NULL){
  plt_data <- subset(data,data$Instrument==ins)
  trades <- unique(plt_data[!is.na(plt_data$TradeID),c('TradeDate','Long')])
  title <- unique(plt_data$Name)
  all_events <- colnames(plt_data)[which(colnames(plt_data)=="Results"):(which(colnames(plt_data)=="Rationale")-1)]
  layout(matrix(c(1,1,2,2,3,3,3,3),4,2,byrow=TRUE))
  par(mar=c(0,6,0,2))
  plot(plt_data$TradeDate,plt_data$ClosePrice,axes=FALSE, frame.plot=TRUE, ylab="Close Price",cex.lab=2)
  prc <- plt_data$ClosePrice[!is.na(plt_data$ClosePrice)]
  p_mvg <- (cumsum(prc)[(5+1):length(prc)] - cumsum(prc)[1:(length(prc)-5)])/5
  dts <- plt_data$TradeDate[!is.na(plt_data$ClosePrice)]
  lines(dts[6:length(dts)],p_mvg,type="l",col="black",lwd=3)
  title_y <- max(plt_data$ClosePrice,na.rm=TRUE)-(max(plt_data$ClosePrice,na.rm=TRUE)-min(plt_data$ClosePrice,na.rm=TRUE))/8
  text(plt_data$TradeDate[round(length(plt_data$TradeDate)/5)],title_y,labels=title,cex=3) 
  add_trades(trades,plt_data$ClosePrice,plt_data$TradeDate)
  add_events(plt_data,all_events)
  add_levels(plt_data)
  Axis(side=1, labels=FALSE)
  Axis(side=2, labels=TRUE, cex.axis=2)
  add_name(title,plt_data)
  par(mar=c(0,6,0,2))
  plot(plt_data$TradeDate,plt_data$MarketValue,axes=FALSE, frame.plot=TRUE, ylab="Market Value",cex.lab=2)
  rng <- 1:length(plt_data$TradeDate)
  lines(plt_data$TradeDate,predict(loess(plt_data$MarketValue~rng,span=0.1)), col='black', lwd=2)
  add_trades(trades,plt_data$MarketValue,plt_data$TradeDate)
  Axis(side=1, labels=FALSE)
  Axis(side=2, labels=TRUE, cex.axis=2)
  pl <- plt_data$TodayPL
  pl[is.na(pl)] <- 0
  plsum <- cumsum(pl)
  initial_date <- min(plt_data$TradeDate[!is.na(plt_data$ClosePrice)&!is.na(plt_data$MarketValue)])
  initial_price <- plt_data$ClosePrice[plt_data$TradeDate==initial_date]
  initial_shares <- (plt_data$MarketValue[plt_data$TradeDate==initial_date])/initial_price
  psv_pl <- (initial_shares*plt_data$ClosePrice)-plt_data$MarketValue[plt_data$TradeDate==initial_date]
  par(mar=c(4,6,0,2))
  pl_y_min <- round(min(plsum)*2)
  pl_y_max <- round(max(c(max(plsum,na.rm=TRUE)*2,max(psv_pl,na.rm=TRUE)*2))) 
  plot(plt_data$TradeDate,plsum,ylab="Total PL", xlab="Date", ylim=c(pl_y_min,pl_y_max),cex.lab=2,cex.axis=2)
  l_mvg <- (cumsum(plsum)[(5+1):length(plsum)] - cumsum(plsum)[1:(length(plsum)-5)])/5
  lines(plt_data$TradeDate[6:length(plt_data$TradeDate)],l_mvg,type="l",col="black",lwd=2)
  s_mvg <- (cumsum(plsum)[(2+1):length(plsum)] - cumsum(plsum)[1:(length(plsum)-2)])/2
  lines(plt_data$TradeDate[3:length(plt_data$TradeDate)],s_mvg,type="l",col="green",lwd=2)
  lines(plt_data$TradeDate,psv_pl,type="l",col="grey",lwd=3)
  add_trades(trades,plsum,plt_data$TradeDate)
  trade_rows <- which(!is.na(plt_data$TradeID))
  rw <- trade_rows[1]
  y_offsets <- sign(s_mvg[(length(s_mvg)-length(l_mvg)+1):length(s_mvg)]-l_mvg)*abs(pl_y_max/4)
  y_offsets <- c(rep(0,length(plsum)-length(y_offsets)),y_offsets)
  x_offsets <- rep(0,length(plsum))
  x_offsets[trade_rows] <- sign(y_offsets[trade_rows])
  x_offsets[trade_rows] <- cumsum(x_offsets[trade_rows])
  x_offsets[abs(x_offsets)==1] <- 0
  x_offsets[x_offsets>0] <- x_offsets[x_offsets>0]-1 
  x_offsets <- x_offsets*3
  last_y_off <- 0
  trd_plt<-TRUE
  cnt <- 1
  for(rw in trade_rows){
    data_in <- c(plt_data$DeltaPL[rw],plt_data$DeltaSwing[rw],plt_data$DeltaSkew[rw])
    names(data_in) <- c("PL","Swing","Skew")
    y_off <- y_offsets[rw]
    if(sign(last_y_off)==sign(y_off)){
      y_off <- -1*y_off
    }
    if(rw!=trade_rows[1])trd_plt<-data_in!=last_data_in
    if(trd_plt){
      tryCatch({
        if(length(adjust_plots)>0){
          dx<-adjust_plots[[cnt]][1]
          dy<-adjust_plots[[cnt]][2]
        }
        else{
          dx <- 0 
          dy <- 0
        }
        add_delta_subplot(data_in,plt_data$TradeDate[rw],plsum[rw],y_offset=y_off+dy,x_offset=x_offsets[rw]+dx,s=scale)  
      },error=function(cond){
        message(paste("Failed to add trade subplot for row",rw,":",cond)) 
      })  
    }
    last_y_off <- y_off
    last_data_in <- data_in
    cnt <- cnt+1
  }
}

#Pets
#39215

