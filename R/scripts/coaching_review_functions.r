library(quantmod)
library(sets)
library(lubridate)

undo_bps <- function(value){
  return(value/10000)
}
avg_vol <- function(vol){
  return(sqrt(mean(as.matrix(vol*vol),na.rm=TRUE)))
}
avg_skw <- function(skw){
  return(mean(as.matrix(skw),na.rm=TRUE))
}
avg_swing <- function(vol_val){
  return(mean(as.matrix(vol_val$Value*undo_bps(vol_val$Vol)),na.rm=TRUE))
}
property_fn <- function(frame,prop,fn){
  return(fn(frame[prop])) 
}
average_vol <- function(frame,into){
  if(into){
    direction <- c('VolInto')
  }
  else{
    direction <- c('VolOutof')
  }
  return(property_fn(frame,direction,function(x)avg_vol(undo_bps(x))))
}
average_swing <- function(frame,into){
  if(into){
    direction <- c('VolInto')
  }
  else{
    direction <- c('VolOutof')
  }
  df <- frame[c('Av.MarketValue',direction)]
  colnames(df) <- c('Value','Vol')
  return(property_fn(df,c('Value','Vol'),avg_swing))
}
average_vol <- function(frame,into){
  if(into){
    direction <- c('VolInto')
  }
  else{
    direction <- c('VolOutof')
  }
  return(property_fn(frame,direction,function(x)avg_vol(undo_bps(x))))
}
average_skew <- function(frame,into){
  if(into){
    direction <- c('SkewInto')
  }
  else{
    direction <- c('SkewOutof')
  }
  return(property_fn(frame,direction,avg_skw))
}

compute_buckets <- function(df,psn_long,trade_buy,psn_new)
{
  earnings_frame <- subset(df,(df$PsnLong==psn_long)&(df$Long==trade_buy)&(df$NewPosition==psn_new))
  
  volin   <- average_vol(earnings_frame,TRUE)
  swingin <- average_swing(earnings_frame,TRUE)
  volout  <- average_vol(earnings_frame,FALSE)
  swingout<- average_swing(earnings_frame,FALSE)
  
  skew_in <- average_skew(earnings_frame,TRUE)
  skew_out<- average_skew(earnings_frame,FALSE)
  
  hitrate <- mean(earnings_frame$Hit1D,na.rm=TRUE)
  mean_win<- mean(earnings_frame$TodayPL[earnings_frame$TodayPL>0],na.rm=TRUE)
  mean_loss<-mean(earnings_frame$TodayPL[earnings_frame$TodayPL<0],na.rm=TRUE)
  win_loss<- mean_win/abs(mean_loss)  
  expected_win_loss <- hitrate*win_loss
  
  mean_ptvepnl_in <- mean(earnings_frame$PnLInto[earnings_frame$PnLInto>0],na.rm=TRUE)
  mean_ntvepnl_in<- mean(earnings_frame$PnLInto[earnings_frame$PnLInto<0],na.rm=TRUE)
  advance_ratio_in<- mean_ptvepnl_in/abs(mean_ntvepnl_in)
  
  mean_ptvepnl_out <- mean(earnings_frame$PnLOutof[earnings_frame$PnLOutof>0],na.rm=TRUE)
  mean_ntvepnl_out<- mean(earnings_frame$PnLOutof[earnings_frame$PnLOutof<0],na.rm=TRUE)
  advance_ratio_out<- mean_ptvepnl_out/abs(mean_ntvepnl_out)  
  
  in_vars  <- c(volin,swingin,skew_in,mean_ptvepnl_in,mean_ntvepnl_in,advance_ratio_in)
  on_trade <- c(hitrate,mean_win,mean_loss,win_loss,expected_win_loss)
  out_vars <- c(volout,swingout,skew_out,mean_ptvepnl_out,mean_ntvepnl_out,advance_ratio_out)
  in_vars[is.na(in_vars)] <- 0
  out_vars[is.na(out_vars)] <- 0
  
  return(list(in_vars,out_vars,on_trade))
}

norm_data <- function(data_in,data_out,nme){
  new_data <- c()
  for(i in 1:length(data_in)){
    new_data <- c(new_data,abs(data_in[i])/(abs(data_in[i])+abs(data_out[i])))
  }
  names(new_data) <- nme
  return(new_data)
}

try_format <- function(label,value){
  rval <- tryCatch({
              sprintf(label,value)
          }, error=function(cond){
              message(paste("Format label failed on value,",value,":",cond))
          })
  return(rval)
}

format_labels <- function(values){
  labels <- c()
  labels <- c(labels,try_format("%d bps",round(10000*values[1])))
  labels <- c(labels,try_format("$%d",round(values[2])))
  labels <- c(labels,try_format("%.2f",values[3]))
  labels <- c(labels,try_format("$%d",round(values[4])))
  labels <- c(labels,try_format("$%d",round(values[5])))
  labels <- c(labels,try_format("%.2f",values[6]))
  return(labels)
}

m_layout <- layout
plot_price_snapshot <- function(prc_module,exp_module,in_data,panel,psn_long,buy,new_psn){
  plot_data <- compute_buckets(in_data,psn_long,buy,new_psn)
  in_vars <- plot_data[[1]]
  out_vars<- plot_data[[2]]
  on_trade<- plot_data[[3]]
  
  in_vars[is.nan(in_vars)] <- NA
  
  out_vars[is.nan(out_vars)] <- NA
  nmes <- c("Vol","Swing","Skew","+tvePL","-tvePL","Advance")
  
  normed_data_in <- norm_data(in_vars,out_vars,nmes)
  normed_data_out <- norm_data(out_vars,in_vars,nmes)
  
  p <- panel
  m <-c(1, 2, 3, 3, 4, 4)
  frame()
  m_layout(matrix(m, 3, 2, byrow = TRUE))
  midpoints <- barplot(normed_data_in,ylim=c(0,1.5),axes=FALSE,las=2,font=2,main="Pre-trade")
  text(midpoints, normed_data_in+0.15, labels=format_labels(in_vars), srt=90,font=2)
  midpoints <- barplot(normed_data_out,ylim=c(0,1.5),axes=FALSE,las=2,font=2,main="Post-trade")
  text(midpoints, normed_data_out+0.15, labels=format_labels(out_vars), srt=90, font=2)
  prc_module@panels[[p]]@visualns[[1]]@visuln_dspl(prc_module@panels[[p]]@visualns[[1]])
  text(1, -0.05, labels=sprintf("%.2f%% hit rate",on_trade[1]*100))
  text(1, -0.075, labels=sprintf("%.2f win loss",on_trade[4]))
  text(1, -0.1, labels=sprintf("%.2f extraction",on_trade[5]))
  exp_module@panels[[p]]@visualns[[1]]@visuln_dspl(exp_module@panels[[p]]@visualns[[1]])
}

age_category_fn <- function(ages){
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

categorise_psn_ages <- function(history_data){
  if(!('MinDate' %in% colnames(history_data))){
    psn_dates <- unique(history_data[!is.na(history_data$TradeID),c('Instrument','TradeDate')])
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
  }
  else{
    min_dates <- history_data$MinDate
  }
  if(!('PsnAge' %in% colnames(history_data))){
    history_data$PsnAge <- history_data$TradeDate-history_data$MinDate  
  }
  history_data$PsnAgeCategory <- age_category_fn(history_data$PsnAge)
  return(list(history_data,min_dates))
}

bucket_stats <- function(history_data,colname,tser_fn,xsec_fn){
  history_data <- unique(history_data[c(colname,'Instrument','PsnAgeCategory')])
  history_data <- history_data[history_data$PsnAgeCategory>0,]
  #timeseries aggregate
  history_pl <- aggregate(history_data[colname],list(Instrument=history_data$Instrument,PsnAgeCategory=history_data$PsnAgeCategory),tser_fn)
  #xsectional aggregate
  bucket_totals <- aggregate(history_pl[colname],list(PsnAgeCategory=history_pl$PsnAgeCategory),xsec_fn)
  return(list(bucket_totals,history_pl))
}

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

market_rel_pl <- function(history_data,trade_rel=FALSE,index="^SX5E"){
  if(trade_rel){
    message("Computing market relative quantities against earliest TRADE dates")
    psn_dates <- unique(history_data[!is.na(history_data$TradeID),c('Instrument','TradeDate','Strategy')])  
  }
  else{
    message("Computing market relative quantities against earliest POSITION dates")
    psn_dates <- unique(history_data[c('Instrument','TradeDate','Strategy')])  
  }
  instruments <- unique(psn_dates$Instrument)
  strategies <- unique(psn_dates$Strategy)
  history_data <- history_data[order(history_data$TradeDate),]
  first <- TRUE
  for(ins in instruments){
    for(st in strategies){
      if(first){
        min_dates <- data.frame(Instrument = ins,Strategy=st,TradeDate = min(psn_dates[psn_dates$Instrument==ins&psn_dates$Strategy==st,'TradeDate'],na.rm=TRUE))
        first <- FALSE
      }  
      else{
        min_dates <- rbind(min_dates,data.frame(Instrument = ins,Strategy=st,TradeDate = min(psn_dates[psn_dates$Instrument==ins&psn_dates$Strategy==st,'TradeDate'],na.rm=TRUE)))
      }
    }
    history_data$PriorClosePrice[history_data$Instrument==ins] <- c(history_data$ClosePrice[history_data$Instrument==ins][1],history_data$ClosePrice[history_data$Instrument==ins][1:(length(history_data$ClosePrice[history_data$Instrument==ins])-1)])
  }
  getSymbols(index)
  index <- ifelse(substr(index,1,1)=='^',substr(index,2,nchar(index)),index)
  idata <- as.data.frame(get(index))
  cname <- paste(index,".Close",sep="")
  index <- data.frame(TradeDate=as.Date(rownames(as.data.frame(idata))),idata[cname])
  history_data <- merge(history_data,index,by='TradeDate')
  colnames(min_dates) <- c('Instrument','Strategy','MinDate')
  history_data <- merge(history_data,min_dates,by=c('Instrument','Strategy'),all.x=TRUE)
  colnames(min_dates) <- c('Instrument','Strategy','TradeDate')
  initial_holdings <- unique(merge(history_data,min_dates,by=c('Instrument','Strategy','TradeDate')))
  initial_holdings <- unique(initial_holdings[c('MarketValue','Instrument','ClosePrice','Strategy',cname)])
  colnames(initial_holdings) <- c('EarliestMarketValue','Instrument','EarliestPrice','Strategy','EarliestIndexLevel')
  history_data <- merge(history_data,initial_holdings,by=c('Instrument','Strategy'),all.x=TRUE)
  history_data$EarliestHolding <- history_data$EarliestMarketValue/history_data$EarliestPrice
  history_data$EarliestIndexHolding <- history_data$EarliestMarketValue/history_data$EarliestIndexLevel
  history_data$CurrentPassiveValue <- history_data$EarliestHolding*history_data$ClosePrice
  history_data$PriorPassiveValue <- history_data$EarliestHolding*history_data$PriorClosePrice
  history_data$CurrentIndexValue <- history_data$EarliestIndexHolding*history_data[[cname]]
  history_data$CurrentPassivePL <- history_data$CurrentPassiveValue - history_data$EarliestMarketValue
  l <- length(history_data$CurrentPassiveValue)
  history_data$PassiveTodayPL <- history_data$PriorPassiveValue*(history_data$ClosePrice/history_data$PriorClosePrice)-history_data$PriorPassiveValue
  history_data$ActiveTodayPL <- history_data$TodayPL - history_data$PassiveTodayPL
  first <- TRUE
  for(ins in instruments){
    for(st in strategies){
      #Need to remove duplicate rows so that PL cumulant is correct
      local_frame <- unique(history_data[history_data$Instrument==ins&history_data$Strategy==st,c('TradeDate','Instrument','Strategy','TodayPL','CurrentIndexValue','ActiveTodayPL','PassiveTodayPL')])
      m <- nrow(local_frame)
      if(m>0){
        local_frame <- local_frame[order(local_frame$TradeDate),]
        if(m>1){
          local_frame$CurrentIndexPL <- c(0,local_frame$CurrentIndexValue[2:m] - local_frame$CurrentIndexValue[1:(m-1)])  
        }
        else{
          local_frame$CurrentIndexPL <- 0
        }
        local_frame$CumulativeActivePL <- NA
        local_frame$CumulativePassivePL <- NA
        local_frame$MarketRelPL <- local_frame$TodayPL - local_frame$CurrentIndexPL
        local_frame$TodayPL[is.na(local_frame$TodayPL)] <- 0
        local_frame$MarketRelPL[is.na(local_frame$MarketRelPL)] <- 0
        local_frame$ActiveTodayPL[is.na(local_frame$ActiveTodayPL)] <- 0
        local_frame$PassiveTodayPL[is.na(local_frame$PassiveTodayPL)] <- 0
        local_frame$CumulativePL <- cumsum(local_frame$TodayPL)
        local_frame$CumulativeMarketRelPL <- cumsum(local_frame$MarketRelPL)
        local_frame$CumulativeActivePL <- cumsum(local_frame$ActiveTodayPL)
        local_frame$CumulativePassivePL <- cumsum(local_frame$PassiveTodayPL)
        if(first){
          full_frame <- local_frame
          first <- FALSE
        }
        else{
          full_frame <- rbind(full_frame,local_frame)
        }
      }
    }
  }
  history_data <- merge(history_data[setdiff(colnames(history_data),'CumulativePL')],full_frame[c('TradeDate','Instrument','Strategy','CumulativePL','CumulativeMarketRelPL','MarketRelPL','CumulativeActivePL','CumulativePassivePL')],by=c('TradeDate','Instrument','Strategy'))
  return(history_data)
}

integrated_offside <- function(history_data,type='IntegratedPL'){
  instruments <- unique(history_data$Instrument)
  instruments_overall_offside <- c()
  instruments_not_offside <- c()
  off_first <- TRUE
  on_first <- TRUE
  for(ins in instruments){
    off <- history_data[history_data$Instrument==ins&history_data[type]<0,]
    if(nrow(off)>0){
      instruments_overall_offside <- c(instruments_overall_offside,ins)
      if(off_first){
        offside_data <- history_data[history_data$Instrument==ins,] 
        off_first <-  FALSE
      }
      else{
        offside_data <- rbind(offside_data,history_data[history_data$Instrument==ins,] )
      }
    }
    else{
      instruments_not_offside <- c(instruments_not_offside,ins)
      if(on_first){
        onside_data <- history_data[history_data$Instrument==ins,] 
        on_first <-  FALSE
      }
      else{
        onside_data <- rbind(onside_data,history_data[history_data$Instrument==ins,] )
      }
    }
  }
  return(list(offside_data,onside_data,instruments_overall_offside,instruments_not_offside))
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

days_offside <- function(history_data){
  history_data$DaysOff<- NA
  for(ins in unique(hd$Instrument)){
    hd[hd$Instrument==ins,]$DaysOff <- cumsum(hd[hd$Instrument==ins,]$RelOff)  
}
}

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

build_revisit_plot_data <- function(revisit_data,times_flat,fn){
  pl_frame <- unique(revisit_data[c('Instrument','TradeDate','TodayPL','Visit','TradeID')])
  pl_frame$IsTrade <- !is.na(pl_frame$TradeID)
  pl_frame <- unique(pl_frame[c('Instrument','TradeDate','TodayPL','Visit','IsTrade')])
  first <- TRUE
  for(r in 1:(max(times_flat$TimesFlat)+1)){
    psns <- times_flat[times_flat$TimesFlat==(r-1),c('Instrument','TimesFlat')]
    bucket_data <- merge(pl_frame,psns,by=('Instrument'))
    if(nrow(bucket_data)>0){
      bucket_data <- aggregate(bucket_data[c('TodayPL','IsTrade')],list(Visit=bucket_data$Visit),fn)
      if(first){
        all_bucket_data <- cbind(TotalVisits=r,bucket_data)
        first <- FALSE
      }
      else{
        all_bucket_data <- rbind(all_bucket_data,cbind(TotalVisits=r,bucket_data))
      } 
    }
  }
  return(all_bucket_data)
}

trade_stats_in <- function(trade_grp_1,trade_grp_2){
  skew_in <- plot_ly(y=trade_grp_1$SkewInto,type="box")
  skew_in <- add_trace(skew_in,y=trade_grp_2$SkewInto,type='box')
  skew_in <- layout(title="Into trade",yaxis=list(title="Skew"),xaxis=list(title=""))
  
  vol_in <- plot_ly(y=trade_grp_1$VolInto,type="box")
  vol_in <- add_trace(vol_in,y=trade_grp_2$VolInto,type='box')
  vol_in <- layout(yaxis=list(title="Vol"),xaxis=list(title=""))
  
  return(subplot(vol_in,skew_in,nrows=2))
}

trade_delta_stats <- function(trade_grp_1,trade_grp_2){
  d_pl <- plot_ly(y=trade_grp_1$DeltaPL,type="box")
  d_pl <- add_trace(d_pl,y=trade_grp_2$DeltaPL,type='box')
  d_pl <- layout(title="Delta vs underlyer",yaxis=list(title="Delta PL"),xaxis=list(title=""))
  d_sw <- plot_ly(y=trade_grp_1$DeltaSwing,type="box")
  d_sw <- add_trace(d_sw,y=trade_grp_2$DeltaSwing,type='box')
  d_sw <- layout(yaxis=list(title="Delta Swing"),xaxis=list(title=""))
  d_sk <- plot_ly(y=trade_grp_1$DeltaSkew,type="box")
  d_sk <- add_trace(d_sk,y=trade_grp_2$DeltaSkew,type='box')
  d_sk <- layout(yaxis=list(title="Delta Skew"),xaxis=list(title=""))
  return(subplot(d_pl,d_sw,d_sk,nrows=3))
}

rank_offside <- function(trades,smooth=TRUE){
  trades$FractionOff <- 100*(trades$CumulativePL/abs(trades$MarketValue))
  trades <- trades[order(trades$FractionOff),]
  adown_fraction <- plot_ly(y=trades$FractionOff,type = "scatter", mode = "markers+lines",name="Offside")
  adown_fraction <- layout(adown_fraction,yaxis=list(title="Offside (%)"),xaxis=list(title=""))
  adown_value <- plot_ly(y=abs(trades$Av.MarketValue),type = "scatter", mode = "markers",name="MarketValue")
  adown_value <- layout(adown_value,yaxis=list(title="Av. Value"),xaxis=list(title="Rank by offside"))
  if(smooth){
    adown_ave <- loess(Av.MarketValue~index,data=cbind(trades,index=(1:nrow(trades))),span=3)
    adown_value <- add_trace(adown_value,y=fitted(adown_ave),mode="lines",name="Average")  
  }
  return(list(subplot(adown_fraction,adown_value,nrows=2),trades))
}

ignorena_cumsum <- function(x){
  x[is.na(x)] <- 0
  cumsum(x)
}

group_by_trade_counts_stats <- function(history_data,group_trades,instruments,cols,stat_primary,fn_primary,plot_cum_primary,stat_secondary,fn_secondary,plot_cum_secondary){
  all_instruments <- unique(history_data$Instrument)
  pl_frame <- merge(history_data,data.frame(Instrument=instruments),by=c('Instrument'))
  pl_frame <- unique(pl_frame[c(cols)])
  pl_frame$TradeCount <- NA
  pl_frame$PsnAge <- NA
  for(ins in instruments){
    cnt <- sum(group_trades$Instrument==ins)
    pl_frame$TradeCount[pl_frame$Instrument==ins] <- cnt
    pl_frame$PsnAge[pl_frame$Instrument==ins] <- as.numeric(pl_frame$TradeDate[pl_frame$Instrument==ins] - min(pl_frame$TradeDate[pl_frame$Instrument==ins],na.rm=TRUE))
  }
  
  other_frame <- unique(history_data[c(cols)])
  other_frame$PsnAge <- NA
  for(ins in setdiff(all_instruments,instruments)){
    other_frame$PsnAge[other_frame$Instrument==ins] <- as.numeric(other_frame$TradeDate[other_frame$Instrument==ins] - min(other_frame$TradeDate[other_frame$Instrument==ins],na.rm=TRUE))
  }
  
  agg_primary   <- aggregate(pl_frame[stat_primary],list(Age=pl_frame$PsnAge,TradeCount=pl_frame$TradeCount),fn_primary)
  agg_secondary <- aggregate(pl_frame[stat_secondary],list(Age=pl_frame$PsnAge,TradeCount=pl_frame$TradeCount),fn_secondary)
  cum_primary   <- data.frame(Age=agg_primary$Age,TradeCount=agg_primary$TradeCount,primary=ignorena_cumsum(agg_primary[stat_primary]))
  colnames(cum_primary)[colnames(cum_primary)=='primary'] <- stat_primary
  cum_secondary <- data.frame(Age=agg_secondary$Age,TradeCount=agg_secondary$TradeCount,secondary=ignorena_cumsum(agg_secondary[stat_secondary]))
  colnames(cum_secondary)[colnames(cum_secondary)=='secondary'] <- stat_secondary
  
  other_primary      <- aggregate(other_frame[stat_primary],list(Age=other_frame$PsnAge),fn_primary)
  other_secondary    <- aggregate(other_frame[stat_secondary],list(Age=other_frame$PsnAge),fn_secondary)
  other_cum_primary  <- data.frame(Age=other_primary$Age,other_primary=ignorena_cumsum(other_primary[stat_primary]))
  colnames(other_cum_primary)[colnames(other_cum_primary)=='other_primary'] <- stat_primary
  other_cum_secondary<- data.frame(Age=other_secondary$Age,other_secondary=ignorena_cumsum(other_secondary[stat_secondary]))
  colnames(other_cum_secondary)[colnames(other_cum_secondary)=='other_secondary'] <- stat_secondary
  
  if(plot_cum_primary){
    primary_frame <- cum_primary
    other_primary_frame <- other_cum_primary
  }
  else{
    primary_frame <- agg_primary
    other_primary_frame <- other_primary
  }
  if(plot_cum_secondary){
    secondary_frame <- cum_secondary
    other_secondary_frame <- other_cum_secondary
  }
  else{
    secondary_frame <- agg_secondary
    other_secondary_frame <- other_secondary
  }
  
  primary_plot <- plot_ly(primary_frame[primary_frame$TradeCount==1,], x = Age, y = get(stat_primary),name="1")
  mavdown <- max(primary_frame$TradeCount,na.rm=TRUE)
  for(a in 2:mavdown){
    primary_plot <- add_trace(primary_frame[primary_frame$TradeCount==a,], x = Age, y = get(stat_primary),name=as.character(a))
  }
  primary_plot <- add_trace(other_primary_frame, x = Age, y = get(stat_primary),name="Overall")
  primary_plot <- layout(yaxis=list(title=stat_primary),xaxis=list(title=""))
  
  secondary_plot <- plot_ly(secondary_frame[secondary_frame$TradeCount==1,], x = Age, y = get(stat_secondary),name="1")
  for(a in 2:mavdown){
    secondary_plot <- add_trace(secondary_frame[secondary_frame$TradeCount==a,], x = Age, y = get(stat_secondary),name=as.character(a))
  }
  secondary_plot <- add_trace(other_secondary_frame, x = Age, y = get(stat_secondary),name="Overall")
  secondary_plot <- layout(yaxis=list(title=stat_secondary),xaxis=list(title="Age"))
  
  return(subplot(primary_plot,secondary_plot,nrows=2))
}

group_by_trade_counts_cumulative_pl <- function(history_data,group_trades,instruments,cols){
  return(group_by_trade_counts_stats(history_data,group_trades,instruments,cols,'TodayPL',function(x)mean(x,na.rm=TRUE),TRUE,'MarketValue',function(x)mean(abs(x[x!=0]),na.rm=TRUE),FALSE))
}

position_activity <- function(position_history,tau){
  position_history$ValueUSD[is.na(position_history$ValueUSD)] <- 0
  instruments <- unique(position_history$Instrument)
  first <- TRUE
  for(ins in instruments){
    ph <- position_history[position_history$Instrument==ins,]
    ph <- ph[order(ph$TradeDate),]
    act <- abs(c(ph$ValueUSD[1]))
    if(length(ph$ValueUSD)>1){
      for(i in 1:(length(ph$ValueUSD)-1)){
        act[i+1] <- act[i] + abs(ph$ValueUSD[i+1]) - (1/tau)*act[i]  
      }  
    }
    else{
      act <- abs(c(ph$ValueUSD[1]))
    }
    ph$Activity <- act
    if(first){
      new_history <- ph  
      first <- FALSE
    }
    else{
      new_history <- rbind(new_history,ph)
    }
  }
  return(new_history)
}

pl_timescales <- function(history_data,data=FALSE){
  psn_hstry <- categorise_psn_ages(history_data)
  position_history <- psn_hstry[[1]]
  min_dates <- psn_hstry[[2]]
  position_history <- market_rel_pl(position_history)
  
  pl_frame <- position_history[position_history$PsnAgeCategory>0,]
  pl_frame <- unique(pl_frame[c('Instrument','TradeDate','TodayPL','PsnAgeCategory','ActiveTodayPL','PassiveTodayPL','PsnAge')])
  
  cum_pl <- aggregate(pl_frame['TodayPL'],list(Age=pl_frame$PsnAge),function(x)sum(x,na.rm=TRUE))
  cumdata <- data.frame(Age=cum_pl$Age,TotalPL=cumsum(cum_pl$TodayPL))
  cum_pnl_age <- plot_ly(cumdata, x = Age, y = TotalPL)
  
  cum_act_pl <- aggregate(pl_frame['ActiveTodayPL'],list(Age=pl_frame$PsnAge),function(x)sum(x,na.rm=TRUE))
  act_cumdata <- data.frame(Age=cum_act_pl$Age,TotalPL=cumsum(cum_act_pl$ActiveTodayPL))
  act_cum_pnl_age <- plot_ly(act_cumdata, x = Age, y = TotalPL)
  
  pass_cumdata <- cumdata
  pass_cumdata$TotalPL <- pass_cumdata$TotalPL - act_cumdata$TotalPL
  act_cum_pnl_age <- add_trace(pass_cumdata, x = Age, y = TotalPL)
  
  position_history <- position_activity(position_history,median(unique(history_data$Age),na.rm=TRUE)/10)
  activity_frame <- position_history[position_history$PsnAgeCategory>0,]
  activity_frame <- unique(activity_frame[c('Instrument','TradeDate','TodayPL','PsnAgeCategory','Activity','PsnAge')])
  activity_frame <- aggregate(activity_frame['Activity'],list(Age=activity_frame$PsnAge),function(x)mean(x,na.rm=TRUE))
  activity <- plot_ly(activity_frame, x = Age, y = Activity)
  
  weight_active <- activity_frame$Activity/max(activity_frame$Activity,na.rm=TRUE)
  weight_passive<- 1-weight_active
  weighted_active <- act_cumdata
  weighted_passive <- pass_cumdata
  weighted_active$TotalPL <- weight_active*(act_cumdata$TotalPL+pass_cumdata$TotalPL)
  weighted_passive$TotalPL <- weight_passive*(pass_cumdata$TotalPL+act_cumdata$TotalPL)
  activity_blend <- plot_ly(weighted_passive, x = Age, y = TotalPL)
  activity_blend <- add_trace(weighted_active, x = Age, y = TotalPL)
  
  pnl_cum <- subplot(cum_pnl_age,act_cum_pnl_age,activity,activity_blend,nrows=4)
  if(data){
    rval <- list(pnl_cum,cumdata,act_cumdata,pass_cumdata,activity_frame,weighted_passive,weighted_active,position_history)
  }
  else{
    rval <- pnl_cum
  }
  return(rval)
}

attribution_categories <- function(data){
  attrib_cols <- c('TradeID','Instrument','TradeDate','Name','Activity','TodayPL','ActiveTodayPL','PassiveTodayPL','CumulativePL','IntegratedPL')
  attrib_data <- unique(data[attrib_cols])
  instruments <- unique(data$Instrument)
  
  first <- TRUE
  for(ins in instruments){
    a_data <- attrib_data[attrib_data$Instrument==ins,]
    a_data <- a_data[order(a_data$TradeDate),]
    a_data$Traded <- a_data$Activity > (a_data$Activity[1]/10)
    a_data$ActiveTodayPL[is.na(a_data$ActiveTodayPL)]<-0
    a_data$Active <- c(a_data$ActiveTodayPL[1]!=0,diff(a_data$ActiveTodayPL,na.rm=TRUE)!=0)
    a_data$Active[is.na(a_data$Active)] <- FALSE
    if(first){
      categorised <- a_data
      first <- FALSE
    }
    else{
      categorised <- rbind(categorised,a_data)
    }
  }
  return(categorised)
}

position_category_breakdown <- function(categorised){
  traded_active_gains <- sum(categorised[categorised$Traded==TRUE&categorised$Active==TRUE&categorised$TodayPL>0,'TodayPL'],na.rm=TRUE)
  traded_active_losses <- sum(categorised[categorised$Traded==TRUE&categorised$Active==TRUE&categorised$TodayPL<0,'TodayPL'],na.rm=TRUE)
  
  traded_passive_gains <- sum(categorised[categorised$Traded==TRUE&categorised$Active==FALSE&categorised$TodayPL>0,'TodayPL'],na.rm=TRUE)
  traded_passive_losses <- sum(categorised[categorised$Traded==TRUE&categorised$Active==FALSE&categorised$TodayPL<0,'TodayPL'],na.rm=TRUE)
  
  untraded_active_gains <- sum(categorised[categorised$Traded==FALSE&categorised$Active==TRUE&categorised$TodayPL>0,'TodayPL'],na.rm=TRUE)
  untraded_active_losses <- sum(categorised[categorised$Traded==FALSE&categorised$Active==TRUE&categorised$TodayPL<0,'TodayPL'],na.rm=TRUE)
  
  untraded_passive_gains <- sum(categorised[categorised$Traded==FALSE&categorised$Active==FALSE&categorised$TodayPL>0,'TodayPL'],na.rm=TRUE)
  untraded_passive_losses <- sum(categorised[categorised$Traded==FALSE&categorised$Active==FALSE&categorised$TodayPL<0,'TodayPL'],na.rm=TRUE)
  
  nms <- c('TradedPassive','TradedActive','UntradedActive','UntradedPassive')
  totals <- plot_ly(x=nms,y=c(traded_passive_gains,traded_active_gains,untraded_active_gains,untraded_passive_gains),type="bar")
  totals <- add_trace(totals,x=nms,y=c(traded_passive_losses,traded_active_losses,untraded_active_losses,untraded_passive_losses),type="bar")
  totals <- layout(totals,yaxis=list(title="Gain/Loss"),xaxis=list(title=""))
  
  traded_active_stock <- aggregate(categorised['TodayPL'],list(Instrument=categorised$Instrument,Name=categorised$Name,Traded=categorised$Traded,Active=categorised$Active),function(x)sum(x,na.rm=TRUE))
  
  top_traded_active_stock <- traded_active_stock[traded_active_stock$Traded==TRUE&traded_active_stock$Active==TRUE,]
  top_traded_active_stock <- top_traded_active_stock[order(top_traded_active_stock$TodayPL),]
  
  top_traded_passive_stock <- traded_active_stock[traded_active_stock$Traded==TRUE&traded_active_stock$Active==FALSE,]
  top_traded_passive_stock <- top_traded_passive_stock[order(top_traded_passive_stock$TodayPL),]
  
  top_untraded_active_stock <- traded_active_stock[traded_active_stock$Traded==FALSE&traded_active_stock$Active==TRUE,]
  top_untraded_active_stock <- top_untraded_active_stock[order(top_untraded_active_stock$TodayPL),]
  
  top_untraded_passive_stock <- traded_active_stock[traded_active_stock$Traded==FALSE&traded_active_stock$Active==FALSE,]
  top_untraded_passive_stock <- top_untraded_passive_stock[order(top_untraded_passive_stock$TodayPL),]
  
  sublist <- list(top_traded_active_stock,top_traded_passive_stock,top_untraded_active_stock,top_untraded_passive_stock)
  names(sublist) <- nms
  return(list(totals,sublist))
}

compute_stock_rank_cor <- function(psn_cat_data){
  
  top_traded_active_stock <- psn_cat_data[[1]]
  top_traded_passive_stock <- psn_cat_data[[2]]
  top_untraded_active_stock <- psn_cat_data[[3]]
  top_untraded_passive_stock <- psn_cat_data[[4]]
  nms <- names(psn_cat_data)
  
  rank_cor_mat <- matrix(rep(NA,16),nrow=4,ncol=4)
  colnames(rank_cor_mat) <- nms
  rownames(rank_cor_mat) <- nms
  
  traded_passive_active <- merge(top_traded_active_stock,top_traded_passive_stock,by='Instrument')
  rank_cor_mat[[2,1]] <- tryCatch({
                                    cor.test(~  TodayPL.x +  TodayPL.y, data = traded_passive_active,method='spearman',alternative=c("two.sided"))[[4]]
                                  },error=function(cond){
                                    message(cond)
                                    return(NA)
                                  })
  traded_passive_untraded_active <- merge(top_untraded_active_stock,top_traded_passive_stock,by='Instrument')
  rank_cor_mat[[3,1]] <- tryCatch({
                                    cor.test(~  TodayPL.x +  TodayPL.y, data = traded_passive_untraded_active,method='spearman',alternative=c("two.sided"))[[4]]
                                  },error=function(cond){
                                    message(cond)
                                    return(NA)
                                  })
  passive_traded_untraded <- merge(top_untraded_passive_stock,top_traded_passive_stock,by='Instrument')
  rank_cor_mat[[4,1]] <- tryCatch({
                                   cor.test(~  TodayPL.x +  TodayPL.y, data = passive_traded_untraded,method='spearman',alternative=c("two.sided"))[[4]]
                                  },error=function(cond){
                                   message(cond)
                                   return(NA)
                                  })
  active_traded_untraded <- merge(top_untraded_active_stock,top_traded_active_stock,by='Instrument')
  rank_cor_mat[[3,2]] <- tryCatch({
                                   cor.test(~  TodayPL.x +  TodayPL.y, data = active_traded_untraded,method='spearman',alternative=c("two.sided"))[[4]]
                                  },error=function(cond){
                                   message(cond)
                                   return(NA)
                                  })
  traded_active_untraded_passive <- merge(top_untraded_passive_stock,top_traded_active_stock,by='Instrument')
  rank_cor_mat[[4,2]] <- tryCatch({
                                   cor.test(~  TodayPL.x +  TodayPL.y, data = traded_active_untraded_passive,method='spearman',alternative=c("two.sided"))[[4]]
                                  },error=function(cond){
                                   message(cond)
                                   return(NA)
                                  })
  untraded_active_passive <- merge(top_untraded_passive_stock,top_untraded_active_stock,by='Instrument')
  rank_cor_mat[[4,3]] <- tryCatch({
                                   cor.test(~  TodayPL.x +  TodayPL.y, data = untraded_active_passive,method='spearman',alternative=c("two.sided"))[[4]]
                                  },error=function(cond){
                                   message(cond)
                                   return(NA)
                                  })
  sublist <- list(traded_passive_active,traded_passive_untraded_active,passive_traded_untraded,active_traded_untraded,traded_active_untraded_passive,untraded_active_passive)
  names(sublist) <- c(paste(nms[1],nms[2],sep=""),paste(nms[1],nms[3],sep=""),paste(nms[1],nms[4],sep=""),paste(nms[2],nms[3],sep=""),paste(nms[2],nms[4],sep=""),paste(nms[3],nms[4],sep=""))
  return(list(rank_cor_mat,sublist))
}

new_psns <- function(psn_data){
  psn_data$PsnAge <- as.numeric(psn_data$TradeDate-psn_data$MinDate)
  psn_data_ins <- unique(psn_data[psn_data$Age==0,]$Instrument)
  psn_data <- psn_data[psn_data$Instrument%in%psn_data_ins,]
  psn_data <- psn_data[psn_data$PsnAge>-1,]
  return(psn_data)
}

mda_kernel <- function(x,y){
  ds <- seq(ymd(x),ymd(y),by='1 day')
  dow_frame <- data.frame(TradeDate=ds,DOW=wday(ds))
  dow_frame$NotMarketDay <- dow_frame$DOW==1|dow_frame$DOW==7
  return(sum(dow_frame$NotMarketDay,na.rm=TRUE))
}
market_day_age <- function(data){
  out_data <- data
  out_data$Nmd <- unlist(Map(mda_kernel,data$MinDate,data$TradeDate))
  out_data$PsnAge <- out_data$TradeDate - out_data$MinDate - out_data$Nmd
  return(out_data[setdiff(colnames(out_data),'Nmd')])
}

remove_nan <- function(data){
  data$TodayPL[is.nan(data$TodayPL)] <- NA
  data$TodayPL[is.infinite(data$TodayPL)] <- NA
  data$PassiveTodayPL[is.nan(data$PassiveTodayPL)] <- NA
  data$PassiveTodayPL[is.infinite(data$PassiveTodayPL)] <- NA
  data$ActiveTodayPL[is.nan(data$ActiveTodayPL)] <- NA
  data$ActiveTodayPL[is.infinite(data$ActiveTodayPL)] <- NA
  data$MarketRelPL[is.nan(data$MarketRelPL)] <- NA
  data$MarketRelPL[is.infinite(data$MarketRelPL)] <- NA
  return(data)
}

position_age_from_flats <- function(history_data,return_cols,abslimit=-0.1,rellimit=-0.1){
    message("Computing postion age relative to flats...")
    first <- TRUE
    history_data$PsnAge <- NA
    history_data$VisitCumulativePL <- NA
    history_data$VisitCumulativeMarketRelPL <- NA
    history_data$VisitCumulativeActivePL <- NA
    history_data$VisitCumulativePassivePL <- NA
    cols <- unique(c(return_cols,'PsnAge','VisitCumulativePL','VisitCumulativeActivePL','VisitCumulativePassivePL','CumulativePL','VisitCumulativeMarketRelPL','CumulativeMarketRelPL','MarketValue','TradeID','MinDate','Strategy','TradeDate','Instrument','EarliestMarketValue'))
    traders <- unique(history_data$Trader)
    for(t in traders){
      trader_strategies <- unique(history_data[history_data$Trader==t,]$Strategy)
      for(strat in trader_strategies){
        trader_instruments <- unique(history_data[history_data$Trader==t&history_data$Strategy==strat,]$Instrument)
        for(ins in trader_instruments){
          hd <- unique(history_data[history_data$Trader==t&history_data$Instrument==ins&history_data$Strategy==strat,cols])
          hd$DaysOffAbs <- NA
          hd$DaysOffRel <- NA
          hd$TradeCount <- NA
          min_date <- unique(hd$MinDate)
          fd <- count_flats(hd)[[1]]
          hd <- create_revisit_data(fd,hd)
          hd$TradeCount[!is.na(hd$TradeID)] <- 1:sum(!is.na(hd$TradeID))
          hd <- hd[order(hd$TradeDate),]
          if(!is.na(fd$FlatDate)){
            for(f in fd['FlatDate']){
              flat_date <- as.Date(f[[1]])
              hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$VisitCumulativePL <- cumsum(hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$TodayPL)  
              hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$VisitCumulativeActivePL <- cumsum(hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$ActiveTodayPL)  
              hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$VisitCumulativePassivePL <- cumsum(hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$PassiveTodayPL)  
              hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$VisitCumulativeMarketRelPL <- cumsum(hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$MarketRelPL)
              nmd <- unlist(Map(function(x,md=min_date)mda_kernel(md,x),hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$TradeDate))
              hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$PsnAge <- hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$TradeDate - min_date - nmd
              if(nrow(hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,])>1){
                hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$EarliestMarketValue <- hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$MarketValue[2]  
              }
              abs <- (hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$VisitCumulativePL/abs(hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$EarliestMarketValue))<abslimit
              abs[is.na(abs)] <- 0
              rel <- (hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$VisitCumulativeMarketRelPL/abs(hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$EarliestMarketValue))<rellimit
              rel[is.na(rel)] <- 0
              hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$DaysOffAbs <- cumsum(abs)-1
              hd[hd$TradeDate>=min_date&hd$TradeDate<flat_date,]$DaysOffRel <- cumsum(rel)-1
              min_date <- flat_date
            }
            hd[hd$TradeDate>=min_date,]$VisitCumulativePL <- cumsum(hd[hd$TradeDate>=min_date,]$TodayPL)  
            hd[hd$TradeDate>=min_date,]$VisitCumulativeActivePL <- cumsum(hd[hd$TradeDate>=min_date,]$ActiveTodayPL)  
            hd[hd$TradeDate>=min_date,]$VisitCumulativePassivePL <- cumsum(hd[hd$TradeDate>=min_date,]$PassiveTodayPL)  
            hd[hd$TradeDate>=min_date,]$VisitCumulativeMarketRelPL <- cumsum(hd[hd$TradeDate>=min_date,]$MarketRelPL)
            nmd <- unlist(Map(function(x,md=min_date)mda_kernel(md,x),hd[hd$TradeDate>=min_date,]$TradeDate))
            hd[hd$TradeDate>=min_date,]$PsnAge <- hd[hd$TradeDate>=min_date,]$TradeDate - min_date - nmd
            if(nrow(hd[hd$TradeDate>=min_date,])>1){
              hd[hd$TradeDate>=min_date,]$EarliestMarketValue <- hd[hd$TradeDate>=min_date,]$MarketValue[2]  
              hd[hd$TradeDate>=min_date,]$EarliestMarketValue[is.na(hd[hd$TradeDate>=min_date,]$EarliestMarketValue)] <- 0
            }
            abs <- (hd[hd$TradeDate>=min_date,]$VisitCumulativePL/abs(hd[hd$TradeDate>=min_date,]$EarliestMarketValue))<abslimit
            abs[is.na(abs)] <- 0
            rel <- (hd[hd$TradeDate>=min_date,]$VisitCumulativeMarketRelPL/abs(hd[hd$TradeDate>=min_date,]$EarliestMarketValue))<rellimit
            rel[is.na(rel)] <- 0
            hd[hd$TradeDate>=min_date,]$DaysOffAbs <- cumsum(abs)-1
            hd[hd$TradeDate>=min_date,]$DaysOffRel <- cumsum(rel)-1
          } else {
            nmd <- unlist(Map(function(x,md=min_date)mda_kernel(md,x),hd$TradeDate))
            hd$PsnAge <- hd$TradeDate - min_date -nmd
            abs <- (hd$VisitCumulativePL/abs(hd$EarliestMarketValue))<abslimit
            abs[is.na(abs)] <- 0
            
            rel <- (hd$VisitCumulativeMarketRelPL/abs(hd$EarliestMarketValue))<rellimit
            rel[is.na(rel)] <- 0
            hd$DaysOffAbs <- cumsum(abs)
            hd$DaysOffRel <- cumsum(rel)
          }
          if(first){
            all_hd <- hd
            first <- FALSE
          } else {
            all_hd <- rbind(all_hd,hd)
          }
        }
      }
    }
    all_hd$DaysOffAbs[all_hd$DaysOffAbs==-1] <- 0 
  all_hd$DaysOffRel[all_hd$DaysOffRel==-1] <- 0 
  return(all_hd)
}

load_and_compute_market_rel <- function(lookback_function,traders,dates){
    first <- TRUE
    for(t in traders){
      kf <- function()lookback_function(t,dates[1])
      trader_data <- tryCatch({
        analysis_module_load_multiple(t,dates,history_analysis_module_builder,lookback_function)
      }, error = function(cond){message(paste("Load error on",t,":",cond))})
      if(length(trader_data)>0){
        if(first){
          history_data <- cbind(Trader=t,trader_data)    
          first <- FALSE
        }
        else{
          history_data <- rbind.fill(history_data,cbind(Trader=t,trader_data))    
        } 
      } else {
        message(paste("Load error on",t,": No rows."))
      }
    }
    history_data <- history_data[!duplicated(history_data[c('Trader','Instrument','TradeDate','Strategy','MarketValue','MidOnEntry','ValueUSD')]),]
    history_data <- market_rel_pl(history_data)
    return(history_data)
}

trade_typer <- function(history_data){
  history_data$TradeType <- 'NA'
  history_data$TradeType[history_data$Long==1&history_data$MarketValue>0&history_data$PsnAge!=0] <- 'Add Long'
  history_data$TradeType[(history_data$Long==0)&history_data$MarketValue>0&history_data$PsnAge!=0] <- 'Reduce Long'
  history_data$TradeType[history_data$Long==1&history_data$MarketValue<0&history_data$PsnAge!=0] <- 'Reduce Short'
  history_data$TradeType[(history_data$Long==0)&history_data$MarketValue<0&history_data$PsnAge!=0] <- 'Add Short'
  history_data$TradeType[grepl('._L.',history_data$Strategy)&history_data$PsnAge==0] <- 'New Long'
  history_data$TradeType[grepl('._S.',history_data$Strategy)&history_data$PsnAge==0] <- 'New Short'
  psn_increased <- aggregate(history_data$TradeType,list(Strategy=history_data$Strategy,Visit=history_data$Visit,Instrument=history_data$Instrument),function(x)sum(x=='Increase',na.rm=TRUE)>0)
  colnames(psn_increased) <- c('Strategy','Visit','Instrument','PsnIncreased')
  history_data <- merge(history_data,psn_increased,by=c('Strategy','Visit','Instrument'),all.x=TRUE)
  return(history_data)
}

switch_direction <- function(history_data,column){
  history_data[column] <- -1^(1+history_data$PsnLong)*history_data[column]
  return(history_data)
}

#integrate this with the version in the preprocessor functions file
data_fractile <- function(data,on,ntiles,name='Quartile',group_on=NULL){
  cnames <- colnames(data)
  data$ntile <- with(data[on], cut(as.numeric(unlist(data[on])), breaks=unique(quantile(data[on], probs=seq(0,1, by=1/ntiles), na.rm=TRUE)), include.lowest=TRUE))
  ud <- sort(unique(data$ntile))
  colnames(data) <- c(cnames,paste(on,name,sep=""))  
  data[paste(on,name,"_N",sep="")] <- unlist(Map(function(x)which(x==ud),data[[paste(on,name,sep="")]]))
  if(length(group_on)!=0){
    group_expand <- unique(expand.grid(unique(data[group_on])))
    cnames <- colnames(data)
    first <- TRUE
    for(g in 1:nrow(group_expand)){
      grp <- group_expand[g,]
      df <- data[unlist(Map(function(z)Reduce(function(x,y)x&&y,data[z,group_on]==grp),1:nrow(data))),]
      if(nrow(df)>0){
        df$grp_ntile <- with(df[on], cut(as.numeric(unlist(df[on])), breaks=unique(quantile(df[on], probs=seq(0,1, by=1/ntiles), na.rm=TRUE)), include.lowest=TRUE))
        ud <- sort(unique(df$grp_ntile))
        colnames(df) <- c(cnames,paste(paste(group_on,collapse=""),"Group",name,sep=""))
        df[paste(paste(group_on,collapse=""),"Group",name,"_N",sep="")] <- unlist(Map(function(x)which(x==ud),df[[paste(paste(group_on,collapse=""),"Group",name,sep="")]]))
        if(first){
          fct_data <- df
          first <- FALSE
        } else {
          fct_data <- rbind(fct_data,df)
        } 
      }
    }
  } else {
    fct_data <- data
  }
  return(fct_data)
}

scale_and_clip <- function(data,bound=5){
  na_idx <- is.na(data) 
  data[na_idx] <- mean(data,na.rm=TRUE)
  data <- as.numeric(scale(data))
  data[abs(data)>bound] <- NA
  data[na_idx] <- NA
  return(data)
}
