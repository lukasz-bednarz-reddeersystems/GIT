setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules/analysis_module_snapshot_resultsday.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
run_module <- resultsday_snapshot_analysis_module_builder
exp_module <- resultsday_exposure_snapshot_analysis_module_builder

trader   <- 70
key_func <- function(){dated_three_monthly_lookback(trader,"2016-01-01")}

#Get data
earnings <- createAnalysisModule(run_module,key_func)
earnings <- updateAnalysisModel(earnings)
earnings <- runAnalysisModule(earnings)

exposure <- createAnalysisModule(exp_module,key_func)
exposure <- updateAnalysisModel(exposure)
exposure <- runAnalysisModule(exposure)

#Compute values for property histogram
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

#Compute trade bucket properties
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

format_labels <- function(values){
  labels <- c()
  labels <- c(labels,sprintf("%d bps",round(10000*values[1])))
  labels <- c(labels,sprintf("$%d",round(values[2])))
  labels <- c(labels,sprintf("%.2f",values[3]))
  labels <- c(labels,sprintf("$%d",round(values[4])))
  labels <- c(labels,sprintf("$%d",round(values[5])))
  labels <- c(labels,sprintf("%.2f",values[6]))
  return(labels)
}

#Before/After aggregate statistics 
p <- 1
psn_long <- TRUE
buy <- TRUE

plot_data <- compute_buckets(earnings@ppmdl@modeldata@data,psn_long,buy,FALSE)
in_vars <- plot_data[[1]]
out_vars<- plot_data[[2]]
on_trade<- plot_data[[3]]

in_vars[is.nan(in_vars)] <- NA

out_vars[is.nan(out_vars)] <- NA
nmes <- c("Vol","Swing","Skew","+tvePL","-tvePL","Advance")

normed_data_in <- norm_data(in_vars,out_vars,nmes)
normed_data_out <- norm_data(out_vars,in_vars,nmes)

m <- rbind(c(1, 2),c(3, 3),c(4, 4))
layout(m)
midpoints <- barplot(normed_data_in,ylim=c(0,1.5),axes=FALSE,las=2,font=2,main="Pre-trade")
text(midpoints, normed_data_in+0.15, labels=format_labels(in_vars), srt=90,font=2)
midpoints <- barplot(normed_data_out,ylim=c(0,1.5),axes=FALSE,las=2,font=2,main="Post-trade")
text(midpoints, normed_data_out+0.15, labels=format_labels(out_vars), srt=90, font=2)
earnings@panels[[p]]@visualns[[1]]@visuln_dspl(earnings@panels[[p]]@visualns[[1]])
text(1, -0.05, labels=sprintf("%.2f%% hit rate",on_trade[1]*100))
text(1, -0.075, labels=sprintf("%.2f win loss",on_trade[4]))
text(1, -0.1, labels=sprintf("%.2f extraction",on_trade[5]))
exposure@panels[[p]]@visualns[[1]]@visuln_dspl(exposure@panels[[p]]@visualns[[1]])











