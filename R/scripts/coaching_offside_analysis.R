setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules/analysis_module_position_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("coaching_review_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("panel_components.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("panel_themes.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(plotly)

trader   <- 70
dates <- c("2016-01-01")
history_data <- analysis_module_load_multiple(trader,dates,history_analysis_module_builder,dated_three_monthly_lookback)
history_data <- market_rel_pl(history_data,trade_rel=FALSE)
instruments <- unique(history_data$Instrument)
all_trades <- history_data[!is.na(history_data$TradeID),]
all_intg_offside_data <- integrated_offside(history_data)
all_cuml_offside_data <- integrated_offside(history_data,type="CumulativePL")

#Offside positions ------------------------------------------------------------------
trade_cols <- c('TradeDate','TradeID','Name','Strategy','MarketValue','SkewInto','ValueUSD','VolInto','DeltaPL','DeltaSwing','DeltaSkew','PnLOutof','TodayPL','Age','Long')
cuml_offside_data <- new_psns(all_cuml_offside_data[[1]])
cuml_onside_data <- new_psns(all_cuml_offside_data[[2]])
cuml_trades_in_offside_psns <- cuml_offside_data[!is.na(cuml_offside_data$TradeID)&cuml_offside_data$CumulativePL<0,]
cuml_trades_in_offside_psns <- unique(cuml_trades_in_offside_psns[trade_cols])
cuml_trades_in_onside_psns  <- cuml_onside_data[!is.na(cuml_onside_data$TradeID)&cuml_onside_data$CumulativePL>=0,]
cuml_trades_in_onside_psns  <- unique(cuml_trades_in_onside_psns[trade_cols])

#1. number positions/trades offside in period, cumulative
cuml_instruments_overall_offside <- all_cuml_offside_data[[3]]
cuml_trades_on <- nrow(unique(cuml_trades_in_onside_psns[c('TradeID','TradeDate','MarketValue')]))
cuml_trades_off<- nrow(unique(cuml_trades_in_offside_psns[c('TradeID','TradeDate','MarketValue')]))

#2. number positions/trades offside in period, integrated
intg_offside_data <- new_psns(all_intg_offside_data[[1]])
intg_onside_data <- new_psns(all_intg_offside_data[[2]])
intg_trades_in_offside_psns <- intg_offside_data[!is.na(intg_offside_data$TradeID)&intg_offside_data$CumulativePL<0,]
intg_trades_in_offside_psns <- unique(intg_trades_in_offside_psns[trade_cols])
intg_trades_in_onside_psns  <- intg_onside_data[!is.na(intg_onside_data$TradeID)&intg_onside_data$CumulativePL>=0,]
intg_trades_in_onside_psns  <- unique(intg_trades_in_onside_psns[trade_cols])
intg_trades_on <- nrow(unique(intg_trades_in_onside_psns[c('TradeID','TradeDate','MarketValue')]))
intg_trades_off<- nrow(unique(intg_trades_in_offside_psns[c('TradeID','TradeDate','MarketValue')]))
intg_instruments_overall_offside <- all_intg_offside_data[[3]]

ptc <- plot_ly(x=c('Offside','Total'),y=c(length(cuml_instruments_overall_offside),length(instruments)),type="bar",name="Postions")
ptc <- add_trace(x=c('Offside','Onside'),y=c(cuml_trades_off,cuml_trades_on),type="bar",name="Trades")
ptc <- add_trace(x=c('Offside'),y=c(length(intg_instruments_overall_offside)),type="bar",name="Positions (I)")
ptc <- add_trace(x=c('Offside','Onside'),y=c(intg_trades_off,intg_trades_on),type="bar",name="Trades (I)")
ptc <- layout(title="Trades/positions offside",yaxis=list(title="Count"),xaxis=list(title=""))

#3. win/loss size for on/offside trades, cumulative
plc <- plot_ly(cuml_trades_in_offside_psns[cuml_trades_in_offside_psns$PnLOutof>0,],y=PnLOutof,type='box',name="offs. win")
plc <- add_trace(cuml_trades_in_onside_psns[cuml_trades_in_onside_psns$PnLOutof>0,],y=PnLOutof,type='box',name="ons. win")
plc <- add_trace(cuml_trades_in_offside_psns[cuml_trades_in_offside_psns$PnLOutof<0,],y=PnLOutof,type='box',name="offs. loose")
plc <- add_trace(cuml_trades_in_onside_psns[cuml_trades_in_onside_psns$PnLOutof<0,],y=PnLOutof,type='box',name="ons. loose")
plc <- layout(yaxis=list(title="PL"),xaxis=list(title=""))

#4. win/loss size for on/offside trades, integrated
pli <- plot_ly(intg_trades_in_offside_psns[intg_trades_in_offside_psns$PnLOutof>0,],y=PnLOutof,type='box',name="offs. win")
pli <- add_trace(intg_trades_in_onside_psns[intg_trades_in_onside_psns$PnLOutof>0,],y=PnLOutof,type='box',name="ons. win")
pli <- add_trace(intg_trades_in_offside_psns[intg_trades_in_offside_psns$PnLOutof<0,],y=PnLOutof,type='box',name="offs. loose")
pli <- add_trace(intg_trades_in_onside_psns[intg_trades_in_onside_psns$PnLOutof<0,],y=PnLOutof,type='box',name="ons. loose")
pli <- layout(yaxis=list(title="PL"),xaxis=list(title=""))

#5. total PL for onside/offside positions in period, cumulative and integrated
tplc <- plot_ly(x=c('Offside','Total'),y=c(sum(cuml_offside_data$TodayPL,na.rm=TRUE),sum(history_data$TodayPL,na.rm=TRUE)),type="bar",name="Cumulative")
tplc <- add_trace(x=c('Offside'),y=c(sum(intg_offside_data$TodayPL,na.rm=TRUE)),type="bar",name="Integrated")
tplc <- layout(yaxis=list(title="Total PL"),xaxis=list(title=""))
ttl_strat <- aggregate(intg_offside_data['TodayPL'],list(Strategy=intg_offside_data$Strategy),function(x)sum(x,na.rm=TRUE))
pli_strat <- plot_ly(ttl_strat,x=Strategy,y=TodayPL,type="bar")

#6. number of concurrently offside positions over the period
occrt_c <- aggregate(cuml_offside_data['CumulativePL'],list(Date=cuml_offside_data$TradeDate),function(x)sum(unique(x)<0))
colnames(occrt_c) <- c('Date','Offside')
occrt_i <- aggregate(cuml_offside_data['IntegratedPL'],list(Date=cuml_offside_data$TradeDate),function(x)sum(unique(x)<0))
colnames(occrt_i) <- c('Date','Offside')
all <- aggregate(history_data['Instrument'],list(Date=history_data$TradeDate),function(x)length(unique(x)))
colnames(all) <- c('Date','Total')
conc_off <- plot_ly(occrt_c,x=Date,y=Offside,name="Cumulative")
conc_off <- add_trace(occrt_i,x=Date,y=Offside,name="Integrated")
conc_off <- add_trace(all,x=Date,y=Total,name="Total")

#7. number positions that have been cumulative offside but not integrated offside
cum_not_int <- union(setdiff(cuml_instruments_overall_offside,intg_instruments_overall_offside),setdiff(intg_instruments_overall_offside,cuml_instruments_overall_offside))
cni_plot <- plot_ly(x=c("Cml!Int","Cml","Int"),y=c(length(cum_not_int),length(cuml_instruments_overall_offside),length(intg_instruments_overall_offside)),type="bar")
cni_plot <- layout(yaxis=list(title="Count"),xaxis=list(title=""))

#8. mean PL of the above 3 groups
cni_pl_plot <- plot_ly(cuml_offside_data[cuml_offside_data$Instrument%in%cum_not_int,],y=TodayPL,type="box",name="Cml!Int")
cni_pl_plot <- layout(yaxis=list(title="PL"),xaxis=list(title=""))
cni_pl_plot <- add_trace(cuml_onside_data,y=TodayPL,type="box",name='On')
cni_pl_plot <- add_trace(cuml_offside_data,y=TodayPL,type="box",name='Cml')
cni_pl_plot <- add_trace(intg_offside_data,y=TodayPL,type="box",name='Int')

cni_o_plot <- plot_ly(cuml_offside_data[cuml_offside_data$Instrument%in%cum_not_int,],y=PnLOutof,type="box",name="Cml!Int")
cni_o_plot <- layout(yaxis=list(title="Count"),xaxis=list(title=""))
cni_o_plot <- add_trace(cuml_onside_data,y=PnLOutof,type="box",name='On')
cni_o_plot <- add_trace(cuml_offside_data,y=PnLOutof,type="box",name='Cml')
cni_o_plot <- add_trace(intg_offside_data,y=PnLOutof,type="box",name='Int')

#9. Dollar swing and skew in offside positions
cuml_offside_data$DlrSwing <- (cuml_offside_data$VolInto/10000)*abs(cuml_offside_data$MarketValue)
cuml_offside_data$DlrSkew  <- ((-1)^(1+cuml_offside_data$PsnLong))*sign(cuml_offside_data$SkewInto)*((abs(cuml_offside_data$SkewInto))^1/3)*cuml_offside_data$DlrSwing
cuml_offside_data$FrcSkew  <- cuml_offside_data$DlrSkew/abs(cuml_offside_data$MarketValue)
intg_offside_data$DlrSwing <- (intg_offside_data$VolInto/10000)*abs(intg_offside_data$MarketValue)
intg_offside_data$DlrSkew  <- ((-1)^(1+intg_offside_data$PsnLong))*((-1)^(1+intg_offside_data$Long))*sign(intg_offside_data$SkewInto)*((abs(intg_offside_data$SkewInto))^1/3)*intg_offside_data$DlrSwing
intg_offside_data$FrcSkew  <- intg_offside_data$DlrSkew/abs(intg_offside_data$MarketValue)
cuml_onside_data$DlrSwing <- (cuml_onside_data$VolInto/10000)*abs(cuml_onside_data$MarketValue)
cuml_onside_data$DlrSkew  <- ((-1)^(1+cuml_onside_data$PsnLong))*sign(cuml_onside_data$SkewInto)*((abs(cuml_onside_data$SkewInto))^1/3)*cuml_onside_data$DlrSwing
cuml_onside_data$FrcSkew  <- cuml_onside_data$DlrSkew/abs(cuml_onside_data$MarketValue)

vol_plot <- plot_ly(cuml_offside_data[cuml_offside_data$Instrument%in%cum_not_int,],y=VolInto,type="box",name="Cml!Int")
vol_plot <- add_trace(cuml_onside_data,y=VolInto,type="box",name='On')
vol_plot <- add_trace(cuml_offside_data,y=VolInto,type="box",name='Cml')
vol_plot <- add_trace(intg_offside_data,y=VolInto,type="box",name='Int')

skw_plot <- plot_ly(cuml_offside_data[cuml_offside_data$Instrument%in%cum_not_int,],y=DlrSkew,type="box",name="Cml!Int")
skw_plot <- add_trace(cuml_onside_data,y=DlrSkew,type="box",name='On')
skw_plot <- add_trace(cuml_offside_data,y=DlrSkew,type="box",name='Cml')
skw_plot <- add_trace(intg_offside_data,y=DlrSkew,type="box",name='Int')

frc_skw_plot <- plot_ly(cuml_offside_data[cuml_offside_data$Instrument%in%cum_not_int,],y=FrcSkew,type="box",name="Cml!Int")
frc_skw_plot <- add_trace(cuml_onside_data,y=FrcSkew,type="box",name='On')
frc_skw_plot <- add_trace(cuml_offside_data,y=FrcSkew,type="box",name='Cml')
frc_skw_plot <- add_trace(intg_offside_data,y=FrcSkew,type="box",name='Int')

cuml_offside_data$DlrSwingOut <- (cuml_offside_data$VolOutof/10000)*abs(cuml_offside_data$MarketValue)
cuml_offside_data$DlrSkewOut  <- (-1^(1+cuml_offside_data$PsnLong))*sign(cuml_offside_data$SkewOutof)*((abs(cuml_offside_data$SkewOutof))^1/3)*cuml_offside_data$DlrSwingOut
cuml_offside_data$FrcSkewOut  <- cuml_offside_data$DlrSkewOut/abs(cuml_offside_data$MarketValue)
intg_offside_data$DlrSwingOut <- (intg_offside_data$VolOutof/10000)*abs(intg_offside_data$MarketValue)
intg_offside_data$DlrSkewOut  <- (-1^(1+intg_offside_data$PsnLong))*(-1^(1+intg_offside_data$Long))*sign(intg_offside_data$SkewOutof)*((abs(intg_offside_data$SkewOutof))^1/3)*intg_offside_data$DlrSwingOut
intg_offside_data$FrcSkewOut  <- intg_offside_data$DlrSkewOut/abs(intg_offside_data$MarketValue)
cuml_onside_data$DlrSwingOut <- (cuml_onside_data$VolOutof/10000)*abs(cuml_onside_data$MarketValue)
cuml_onside_data$DlrSkewOut  <- (-1^(1+cuml_onside_data$PsnLong))*sign(cuml_onside_data$SkewOutof)*((abs(cuml_onside_data$SkewOutof))^1/3)*cuml_onside_data$DlrSwingOut
cuml_onside_data$FrcSkewOut  <- cuml_onside_data$DlrSkewOut/abs(cuml_onside_data$MarketValue)

vol_plot_out <- plot_ly(cuml_offside_data[cuml_offside_data$Instrument%in%cum_not_int,],y=VolOutof,type="box",name="Cml!Int")
vol_plot_out <- add_trace(cuml_onside_data,y=VolOutof,type="box",name='On')
vol_plot_out <- add_trace(cuml_offside_data,y=VolOutof,type="box",name='Cml')
vol_plot_out <- add_trace(intg_offside_data,y=VolOutof,type="box",name='Int')

skw_plot_out <- plot_ly(cuml_offside_data[cuml_offside_data$Instrument%in%cum_not_int,],y=DlrSkewOut,type="box",name="Cml!Int")
skw_plot_out <- add_trace(cuml_onside_data,y=DlrSkewOut,type="box",name='On')
skw_plot_out <- add_trace(cuml_offside_data,y=DlrSkewOut,type="box",name='Cml')
skw_plot_out <- add_trace(intg_offside_data,y=DlrSkewOut,type="box",name='Int')

frc_skw_plot_out <- plot_ly(cuml_offside_data[cuml_offside_data$Instrument%in%cum_not_int,],y=FrcSkewOut,type="box",name="Cml!Int")
frc_skw_plot_out <- add_trace(cuml_onside_data,y=FrcSkewOut,type="box",name='On')
frc_skw_plot_out <- add_trace(cuml_offside_data,y=FrcSkewOut,type="box",name='Cml')
frc_skw_plot_out <- add_trace(intg_offside_data,y=FrcSkewOut,type="box",name='Int')

#10. how many positions that have been integrated offside, turn around
turnaround <- length(unique(history_data[(history_data$CumulativePL>0)&(history_data$IntegratedPL<0),]$Instrument))
to <- plot_ly(x=c('Switch','Total'),y=c(turnaround,length(intg_instruments_overall_offside)),type="bar")
to <- layout(title="Offside (cumulative)",yaxis=list(title="Number positions"),xaxis=list(title=""))

#11. PL effect of cutting positions when they become integrated offside
off_ins <- unique(cuml_offside_data$Instrument)
cut_pl <- c()
i_cut_pl <- c()
for(ins in off_ins){
  ins_pl <- cuml_offside_data[cuml_offside_data$Instrument==ins,]
  off_date <- min(ins_pl[ins_pl$CumulativePL<0,]$TradeDate)
  i_off_date <- min(ins_pl[ins_pl$IntegratedPL<0,]$TradeDate)
  cut_pl <- c(cut_pl,sum(cuml_offside_data[cuml_offside_data$TradeDate<=off_date&cuml_offside_data$Instrument==ins,]$TodayPL,na.rm=TRUE))
  i_cut_pl <- c(i_cut_pl,sum(cuml_offside_data[cuml_offside_data$TradeDate<=i_off_date&cuml_offside_data$Instrument==ins,]$TodayPL,na.rm=TRUE))
}

c_cut <- plot_ly(x=c('Cut','NotCut'),y=c(sum(cut_pl,na.rm=TRUE),sum(cuml_offside_data$TodayPL,na.rm=TRUE)),type="bar",name="Cumulative")
c_cut <- add_trace(x=c('Cut'),y=c(sum(i_cut_pl,na.rm=TRUE)),type="bar",name="Integrated")
c_cut <- layout(yaxis=list(title="Total PL"),xaxis=list(title=""))

#12. cumulative PL/return vs position age
psn_data <- new_psns(history_data)
agg_col <- 'PsnAge'
psn_data <- psn_data[psn_data$PsnAge<21,]
psn_data <- unique(psn_data[order(psn_data$Instrument,psn_data$TradeDate),c(agg_col,'TradeDate','Instrument','TodayPL')])
psns <- unique(psn_data$Instrument)
psn_data$CumulativePL <- NA
psn_data$IntegratedPL <- NA
for(p in psns){
  psn_data[psn_data$Instrument==p,]$TodayPL[is.na(psn_data[psn_data$Instrument==p,]$TodayPL)] <- 0
  psn_data[psn_data$Instrument==p,]$CumulativePL <- cumsum(psn_data[psn_data$Instrument==p,]$TodayPL)
  psn_data[psn_data$Instrument==p,]$IntegratedPL <- cumsum(psn_data[psn_data$Instrument==p,]$CumulativePL)
}
acc_cols <- c('CumulativePL','IntegratedPL')
acc_fn <- function(x)x
agg_fn <- function(x)mean(x,na.rm=TRUE)

plt_data <- aggregate(psn_data[acc_cols],list(Age=psn_data[[agg_col]]),agg_fn)
for(ac in acc_cols){
  plt_data[ac] <- acc_fn(plt_data[ac])  
}

cumpl <- plot_ly(plt_data,x=Age,y=CumulativePL,name="Cumulative")
cumpl <- add_trace(plt_data,x=Age,y=IntegratedPL,name="Integrated")

#13. rank by offside
rnk_data <- rank_offside(cuml_offside_data[!is.na(cuml_offside_data$TradeID),],smooth=FALSE)
rnk_frame <- rnk_data[[2]][!is.infinite(rnk_data[[2]]$FractionOff),]
rnk_frame <- rnk_frame[order(rnk_frame$FractionOff),]
rnk <- rnk_data[[1]] 

# BUILD GRAPHIC OUTPUT:
ins <- 5056
iname <- unique(history_data[history_data$Instrument==ins,'Name'])
psn_data <- psn_data[psn_data$Instrument==ins,]
example_data <- cbind(Name='$ Cumulative',psn_data[c('TradeDate','CumulativePL')])
example_plot <- multiline_series_plot(example_data,paste("Example position:",iname),example_data$TradeDate,example_data$CumulativePL,"$","Date",example_data$Name,"",theme_fn=kobe_theme2)

colnames(occrt_c) <- c('Date','Total')
positions_data <- rbind(cbind(Name='All positions',all),cbind(Name='Offside',occrt_c))
#positions_plot <- multiline_series_plot(positions_data,paste("Number offside positions"),positions_data$Date,positions_data$Total,"Count","Date",positions_data$Name,"",theme_fn=kobe_theme2)
positions_plot <- ggplot(data=positions_data, aes(x=Date, y=Total, group = Name, colour = Name)) +
  geom_line(size = 0.8) + 
  ylab("Number of positions") + xlab('Date') + ggtitle('Number of absolute offside positions') + scale_linetype_discrete(name = "") +
  #geom_point( size=4, shape=21, fill="white") +
  kobe_theme2() 
  
occrt_c_strat <- aggregate(cuml_offside_data['CumulativePL'],list(Date=cuml_offside_data$TradeDate,Strategy=cuml_offside_data$Strategy),function(x)sum(unique(x)<0))
colnames(occrt_c_strat) <- c('Date','Strategy','Total')
#positions_data_strat <- rbind(cbind(Strategy='All',occrt_c),occrt_c_strat)
positions_data_strat <- occrt_c_strat
#positions_plot_strat <- multiline_series_plot(positions_data_strat,paste("What strategies do these occur in?"),positions_data_strat$Date,positions_data_strat$Total,"Count","Date",positions_data_strat$Strategy,"",theme_fn=kobe_theme2,facet_spec=facet_grid(Strategy ~ .))
positions_plot_strat <- ggplot(data=positions_data_strat, aes(x=Date, y=Total, group = Strategy, colour = Strategy)) +
  geom_line(size = 0.8) + 
  ylab("Number of positions") + xlab('Date') + ggtitle('How are these distributed over strategies?') + scale_linetype_discrete(name = "Strategy") +
  #geom_point( size=4, shape=21, fill="white") +
  kobe_theme2() +
  facet_grid( Strategy~. )

psn_pl_data <- cuml_offside_data
all_psn_pl_data <- history_data
prepare_strat_sum_data <- function(psn_pl_data,all_psn_pl_data,subtract_data=TRUE){
  pl_data_strat <- aggregate(psn_pl_data['TodayPL'],list(Strategy=psn_pl_data$Strategy,Date=psn_pl_data$TradeDate),function(x)sum(x,na.rm=TRUE))
  all_pl_data_strat <- aggregate(all_psn_pl_data['TodayPL'],list(Strategy=all_psn_pl_data$Strategy,Date=all_psn_pl_data$TradeDate),function(x)sum(x,na.rm=TRUE))
  if(subtract_data){
    pl_data_strat <- merge(all_pl_data_strat,pl_data_strat,by=c('Date','Strategy'),all.x=TRUE)
    pl_data_strat$TodayPL.y[is.na(pl_data_strat$TodayPL.y)] <- 0
    pl_data_strat$TodayPL <- pl_data_strat$TodayPL.x - pl_data_strat$TodayPL.y 
  }
  pl_data_strat <- pl_data_strat[order(pl_data_strat$Date),]
  all_pl_data_strat <- all_pl_data_strat[order(all_pl_data_strat$Date),]
  for(s in unique(all_pl_data_strat$Strategy)){
    pl_data_strat[pl_data_strat$Strategy==s,]$TodayPL <- cumsum(pl_data_strat[pl_data_strat$Strategy==s,]$TodayPL)
    all_pl_data_strat[all_pl_data_strat$Strategy==s,]$TodayPL <- cumsum(all_pl_data_strat[all_pl_data_strat$Strategy==s,]$TodayPL)
  }
  pl_data_strat <- cbind(Name='Offside PL',pl_data_strat)
  all_pl_data_strat <- cbind(Name='Total PL',all_pl_data_strat)
  pl_data_strat <- rbind(pl_data_strat[c('Name','Date','Strategy','TodayPL')],all_pl_data_strat)
  return(pl_data_strat)
}
pl_data_strat <- prepare_strat_sum_data(psn_pl_data,all_psn_pl_data)
#pl_data_plot <- multicolor_series_plot(all_pl_data_strat,paste("Arial of offside positions on PL"),all_pl_data_strat$Date,all_pl_data_strat$TodayPL,"$","Date",all_pl_data$Name,"Name",theme_fn=kobe_theme2,facet_spec=facet_wrap('Strategy'))
pl_data_plot <- ggplot(data=pl_data_strat, aes(x=Date, y=TodayPL, group = Name, colour = Name)) +
       geom_line(size = 1.5) + 
       #geom_point( size=4, shape=21, fill="white") +
       kobe_theme2() +
       facet_grid( Strategy~. )

off_ins <- unique(cuml_offside_data$Instrument)
cut_data <- cuml_offside_data
for(ins in off_ins){
  ins_pl <- cut_data[cut_data$Instrument==ins,]
  off_date <- min(ins_pl[ins_pl$CumulativePL<0,]$TradeDate,na.rm=TRUE)
  if(length(cut_data[cut_data$Instrument==ins&cut_data$TradeDate>off_date,]$TodayPL)>0){
    cut_data[cut_data$Instrument==ins&cut_data$TradeDate>off_date,]$TodayPL <-0
  }
  #i_off_date <- min(ins_pl[ins_pl$IntegratedPL<0,]$TradeDate)
}
cut_data_strat <- prepare_strat_sum_data(cut_data,all_psn_pl_data,subtract_data=FALSE)
cut_off_data <- cut_data_strat[cut_data_strat$Name=='Offside PL',]
no_off_data <- pl_data_strat[pl_data_strat$Name=='Offside PL',]
revised_off_data <- merge(no_off_data,cut_off_data,by=c('Date','Strategy'),all.x=TRUE)
revised_off_data[is.na(revised_off_data$TodayPL.y),]$TodayPL.y <- 0
revised_off_data$TodayPL <- revised_off_data$TodayPL.x + revised_off_data$TodayPL.y
revised_off_data <- revised_off_data[c('Date','Strategy','Name.x','TodayPL')]
colnames(revised_off_data) <- c('Date','Strategy','Name','TodayPL')
revised_off_data$Name <- 'Offside Cut'
cut_data_strat <- rbind(pl_data_strat,revised_off_data)

cut_data_plot <- ggplot(data=cut_data_strat, aes(x=Date, y=TodayPL, group = Name, colour = Name)) +
  geom_line(size = 0.5) + 
  ylab("PL $") + xlab('Date') + ggtitle('PL when offside positions are cut') + scale_linetype_discrete(name = "") +
  #geom_point( size=4, shape=21, fill="white") +
  facet_grid( Strategy~. , scales = "free_y")  

cut_total <- aggregate(cut_data_strat[cut_data_strat$Date=='2015-12-31',]$TodayPL,list(Name=cut_data_strat[cut_data_strat$Date=='2015-12-31',]$Name),sum)
cut_total <- cbind(Type="Cut",cut_total)
not_taken_total <- aggregate(pl_data_strat[pl_data_strat$Date=='2015-12-31',]$TodayPL,list(Name=pl_data_strat[pl_data_strat$Date=='2015-12-31',]$Name),sum)
not_taken_total <- cbind(Type="Not taken",not_taken_total)
summary <- rbind(not_taken_total,cut_total)
colnames(summary) <- c('Type','Name','PL')
pl_summary_plot <- ggplot(data=summary, aes(x=Name)) +
  geom_bar(aes(weight=PL)) + 
  ylab("Total PL $") + xlab('') + ggtitle('PL over whole period') + 
  facet_grid( Type~. )

#repeat above for histrgram type/trades
psn_trade_data <- cuml_trades_in_offside_psns[c('TradeDate','Name','Strategy','MarketValue','Long','Age')]
psn_trade_data$PsnLong <- psn_trade_data$MarketValue > 0
psn_trade_data$New <- psn_trade_data$Age == 0
psn_trade_data$TradeDate <- format(psn_trade_data$TradeDate,'%Y-%m')
psn_trade_data$Increase <- psn_trade_data$Long==psn_trade_data$PsnLong
psn_trade_data$Decrease <- psn_trade_data$Long!=psn_trade_data$PsnLong
psn_trade_data$Type[psn_trade_data$Increase&!psn_trade_data$New] <- 'Increase'
psn_trade_data$Type[psn_trade_data$Decrease&!psn_trade_data$New] <- 'Decrease'
psn_trade_data$Type[psn_trade_data$New] <- 'New'
trades_strat <- aggregate(psn_trade_data['Name'],list(Month=psn_trade_data$TradeDate,Strategy=psn_trade_data$Strategy,Type=psn_trade_data$Type),function(x)length(x))
#trades_plot_strat <- histogram(trades_strat,paste("How many trades offside?"),trades_strat$Month,trades_strat$Name,"Count","Month",group_names=trades_strat$Type,theme_fn=kobe_theme2,facet_spec=facet_grid(Strategy ~ .))
trades_plot_strat <- ggplot(data=trades_strat, aes(x=Month, fill = Type)) +
  geom_bar() + 
  ylab("Number trades") + xlab('') + ggtitle('Number offside trades') + 
  facet_grid( Strategy~. )

#Total PL after the trades in the above categories
psn_trade_pl_data <- cuml_offside_data
psn_trade_pl_data$PsnLong <- psn_trade_pl_data$MarketValue > 0
psn_trade_pl_data$New <- psn_trade_pl_data$Age == 0
psn_trade_pl_data$Increase <- psn_trade_pl_data$Long==psn_trade_pl_data$PsnLong
psn_trade_pl_data$Decrease <- psn_trade_pl_data$Long!=psn_trade_pl_data$PsnLong
psn_trade_pl_data$Type[psn_trade_pl_data$Increase&!psn_trade_pl_data$New] <- 'Increase'
psn_trade_pl_data$Type[psn_trade_pl_data$Decrease&!psn_trade_pl_data$New] <- 'Decrease'
psn_trade_pl_data$Type[psn_trade_pl_data$New] <- 'New'
first <- TRUE
for(stk in unique(cuml_offside_data$Instrument)){
  offside_trade_dates <- rev(sort(unique(psn_trade_pl_data[psn_trade_pl_data$Instrument==stk&!is.na(psn_trade_pl_data$TradeID),'TradeDate'])))
  for(t in 1:length(offside_trade_dates)){
    dt <- offside_trade_dates[t]
    strategy <- unique(psn_trade_pl_data[psn_trade_pl_data$Instrument==stk&psn_trade_pl_data$TradeDate==dt&!is.na(psn_trade_pl_data$TradeID),'Strategy'])
    stopifnot(length(strategy)==1)
    type <- unique(psn_trade_pl_data[psn_trade_pl_data$Instrument==stk&psn_trade_pl_data$TradeDate==dt&!is.na(psn_trade_pl_data$TradeID),'Type'])
    stopifnot(length(type)==1)
    ttl <- sum(psn_trade_pl_data[psn_trade_pl_data$Instrument==stk&psn_trade_pl_data$TradeDate>=dt,'TodayPL'],na.rm=TRUE)
    if(first){
      ttls <- data.frame(TradeDate=dt,Strategy=strategy,Type=type,PL=ttl)
      first <- FALSE
    }
    else{
      ttls <- rbind(ttls,data.frame(TradeDate=dt,Strategy=strategy,Type=type,PL=ttl))
    }
  }
}
ttls$TradeDate <- format(ttls$TradeDate,'%Y-%m')
trade_pl <- aggregate(ttls['PL'],list(Month=ttls$TradeDate,Strategy=ttls$Strategy,Type=ttls$Type),function(x)mean(x,na.rm=TRUE))

trades_plot_strat_pl <- ggplot(data=trade_pl, aes(x=Month, fill = Type)) +
  geom_bar(aes(weight=PL),position="dodge") + 
  ylab("Mean post trade PL $") + xlab('') + ggtitle('Mean offside post trade PL') + 
  facet_grid( Strategy~. )

#Compose into graphic pane
#1. Position level
loadfonts()
pdf("prototype_offside_analysis.pdf", width=7.5, height= 11.2,family="Arial")
grid.newpage() 
pushViewport(viewport(layout = grid.layout(4, 3)))
grid.rect(gp = gpar(fill = "#E2E2E3", col = "#E2E2E3"))
grid.text("Offside Positions", y = unit(0.94, "npc"), gp = gpar(fontfamily = "Arial", col = "#E7A922", cex = 2.5))
print(positions_plot, vp = vplayout(2, 1:3))
print(positions_plot_strat, vp = vplayout(3:4, 1:3))
#print(p2, vp = vplayout(4, 1:3))
grid.rect(gp = gpar(fill = "#E7A922", col = "#E7A922"), x = unit(0.5, "npc"), y = unit(0.82, "npc"), width = unit(1, "npc"), height = unit(0.11, "npc"))
grid.text("The number of offside positions", vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.88, "npc"), gp = gpar(fontfamily = "Arial", col = "#552683", cex = 0.8))
grid.text("Data utilised:", vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.86, "npc"), gp = gpar(fontfamily = "Arial", col = "white", cex = 0.6))
grid.text(paste(
  "Trader",
  "Start date",
  "End date",
  "Number strategies",
  "Analysis level",
  "Granularity",
  "Run date", sep = "\n"), vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.78, "npc"), gp = gpar(fontfamily = "Arial", col = "#552683", cex = 0.5))
grid.text(paste(
  "Barry Anten",
  "2015-09-01",
  "2015-12-01",
  "10",
  "Position",
  "Daily",
  "2016-03-18", sep = "\n"), vjust = 0, hjust = 0, x = unit(0.15, "npc"), y = unit(0.78, "npc"), gp = gpar(fontfamily = "Arial", col = "#552683", cex = 0.5))
dev.off()

pdf("prototype_pl_analysis.pdf", width=7.5, height= 11.2,family="Arial")
grid.newpage() 
pushViewport(viewport(layout = grid.layout(4, 3)))
grid.rect(gp = gpar(fill = "#E2E2E3", col = "#E2E2E3"))
grid.text("Offside Positions", y = unit(0.94, "npc"), gp = gpar(fontfamily = "Arial", col = "#E7A922", cex = 2.5))
print(cut_data_plot, vp = vplayout(2:4, 2:3))
print(pl_summary_plot, vp = vplayout(2:3, 1))
#print(p2, vp = vplayout(4, 1:3))
grid.rect(gp = gpar(fill = "#E7A922", col = "#E7A922"), x = unit(0.5, "npc"), y = unit(0.82, "npc"), width = unit(1, "npc"), height = unit(0.11, "npc"))
grid.text("PL impact of offside positions", vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.88, "npc"), gp = gpar(fontfamily = "Arial", col = "#552683", cex = 0.8))
grid.text("Data utilised:", vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.86, "npc"), gp = gpar(fontfamily = "Arial", col = "white", cex = 0.6))
grid.text(paste(
  "Trader",
  "Start date",
  "End date",
  "Number strategies",
  "Analysis level",
  "Granularity",
  "Run date", sep = "\n"), vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.78, "npc"), gp = gpar(fontfamily = "Arial", col = "#552683", cex = 0.5))
grid.text(paste(
  "Barry Anten",
  "2015-09-01",
  "2015-12-01",
  "10",
  "Position",
  "Daily",
  "2016-03-18", sep = "\n"), vjust = 0, hjust = 0, x = unit(0.15, "npc"), y = unit(0.78, "npc"), gp = gpar(fontfamily = "Arial", col = "#552683", cex = 0.5))
dev.off()

pdf("prototype_trade_pl_analysis.pdf", width=7.5, height= 11.2,family="Arial")
grid.newpage() 
pushViewport(viewport(layout = grid.layout(4, 2)))
grid.rect(gp = gpar(fill = "#E2E2E3", col = "#E2E2E3"))
grid.text("Offside Positions", y = unit(0.94, "npc"), gp = gpar(fontfamily = "Arial", col = "#E7A922", cex = 2.5))
print(trades_plot_strat, vp = vplayout(2:4, 1))
print(trades_plot_strat_pl, vp = vplayout(2:4, 2))
#print(p2, vp = vplayout(4, 1:3))
grid.rect(gp = gpar(fill = "#E7A922", col = "#E7A922"), x = unit(0.5, "npc"), y = unit(0.82, "npc"), width = unit(1, "npc"), height = unit(0.11, "npc"))
grid.text("PL impact of offside trades", vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.88, "npc"), gp = gpar(fontfamily = "Arial", col = "#552683", cex = 0.8))
grid.text("Data utilised:", vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.86, "npc"), gp = gpar(fontfamily = "Arial", col = "white", cex = 0.6))
grid.text(paste(
  "Trader",
  "Start date",
  "End date",
  "Number strategies",
  "Analysis level",
  "Granularity",
  "Run date", sep = "\n"), vjust = 0, hjust = 0, x = unit(0.01, "npc"), y = unit(0.78, "npc"), gp = gpar(fontfamily = "Arial", col = "#552683", cex = 0.5))
grid.text(paste(
  "Barry Anten",
  "2015-09-01",
  "2015-12-01",
  "10",
  "Trade",
  "Monthly",
  "2016-03-18", sep = "\n"), vjust = 0, hjust = 0, x = unit(0.15, "npc"), y = unit(0.78, "npc"), gp = gpar(fontfamily = "Arial", col = "#552683", cex = 0.5))
dev.off()

