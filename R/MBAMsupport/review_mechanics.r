setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules/analysis_module_position_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../reporting/raid_data_import.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/dataplex.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("prototype_portfolio_core_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("coaching_review_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(lubridate)
library(quantmod)
library(ggplot2)
library(useful)
library(grid)
library(RODBC)

trader   <- 101
dates <- c("2016-04-01")
history_data <- analysis_module_load_multiple(trader,dates,history_analysis_module_builder,dated_twelve_monthly_lookback)
thisQ <- quarter(as.Date(dates)-1)
history_data <- market_rel_pl(history_data,trade_rel=FALSE)
history_data <- market_day_age(history_data)
history_data <- history_data[history_data$TradeDate>='2015-04-01'&history_data$TradeDate<'2016-04-01',]
instruments <- unique(history_data$Instrument)
all_trades <- history_data[!is.na(history_data$TradeID),]

#1. Offside positions
#NB: Need to handle going flat (at the moment just look at earliest position date)
rel_offside_data <- integrated_offside(history_data,type="CumulativeMarketRelPL")
abs_offside_data <- integrated_offside(history_data,type="CumulativePL")
abs_off <- new_psns(abs_offside_data[[1]])
abs_off <- abs_off[abs_off$PsnAge>5,]
rel_off <- new_psns(rel_offside_data[[1]])
rel_off <- rel_off[rel_off$PsnAge>5,]
n_psns <- aggregate(history_data['Instrument'],list(Date=history_data$TradeDate),function(x)sum(!is.na(unique(x))))
n_abs_offside <- aggregate(abs_off['CumulativePL'],list(Date=abs_off$TradeDate),function(x)sum(unique(x)<0))
n_rel_offside <- aggregate(rel_off['CumulativeMarketRelPL'],list(Date=rel_off$TradeDate),function(x)sum(unique(x)<0))

n_psns$Q <- quarter(n_psns$Date)
n_psns <- aggregate(n_psns['Instrument'],list(Quarter=(n_psns$Q==thisQ)),mean)
n_abs_offside$Q <- quarter(n_abs_offside$Date)
n_abs_offside <- aggregate(n_abs_offside['CumulativePL'],list(Quarter=(n_abs_offside$Q==thisQ)),mean)
n_rel_offside$Q <- quarter(n_rel_offside$Date)
n_rel_offside <- aggregate(n_rel_offside['CumulativeMarketRelPL'],list(Quarter=(n_rel_offside$Q==thisQ)),mean)
all_n_psn_data <- merge(n_psns,n_abs_offside,by='Quarter')
all_n_psn_data <- merge(all_n_psn_data,n_rel_offside,by='Quarter')
colnames(all_n_psn_data) <- c('Quarter','Total','AbOff','RelOff')

off_chrt_data <- rbind(data.frame(Quantity='Number abs. offside',
                                  Value=all_n_psn_data$AbOff[all_n_psn_data$Quarter],
                                  Delta=all_n_psn_data$AbOff[all_n_psn_data$Quarter]-all_n_psn_data$AbOff[!all_n_psn_data$Quarter]),
                       data.frame(Quantity='Number rel. offside',
                                  Value=all_n_psn_data$RelOff[all_n_psn_data$Quarter],
                                  Delta=all_n_psn_data$RelOff[all_n_psn_data$Quarter]-all_n_psn_data$RelOff[!all_n_psn_data$Quarter]),
                       data.frame(Quantity='Total positions',
                                 Value=all_n_psn_data$Total[all_n_psn_data$Quarter],
                                 Delta=all_n_psn_data$Total[all_n_psn_data$Quarter]-all_n_psn_data$Total[!all_n_psn_data$Quarter]))

off_smmry <- ggplot(data=off_chrt_data, aes(x=Quantity, fill=Quantity)) +
  geom_bar(aes(weight=Value)) +
  ylab("Number of Positions") + xlab("") + ggtitle('Total number of offside positions') +
  geom_text(aes(x= Quantity, y=Value, label = round(Delta)),size=4,fontface='bold') +  
  theme(legend.position = "none",axis.text.x = element_text(angle = 30, hjust = 1)) +
  theme(text = element_text(size=15)) +
  scale_fill_brewer(palette="Set1")

#Relate offside to position age and size
hd <- unique(history_data[c('TradeDate','Instrument','CumulativePL','CumulativeMarketRelPL','MarketRelPL','TodayPL','MarketValue','MinDate','PsnAge')])
hd <- hd[hd$PsnAge<50,]
hd$OffsideCnt <- hd$CumulativePL < 0
hd$RelOffsideCnt <- hd$CumulativeMarketRelPL < 0
rank_offside <- aggregate(hd[c('CumulativePL','CumulativeMarketRelPL')],list(Instrument=hd$Instrument),function(x)min(x,na.rm=TRUE))
rank_offside <- merge(rank_offside,
                      aggregate(hd[c('TodayPL','MarketRelPL','OffsideCnt','RelOffsideCnt')],list(Instrument=hd$Instrument),function(x)sum(x,na.rm=TRUE)),
                      by = c('Instrument'))
rank_offside <- merge(rank_offside,
                      aggregate(hd[c('MarketValue')],list(Instrument=hd$Instrument),function(x)mean(abs(x),na.rm=TRUE)),
                      by = c('Instrument'))
rank_offside$Offside <- 100*(rank_offside$CumulativePL/abs(rank_offside$MarketValue))
rank_offside$OffsideRel <- 100*(rank_offside$CumulativeMarketRelPL/abs(rank_offside$MarketValue))
rank_offside <- rank_offside[rank_offside$OffsideCnt<50,]
rank_offside$Gain <- rank_offside$TodayPL > 0
rank_offside$RelGain <- rank_offside$MarketRelPL > 0
rank_plt_data <- with(rank_offside,rbind(cbind(Type='Absolute Offside',data.frame(DaysOffside=OffsideCnt,PcntOffside=Offside,Size=MarketValue,WinLoss=Gain)),
                                         cbind(Type='Relative Offside',data.frame(DaysOffside=OffsideCnt,PcntOffside=OffsideRel,Size=MarketValue,WinLoss=RelGain))))
off_rank <- ggplot(data=rank_plt_data,aes(x=DaysOffside,y=PcntOffside,size=Size)) +
            geom_point(aes(colour=WinLoss)) +
            ylim(c(-200,200)) +
            labs(size='Av. Size $',colour='Positive PL') +
            theme(text = element_text(size=15)) +
            ylab("% offside") + 
            xlab("Total days position offside") + 
            theme(legend.position = "bottom") +
            ggtitle('Position PL in buckets of total days offside')  +
            facet_grid(Type~.)

rank_offside$OffCat <- "NA"
rank_offside$OffCat[rank_offside$OffsideCnt<10] <- "< 10d"
rank_offside$OffCat[rank_offside$OffsideCnt>9&rank_offside$OffsideCnt<20] <- "10 - 20d"
rank_offside$OffCat[rank_offside$OffsideCnt>19&rank_offside$OffsideCnt<30] <- "20 - 30d"
rank_offside$OffCat[rank_offside$OffsideCnt>29&rank_offside$OffsideCnt<40] <- "30 - 40d"
rank_offside$OffCat[rank_offside$OffsideCnt>39&rank_offside$OffsideCnt<50] <- "40 - 50d"
pl_by_offside <- aggregate(rank_offside[c('TodayPL','MarketRelPL')],list(DaysOff=rank_offside$OffCat),sum)
pl_bucket_plt <- rbind(cbind(Type='Abs. offside',data.frame(Days=pl_by_offside$DaysOff,PL=pl_by_offside$TodayPL)),
                       cbind(Type='Rel. offside',data.frame(Days=pl_by_offside$DaysOff,PL=pl_by_offside$MarketRelPL)))

bucket_off_pl <- ggplot(data=pl_bucket_plt,aes(x=Days,fill=Type)) +
                 geom_bar(aes(weight=PL),position="dodge") +
                 ylab("Total PL $") + 
                 xlab("Total days position offside") + 
                 labs(fill="") +
                 theme(legend.position = "bottom") +
                 scale_fill_brewer(palette="Set1") +
                 theme(text = element_text(size=15)) +
                 ggtitle('Position PL by total days offside') 


#Plot cum pl on same axes and determine identities of old positions (hedges?)
#that seem to add significantly to PL
pl_by_day <- aggregate(rank_offside[c('TodayPL','MarketRelPL')],list(Days=rank_offside$OffsideCnt),sum)
pl_by_day <- pl_by_day[order(pl_by_day$Days),]

cum_pl_plt <- rbind(cbind(Type='Abs. offside',data.frame(Days=pl_by_day$Days,PL=cumsum(pl_by_day$TodayPL))),
                    cbind(Type='Rel. offside',data.frame(Days=pl_by_day$Days,PL=cumsum(pl_by_day$MarketRelPL))))

cum_pl_smmry <- ggplot(data=cum_pl_plt,aes(x=Days,y=PL,group=Type,colour=Type)) +
                geom_line(size=1) +
                ylab("Cumulative PL $") + 
                xlab("Total days position offside") + 
                labs(colour="") +
                theme(legend.position = "bottom") +
                theme(text = element_text(size=15)) +
                scale_colour_brewer(palette="Set1") +
                ggtitle('Cumulative PL by days offside') 

#summry: extent*days and PL
hd <- unique(history_data[c('Instrument','TradeDate','CumulativePL','CumulativeMarketRelPL','MarketRelPL','MarketValue','Strategy')])
hd <- hd[hd$Strategy!='BA_SHEDGE',]
hd$RelOff <- hd$CumulativeMarketRelPL < 0 
hd$DaysOff<- NA
for(ins in unique(hd$Instrument)){
  hd[hd$Instrument==ins,]$DaysOff <- cumsum(hd[hd$Instrument==ins,]$RelOff)  
}
hd$BpsOff <- 10000*(hd$CumulativePL/abs(hd$MarketValue))
hd$BpsOff[is.infinite(hd$BpsOff)] <- NA
hd$BpsOff[is.nan(hd$BpsOff)] <- NA
track_offside <- with(hd,aggregate(Instrument,list(Date=TradeDate,Offside=RelOff),function(x)length(unique(x))))
track_offside <- merge(track_offside,aggregate(hd[c('BpsOff','DaysOff')],list(Date=hd$TradeDate,Offside=hd$RelOff),function(x)mean(x,na.rm=TRUE)),by=c('Date','Offside'))
track_offside <- merge(track_offside,aggregate(hd[c('MarketValue')],list(Date=hd$TradeDate,Offside=hd$RelOff),function(x)median(abs(x),na.rm=TRUE)),by=c('Date','Offside'))
colnames(track_offside) <- c('Date','Offside','NPos','BpsOff','DaysOff','MarketValue')
track_offside <- rbind(cbind(Quantity="Fraction offside",data.frame(Date=track_offside$Date,Value=(track_offside[track_offside$Offside,]$NPos/(track_offside[track_offside$Offside,]$NPos+track_offside[!track_offside$Offside,]$NPos)),Days=track_offside[track_offside$Offside,]$DaysOff,MarketValue=track_offside[track_offside$Offside,]$MarketValue)),
                       cbind(Quantity="Bps Offside",data.frame(Date=track_offside$Date,Value=track_offside[track_offside$Offside,]$BpsOff,Days=track_offside[track_offside$Offside,]$DaysOff,MarketValue=track_offside[track_offside$Offside,]$MarketValue)))
track_offside$Date <- format(track_offside$Date,'%Y-%m') 
track_offside <- aggregate(track_offside[c('Value','Days','MarketValue')],list(Date=track_offside$Date,Quantity=track_offside$Quantity),function(x)mean(x,na.rm=TRUE))

track_offside_plt <-ggplot(data=track_offside[track_offside$Quantity=="Bps Offside",],aes(x=Date,y=Value,size=MarketValue)) +
  geom_point(aes(colour=Days)) +
  theme(text = element_text(size=15)) +
  ylab("Bps offside") + 
  xlab("Month") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_size_continuous(guide = FALSE) +
  labs(colour="Av. days offside") +
  scale_colour_distiller(palette="Spectral") +
  ggtitle('Av. bps offside of relative offside positions by month')  

top_off <- hd[hd$TradeDate>'2016-01-01',]

#Averaging down
avg_dwn_cols <- c('Instrument','TradeDate','TodayPL','PnLOutof','VolInto','SkewInto','PnLInto','DeltaPL','DeltaSwing','DeltaSkew','MarketValue','Av.MarketValue','Long','PsnLong','CumulativePL','IntegratedPL','MarketRelPL','CumulativeMarketRelPL','ValueUSD')
hd <- history_data
average_down_trades <- unique(hd[(hd$CumulativePL<0)&!is.na(hd$TradeID)&hd$Age>0,avg_dwn_cols])
average_down_trades <- subset(average_down_trades,(average_down_trades$Long&average_down_trades$PsnLong)|(!average_down_trades$Long&!average_down_trades$PsnLong))
average_down_positions <- unique(average_down_trades$Instrument)
other_trades <- unique(hd[(hd$CumulativePL>0)&!is.na(hd$TradeID),avg_dwn_cols])

pl_frame <- merge(hd,data.frame(Instrument=instruments),by=c('Instrument'))
pl_frame <- unique(pl_frame[c(avg_dwn_cols)])
pl_frame$TradeCount <- NA
pl_frame$PsnAge <- NA
for(ins in instruments){
  cnt <- sum(average_down_trades$Instrument==ins)
  pl_frame$TradeCount[pl_frame$Instrument==ins] <- cnt
  pl_frame$PsnAge[pl_frame$Instrument==ins] <- as.numeric(pl_frame$TradeDate[pl_frame$Instrument==ins] - min(pl_frame$TradeDate[pl_frame$Instrument==ins],na.rm=TRUE))
}
avg_dwn <- aggregate(pl_frame[pl_frame$PsnAge<50,c('TodayPL','MarketRelPL')],list(TradeCount=pl_frame[pl_frame$PsnAge<50,]$TradeCount),sum)
avg_dwn <- merge(avg_dwn,aggregate(pl_frame[pl_frame$PsnAge<50,c('Instrument')],list(TradeCount=pl_frame[pl_frame$PsnAge<50,]$TradeCount),function(x)length(unique(x))),by=c('TradeCount'))
adown_plt <- rbind(data.frame(Quantity='Abs PL',TradeCount=avg_dwn$TradeCount,PL=avg_dwn$TodayPL,Ntrades=avg_dwn$x),
                   data.frame(Quantity='Rel PL',TradeCount=avg_dwn$TradeCount,PL=avg_dwn$MarketRelPL,Ntrades=avg_dwn$x))

colnames(adown_plt)[colnames(adown_plt)=='Quantity'] <- 'Type'
colnames(adown_plt)[colnames(adown_plt)=='Ntrades'] <- 'Num.Trades'
adown_smmry <- ggplot(data=adown_plt,aes(x=TradeCount,y=PL,size=Num.Trades)) +
               ylab("Total position PL $") + 
               xlab("Total number average down trades") + 
               ggtitle('Position PL by times averaged down') +
               scale_colour_brewer(palette="Set1") +
               theme(text = element_text(size=15)) +
               geom_point(aes(colour=Type))

average_down_trades <- unique(average_down_trades[c('Instrument','TradeDate','ValueUSD','TodayPL')])
average_down_trades$TradeDate <- format(average_down_trades$TradeDate,'%Y-%m') 
adown_focus <- aggregate(average_down_trades['Instrument'],list(Date=average_down_trades$TradeDate),function(x)length(x))
adown_focus <- merge(adown_focus,aggregate(average_down_trades[c('ValueUSD','TodayPL')],list(Date=average_down_trades$TradeDate),function(x)mean(x,na.rm=TRUE)),by='Date')

adown_focus_plt <- ggplot(data=adown_focus,aes(x=Date,y=Instrument,size=ValueUSD)) +
  geom_point(aes(colour=TodayPL)) +
  theme(text = element_text(size=15)) +
  ylab("N Trades") + 
  xlab("Month") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_size_continuous(guide = FALSE) +
  labs(colour="Av. $ PL") +
  #scale_colour_distiller(palette="Spectral") +
  scale_colour_gradient(low = "Red", high = "Blue") +
  ggtitle('Number average down trades')  



grid.newpage() 
pushViewport(viewport(layout = grid.layout(2, 2)))
print(off_smmry, vp = vplayout(1, 1))
print(bucket_off_pl, vp = vplayout(1, 2))
print(adown_smmry, vp = vplayout(2, 1))
print(cum_pl_smmry, vp = vplayout(2, 2))


#2. Position revisits
#NB: Add market rel?
# pick out petnames/most visited
flat_data <- count_flats(history_data)
revisit_data <- create_revisit_data(flat_data[[1]],history_data)
revisit_plt_data  <- build_revisit_plot_data(revisit_data,flat_data[[2]],function(x)sum(x,na.rm=TRUE))
#df <- cbind(IsTrade=0,TodayPL=0,Reduce(function(x,y)rbind(x,y),Map(function(x)data.frame(x,1:11),1:11)))
#colnames(df) <- c('IsTrade','TodayPL','TotalVisits','Visit')
#revisit_plt_data <- rbind(revisit_plt_data,df)
revisit_plt_data <- aggregate(revisit_plt_data[c('IsTrade','TodayPL')],list(TotalVisits=revisit_plt_data$TotalVisits,Visit=revisit_plt_data$Visit),sum)
revisit_plt_data <- with(revisit_plt_data,rbind(data.frame(Quantity='Total PL after visit',Value=TodayPL,TotalN=TotalVisits,VisitN=Visit),
                                                data.frame(Quantity='Number Trades',Value=IsTrade,TotalN=TotalVisits,VisitN=Visit)))
revisit_smmry <- ggplot(revisit_plt_data,aes(x=TotalN,group=VisitN,fill=VisitN)) +
                 geom_bar(position="dodge",aes(weight=Value)) +
                 scale_fill_distiller(palette = "Spectral") +
                 ylab("") + 
                 xlab("Total visits to stock") + 
                 labs(fill="Visit number") +
                 ggtitle('PL by number of visits to a stock') +
                 theme(text = element_text(size=15)) +
                 facet_grid(Quantity~.,scales="free_y")

#3. Holding period
pl_hd <- unique(history_data[c('TradeDate','Instrument','TodayPL','PassiveTodayPL','ActiveTodayPL','MarketRelPL','MinDate','MarketValue','PsnAge')])
pl_hd <- pl_hd[pl_hd$PsnAge<70,]

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

pl_hd$Indicator <- 1
pl_by_age <- aggregate(pl_hd[c('TodayPL','PassiveTodayPL','ActiveTodayPL','MarketRelPL','Indicator')],list(Age=pl_hd$PsnAge),function(x)sum(x,na.rm=TRUE))
pl_by_age <- merge(pl_by_age,aggregate(pl_hd['MarketValue'],list(Age=pl_hd$PsnAge),function(x)mean(x,na.rm=TRUE)),by='Age')
pl_by_age <- remove_nan(pl_by_age)
pl_by_age[is.na(pl_by_age)] <- 0

plt_cum_pl_data <- rbind(cbind(Type='Cumulative PL',Quantity='Total PL',data.frame(Age=pl_by_age$Age,PL=cumsum(pl_by_age$TodayPL))),
                         cbind(Type='Cumulative PL',Quantity='MarketRel PL',data.frame(Age=pl_by_age$Age,PL=cumsum(pl_by_age$MarketRelPL))),
                         cbind(Type='Cumulative PL',Quantity='Passive PL',data.frame(Age=pl_by_age$Age,PL=cumsum(pl_by_age$PassiveTodayPL))),
                         cbind(Type='Cumulative PL',Quantity='Active PL',data.frame(Age=pl_by_age$Age,PL=cumsum(pl_by_age$ActiveTodayPL))),
                         cbind(Type='Av. Market Value',Quantity='$ Value',data.frame(Age=pl_by_age$Age,PL=pl_by_age$MarketValue)),
                         cbind(Type='Av. Number positions',Quantity='N. Psns',data.frame(Age=pl_by_age$Age,PL=(pl_by_age$Indicator/252))),
                         cbind(Type='Av. Capital distribution',Quantity='$ at age',data.frame(Age=pl_by_age$Age,PL=pl_by_age$MarketValue*(pl_by_age$Indicator/252))))
cum_tpl_smmry <- ggplot(plt_cum_pl_data,aes(x=as.numeric(Age),y=PL,group=Quantity,colour=Quantity)) + 
                 geom_line(size=1) +
                 ylab("") + 
                 xlab("Position Age") + 
                 labs(colour="") +
                 ggtitle('Position PL and size by age') +
                 theme(legend.position = "bottom") +
                 theme(text = element_text(size=15)) +
                 facet_grid(Type~.,scales="free_y")

day_0_focus <- unique(history_data[c('TradeDate','Instrument','TodayPL','PassiveTodayPL','ActiveTodayPL','MarketRelPL','MinDate','MarketValue','Age','VolOutof')])
day_0_focus <- day_0_focus[day_0_focus$Age==0&!is.na(day_0_focus$Age),]
day_0_focus$MarketValue <- abs(day_0_focus$MarketValue)
day_0_focus$Swing <- (day_0_focus$VolOutof/10000)*day_0_focus$MarketValue
day_0_focus$TradeDate <- format(day_0_focus$TradeDate,'%Y-%m') 
day_0_focus <- aggregate(day_0_focus[c('TodayPL','MarketValue','Swing')],list(Date=day_0_focus$TradeDate),function(x)mean(x,na.rm=TRUE))
d0_plt <- ggplot(data=day_0_focus,aes(x=Date,y=TodayPL,size=MarketValue)) +
  geom_point(aes(colour=Swing)) +
  theme(text = element_text(size=15)) +
  ylab("Av. Day 0 PL") + 
  xlab("Month") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_size_continuous(guide = FALSE) +
  labs(colour="Av. $ Swing") +
  scale_colour_distiller(palette="Spectral") +
  ggtitle('Day 0 sizing and PL')  

