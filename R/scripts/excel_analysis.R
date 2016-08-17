#Script to load data from Excel files for rapid analysis
setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
options(java.parameters = "-Xmx8g" )
library(XLConnect)
library(sets)
library(plyr)
library(ggplot2)
source('excel_analysis_functions.R')
source("coaching_review_functions.r")

base_path <- "Z:/Christine/"

summary_tables <- list(Summary=list(start_header_col=1,end_header_col=1,start_header='Trading Day',end_header='Per|HSBC',start_col=1,end_col=39,start_offset=1,end_offset=-1))
data_tables <- list(Position=list(start_header_col=1,end_header_col=1,start_header='Ticker',end_header='Active Portfolio',start_col=1,end_col=33,start_offset=1,end_offset=-1),
                    Buys=list(start_header_col=1,end_header_col=1,start_header='Bought Positions',end_header='Sold Positions',start_col=1,end_col=33,start_offset=1,end_offset=-2),
                    Sells=list(start_header_col=1,end_header_col=2,start_header='Sold Positions',end_header='index|Attribution',start_col=1,end_col=33,start_offset=1,end_offset=-3),
                    Hedges=list(start_header_col=1,end_header_col=2,start_header='Short Hedges',end_header='Attribution Summary',start_col=1,end_col=33,start_offset=1,end_offset=-2))

years <- c('2016','2015','2014','2013')
data <- list()
first <- as.list(rep(TRUE,length(c(names(data_tables),names(summary_tables)))))
names(first) <- c(names(data_tables),names(summary_tables))
data_state <- list(data=data,state=first)
for(yr in years){
  sub_d <- list.files(path=paste(base_path,yr,sep=""))
  wb_dirs <- dir_findr(Map(function(x)paste(x[[1]],x[[2]]),strsplit(sub_d," ")))
  for(mnth in wb_dirs){
    m <- strsplit(mnth," ")[[1]][[2]]
    wb <- loadWorkbook(paste(base_path,yr,"/",mnth,"/HARDCODED-",m," Daily NAV_plain.xlsx",sep=""))  
    data_state <- extract_from_workbook(wb,'Summary',summary_tables,data_state[['data']],data_state[['state']])
    data_state <- extract_from_workbook(wb,m,data_tables,data_state[['data']],data_state[['state']])
  }
}
#Complete data-load:
saveRDS(data_state,paste(base_path,"data_extract.rds",sep=""))
data <- data_state[['data']]
psns <- subset_positions(data)
trds <- subset_trades(data)
history_data <- trade_history(psns,trds)
history_data <- history_data[!is.na(history_data$Instrument),]
history_data <- history_data[!is.na(history_data$TradeDate),]
saveRDS(history_data,paste(base_path,"history_data_raw.rds",sep=""))
#history_data<- readRDS(paste(base_path,"history_data_raw.rds",sep=""))

history_data$TradeDate <- as.Date(history_data$TradeDate)
history_data <- market_rel_pl(history_data,index='EEM')
saveRDS(history_data,paste(base_path,"history_data_rel.rds",sep=""))
cols <- c('Trader','TradeDate','Strategy','Instrument','ValueUSD','TodayPL','Long','MarketValue','EarliestMarketValue','MarketRelPL','ActiveTodayPL','PassiveTodayPL','MinDate')
history_data <- position_age_from_flats(history_data,cols)
saveRDS(history_data,paste(base_path,"history_data_age.rds",sep=""))
#history_data<- readRDS(paste(base_path,"history_data_age.rds",sep=""))

trade_typer <- function(history_data){
  history_data$TradeType <- 'NA'
  history_data$TradeType[history_data$Long==1&history_data$MarketValue>0&history_data$PsnAge!=0] <- 'Add Long'
  history_data$TradeType[(history_data$Long==0)&history_data$MarketValue>0&history_data$PsnAge!=0] <- 'Reduce Long'
  history_data$TradeType[history_data$Long==1&history_data$MarketValue<0&history_data$PsnAge!=0] <- 'Reduce Short'
  history_data$TradeType[(history_data$Long==0)&history_data$MarketValue<0&history_data$PsnAge!=0] <- 'Add Short'
  history_data$TradeType[(history_data$Long==1)&history_data$PsnAge==0] <- 'New Long'
  history_data$TradeType[(history_data$Long==0)&history_data$PsnAge==0] <- 'New Short'
  psn_increased <- aggregate(history_data$TradeType,list(Strategy=history_data$Strategy,Visit=history_data$Visit,Instrument=history_data$Instrument),function(x)sum(x=='Increase',na.rm=TRUE)>0)
  colnames(psn_increased) <- c('Strategy','Visit','Instrument','PsnIncreased')
  history_data <- merge(history_data,psn_increased,by=c('Strategy','Visit','Instrument'),all.x=TRUE)
  return(history_data)
}


#1. Summary

history_data<- readRDS(paste(base_path,"history_data_age.rds",sep=""))
summary_perf <- data[['Summary']][c('Daily.Perf...bps..1','Trading.Day')]
colnames(summary_perf) <- c('Return','TradeDate')
summary_perf$TradeDate <- as.Date(summary_perf$TradeDate)
index <- get_commodity_returns(min(summary_perf$TradeDate,na.rm=TRUE),max(summary_perf$TradeDate,na.rm=TRUE))
colnames(index) <- c("TradeDate","Return")
summary_perf$Return[is.na(summary_perf$Return)] <- 0
iperf <- merge(summary_perf[c("TradeDate")],index, by = c("TradeDate"),all.x=TRUE)
iperf$Return[is.na(iperf$Return)] <- 0
summary_perf <- summary_perf[order(summary_perf$TradeDate),]
summary_perf <- rbind(cbind(data.frame(Metric='MSCI EEM (%)',Value=cumsum(iperf$Return*100)),summary_perf),
                      cbind(data.frame(Metric='Cum. Return (%)',Value=(cumsum(summary_perf$Return)/100)),summary_perf),
                      cbind(Metric='Sharpe',compute_rolling_fn(summary_perf,'Value','Return',20,function(x)mean(x,na.rm=TRUE)/sd(x,na.rm=TRUE))),
                      cbind(Metric='GainToPain',compute_rolling_fn(summary_perf[c('TradeDate','Return')],'Value','Return',20,function(x)sum(x,na.rm=TRUE)/sum(abs(x[x<0]),na.rm=TRUE))))
sum_perf <- ggplot(summary_perf,aes(x=TradeDate,y=Value,group=Metric,colour=Metric)) +
            geom_line(size=1) +
  ylab("")+
  theme(axis.text=element_text(size=14),
        text = element_text(size=14),
        strip.text = element_text(size=14),
        title=element_text(size=16,face="bold")) +
            facet_grid(Metric~.,scales="free_y")

all_perf <- ggplot(history_data,aes(x=TradeDate,y=CumulativePL,colour=Instrument)) +
            geom_line(size=1) +
            facet_grid(Strategy~.)

mean(roll_alpha[format(roll_alpha$TradeDate,"%Y")=="2013"&roll_alpha$TradeType=='Add Long'&roll_alpha$Type=='Hit',]$Value,na.rm=TRUE)
mean(roll_alpha[format(roll_alpha$TradeDate,"%Y")=="2014"&roll_alpha$TradeType=='Add Long'&roll_alpha$Type=='Hit',]$Value,na.rm=TRUE)
mean(roll_alpha[format(roll_alpha$TradeDate,"%Y")=="2015"&roll_alpha$TradeType=='Add Long'&roll_alpha$Type=='Hit',]$Value,na.rm=TRUE)
mean(roll_alpha[format(roll_alpha$TradeDate,"%Y")=="2016"&roll_alpha$TradeType=='Add Long'&roll_alpha$Type=='Hit',]$Value,na.rm=TRUE)

mean(roll_alpha[format(roll_alpha$TradeDate,"%Y")=="2013"&roll_alpha$TradeType=='Add Long'&roll_alpha$Type=='Loss',]$Value,na.rm=TRUE)
mean(roll_alpha[format(roll_alpha$TradeDate,"%Y")=="2014"&roll_alpha$TradeType=='Add Long'&roll_alpha$Type=='Loss',]$Value,na.rm=TRUE)
mean(roll_alpha[format(roll_alpha$TradeDate,"%Y")=="2015"&roll_alpha$TradeType=='Add Long'&roll_alpha$Type=='Loss',]$Value,na.rm=TRUE)
mean(roll_alpha[format(roll_alpha$TradeDate,"%Y")=="2016"&roll_alpha$TradeType=='Add Long'&roll_alpha$Type=='Loss',]$Value,na.rm=TRUE)

mean(roll_alpha[format(roll_alpha$TradeDate,"%Y")=="2013"&roll_alpha$TradeType=='Add Long'&roll_alpha$Type=='Win',]$Value,na.rm=TRUE)
mean(roll_alpha[format(roll_alpha$TradeDate,"%Y")=="2014"&roll_alpha$TradeType=='Add Long'&roll_alpha$Type=='Win',]$Value,na.rm=TRUE)
mean(roll_alpha[format(roll_alpha$TradeDate,"%Y")=="2015"&roll_alpha$TradeType=='Add Long'&roll_alpha$Type=='Win',]$Value,na.rm=TRUE)
mean(roll_alpha[format(roll_alpha$TradeDate,"%Y")=="2016"&roll_alpha$TradeType=='Add Long'&roll_alpha$Type=='Win',]$Value,na.rm=TRUE)

stat_pl <- aggregate(history_data['TodayPL'],list(TradeDate=history_data$TradeDate,Strategy=history_data$Strategy),function(x)sum(x,na.rm=TRUE))
stat_pl <- stat_pl[order(stat_pl$TradeDate),]
stat_pl$CumulativePL <- NA
stat_pl$CumulativePL[stat_pl$Strategy=='HEDGE'] <- cumsum(stat_pl$TodayPL[stat_pl$Strategy=='HEDGE'])
stat_pl$CumulativePL[stat_pl$Strategy=='Aperios'] <- cumsum(stat_pl$TodayPL[stat_pl$Strategy=='Aperios'])
strat_perf <- ggplot(stat_pl,aes(x=TradeDate,y=CumulativePL,group=Strategy,colour=Strategy)) +
              geom_line(size=1) 

psns <- aggregate(history_data$Instrument,list(TradeDate=history_data$TradeDate),function(x)length(x))
psns_plot <- ggplot(psns,aes(x=TradeDate,y=x))+
             geom_line(size=1)          
history_data$Month <- format(history_data$TradeDate,"%Y-%m")
to <- aggregate(history_data$ValueUSD,list(Month=history_data$Month),function(x)sum(x,na.rm=TRUE))
to_plot <- ggplot(to,aes(x=as.character(Month),y=x))+
  geom_line(size=1)          

mn <- aggregate(history_data$ValueUSD,list(TradeDate=history_data$TradeDate),function(x)mean(x,na.rm=TRUE))
median(history_data$PsnAge,na.rm=TRUE)

#2. Alpha process

history_data<- readRDS(paste(base_path,"history_data_raw.rds",sep=""))
instruments <- unique(history_data$Instrument)
first <- TRUE
for(ins in instruments){
  df <- unique(history_data[history_data$Instrument==ins,c('TradeDate','Instrument','TodayPL','ClosePrice')])
  df <- df[order(df$TradeDate),]
  window <- min(nrow(df),20)
  roll_plout <- compute_rolling_fn(df[c('TradeDate','Instrument','TodayPL')],'PlOut20','TodayPL',window,function(x)sum(x,na.rm=TRUE))
  roll_stockrtn <- compute_rolling_fn(df[c('TradeDate','Instrument','ClosePrice')],'RtnOut20','ClosePrice',window,function(x)(x[!is.na(x)][2]/x[!is.na(x)][1])-1)
  if(first){
    plout <- roll_plout
    srtn <-  roll_stockrtn
    first <- FALSE
  } else {
    plout <- rbind(plout,roll_plout)
    srtn <-  rbind(roll_stockrtn,srtn)
  }
}
history_data<- readRDS(paste(base_path,"history_data_age.rds",sep=""))
alpha_data$RtnOut20[is.na(alpha_data$RtnOut20)] <- 0
alpha_data$RtnOut20[is.na(alpha_data$PlOut20)] <- 0
alpha_data <- merge(history_data,plout[c('TradeDate','Instrument','PlOut20')],by=c('TradeDate','Instrument'))
alpha_data <- merge(alpha_data,srtn[c('TradeDate','Instrument','RtnOut20')],by=c('TradeDate','Instrument'))
alpha_data$RtnOut20[is.na(alpha_data$RtnOut20)] <- 0
alpha_data$RtnOut20[is.na(alpha_data$PlOut20)] <- 0
alpha_data <- trade_typer(alpha_data)
alpha_data$Hit <- NA
alpha_data$Hit[!is.na(alpha_data$TradeID)] <- alpha_data$RtnOut20[!is.na(alpha_data$TradeID)]>0
alpha_data$StockWn <- NA
alpha_data$StockWn[!is.na(alpha_data$TradeID)&alpha_data$RtnOut20>0] <- alpha_data$RtnOut20[!is.na(alpha_data$TradeID)&alpha_data$RtnOut20>0]
alpha_data$StockLs <- NA
alpha_data$StockLs[!is.na(alpha_data$TradeID)&alpha_data$RtnOut20<0] <- alpha_data$RtnOut20[!is.na(alpha_data$TradeID)&alpha_data$RtnOut20<0]

alpha_stats <- aggregate(alpha_data[!(alpha_data$TradeType=='NA'),c('Hit','StockWn','StockLs','ValueUSD')],list(TradeType=alpha_data[!(alpha_data$TradeType=='NA'),]$TradeType,TradeDate=alpha_data[!(alpha_data$TradeType=='NA'),]$TradeDate),function(x)mean(x,na.rm=TRUE))
roll_hit <- compute_rolling_fn(alpha_stats[c('TradeType','TradeDate','Hit')],'Value','Hit',20,function(x)mean(x,na.rm=TRUE))
roll_win <- compute_rolling_fn(alpha_stats[c('TradeType','TradeDate','StockWn')],'Value','StockWn',20,function(x)mean(x,na.rm=TRUE))
roll_loss <- compute_rolling_fn(alpha_stats[c('TradeType','TradeDate','StockLs')],'Value','StockLs',20,function(x)mean(x,na.rm=TRUE))
roll_size <- compute_rolling_fn(alpha_stats[c('TradeType','TradeDate','ValueUSD')],'Value','ValueUSD',20,function(x)mean(x,na.rm=TRUE))
roll_alpha <- rbind(cbind(Type='Hit',roll_hit[c('TradeType','TradeDate','Value')]),cbind(Type='Loss',roll_loss[c('TradeType','TradeDate','Value')]),cbind(Type='Win',roll_win[c('TradeType','TradeDate','Value')]),cbind(Type='Size',roll_size[c('TradeType','TradeDate','Value')]))

alpha_plt <- ggplot(roll_alpha[roll_alpha$TradeType%in%c('Add Long','New Long'),],aes(x=TradeDate,y=Value,group=TradeType,color=TradeType)) +
         geom_line(size=1) +
  theme(axis.text=element_text(size=14),
        text = element_text(size=14),
        strip.text = element_text(size=14),
        title=element_text(size=16,face="bold")) +
         facet_grid(Type~TradeType,scales="free_y")

#alpha_data$ValueUSD <- scale(alpha_data$ValueUSD)
#alpha_data$ValueUSD <- scale(alpha_data$ValueUSD)

#3. Position management

#Offside
hd <- unique(history_data[c('TradeDate','Instrument','CumulativePL','CumulativeMarketRelPL','MarketRelPL','TodayPL','MarketValue','MinDate','PsnAge')])
hd <- hd[hd$PsnAge<50,]
hd <- hd[hd$Strategy!='HEDGE',]
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

#Average down
avg_dwn_cols <- c('Instrument','TradeDate','TodayPL','MarketValue','Long','PsnLong','CumulativePL','MarketRelPL','CumulativeMarketRelPL','ValueUSD')
hd <- history_data
hd$PsnLong <- hd$MarketValue >=0
average_down_trades <- unique(hd[(hd$CumulativePL<0)&!is.na(hd$TradeID)&hd$PsnAge>0,avg_dwn_cols])
average_down_trades <- subset(average_down_trades,(average_down_trades$Long&average_down_trades$PsnLong)|(!average_down_trades$Long&!average_down_trades$PsnLong))
average_down_positions <- unique(average_down_trades$Instrument)
other_trades <- unique(hd[(hd$CumulativePL>0)&!is.na(hd$TradeID),avg_dwn_cols])

pl_frame <- merge(hd,data.frame(Instrument=unique(average_down_trades$Instrument)),by=c('Instrument'))
pl_frame <- unique(pl_frame[c(avg_dwn_cols)])
pl_frame$TradeCount <- NA
pl_frame$PsnAge <- NA
for(ins in unique(average_down_trades$Instrument)){
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

#Holding time
pl_hd <- history_data
pl_hd$Indicator <- 1
pl_by_age <- aggregate(pl_hd[c('VisitCumulativePL','VisitCumulativePassivePL','VisitCumulativeActivePL','VisitCumulativeMarketRelPL','Indicator')],list(Age=pl_hd$PsnAge),function(x)sum(x,na.rm=TRUE))
pl_by_age <- merge(pl_by_age,aggregate(pl_hd['MarketValue'],list(Age=pl_hd$PsnAge),function(x)mean(x,na.rm=TRUE)),by='Age')

plt_cum_pl_data <- rbind(cbind(Type='Cumulative PL',Quantity='Total PL',data.frame(Age=pl_by_age$Age,PL=pl_by_age$VisitCumulativePL)),
                         cbind(Type='Cumulative PL',Quantity='MarketRel PL',data.frame(Age=pl_by_age$Age,PL=pl_by_age$VisitCumulativeMarketRelPL)),
                         cbind(Type='Cumulative PL',Quantity='Passive PL',data.frame(Age=pl_by_age$Age,PL=pl_by_age$VisitCumulativePassivePL)),
                         cbind(Type='Cumulative PL',Quantity='Active PL',data.frame(Age=pl_by_age$Age,PL=pl_by_age$VisitCumulativeActivePL)),
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

#Revisits
revisit_plt_data <- aggregate(history_data[c('TodayPL')],list(Visit=history_data$Visit),sum)
revisit_smmry <- ggplot(revisit_plt_data,aes(x=Visit)) +
  geom_bar(position="dodge",aes(weight=x)) +
  scale_fill_distiller(palette = "Spectral") +
  ylab("") + 
  xlab("Total visits to stock") + 
  labs(fill="Visit number") +
  ggtitle('PL by number of visits to a stock') +
  theme(text = element_text(size=15)) +
  facet_grid(Quantity~.,scales="free_y")

#Drawdown
history_data<- readRDS(paste(base_path,"history_data_age.rds",sep=""))
drawdown_data <- aggregate(history_data[c('TodayPL','MarketValue')],list(TradeDate=history_data$TradeDate,Strategy=history_data$Strategy),function(x)sum(x,na.rm=TRUE))
drawdown_data <- drawdown_data[order(drawdown_data$TradeDate),]
drawdown_data$CumulativePL <- NA
drawdown_data$CumulativePL[drawdown_data$Strategy=='HEDGE'] <- cumsum(drawdown_data$TodayPL[drawdown_data$Strategy=='HEDGE'])
drawdown_data$CumulativePL[drawdown_data$Strategy=='Aperios'] <- cumsum(drawdown_data$TodayPL[drawdown_data$Strategy=='Aperios'])
drawdown <- rbind(compute_rolling_fn(drawdown_data[drawdown_data$Strategy=='HEDGE',c('TradeDate','Strategy','CumulativePL')],'DrawDown20','CumulativePL',20,function(x)(x[length(x)]-x[1])),
                  compute_rolling_fn(drawdown_data[drawdown_data$Strategy=='Aperios',c('TradeDate','Strategy','CumulativePL')],'DrawDown20','CumulativePL',20,function(x)(x[length(x)]-x[1])))
size <- compute_rolling_fn(drawdown_data[c('TradeDate','Strategy','MarketValue')],'Value20','MarketValue',20,function(x)mean(x,na.rm=TRUE))
drawdown_plot <- ggplot(drawdown,aes(x=TradeDate,y=DrawDown20,group=Strategy,colour=Strategy)) +
                 geom_line(size=1)
size_plot <- ggplot(size,aes(x=TradeDate,y=Value20,group=Strategy,colour=Strategy)) +
             geom_line(size=1)

#Number and $ value of trades by instrument over time
library(scales)
history_data<- readRDS(paste(base_path,"history_data_raw.rds",sep=""))
history_data$Month <- as.Date(paste(format(history_data$TradeDate,"%Y-%m"),"-01",sep=""))
history_data$Currency <- tolower(history_data$Currency)
history_data$PctNAV[is.nan(history_data$PctNAV)] <- NA
history_data$PctNAV[is.infinite(history_data$PctNAV)] <- NA
history_data$Type[is.na(history_data$Type)] <- 'Cash Equity'
history_data <- unique(history_data[!is.na(history_data$ValueUSD),c('Type','Month','TradeShares','ValueUSD')])
to_agg <- aggregate(history_data[c('TradeShares','ValueUSD')],list(Type=history_data$Type,Month=history_data$Month),function(x)sum(x,na.rm=TRUE))
colnames(to_agg) <- c('Type','Month','TradeShares','ValueUSD')
plot_to <- rbind(cbind(Qty='Shares',to_agg[c('Type','Month')],data.frame(Value=to_agg$TradeShares)),
                 cbind(Qty='$ Value',to_agg[c('Type','Month')],data.frame(Value=to_agg$ValueUSD)))
to_plot <- ggplot(plot_to,aes(x=Month,fill=Type)) +
           #geom_line(size=1) +
           #geom_point() +
           #geom_smooth() +
           geom_bar(aes(weight=Value))+
           scale_y_continuous(name="", labels = comma) +
           ylab("") +
           xlab("Date") +
           theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
           facet_grid(Qty~Type,scale="free_y")

to_count <- aggregate(history_data$ValueUSD,list(Type=history_data$Type,Month=history_data$Month),function(x)length(x))
to_count_all <- aggregate(history_data$ValueUSD,list(Month=history_data$Month),function(x)length(x)) 
to_counts <- rbind(to_count,
                   cbind(Type="All types",to_count_all))

count_plot <- ggplot(to_counts[to_counts$Month>='2015-01-01',],aes(x=Month,fill=Type)) +
  #geom_line(size=1) +
  #geom_point() +
  #geom_smooth() +
  geom_bar(aes(weight=x))+
  scale_y_continuous(name="", labels = comma) +
  ylab("Number of trades") +
  xlab("Date") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(Type~.,scale="free_y")

