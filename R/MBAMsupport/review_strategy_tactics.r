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

#ToDo:
# 1. Permit arbitrary date range for factor exposures
# 2. Get PL/Rtn by strategy from trading analysis? (strategy performance summary module)

trader   <- 70
initial  <- 'BA'
dates <- c("2016-04-01")
history_data <- analysis_module_load_multiple(trader,dates,history_analysis_module_builder,dated_twelve_monthly_lookback)
thisQ <- quarter(as.Date(dates)-1)

#Get exposure data from database
cn <- odbcConnect('RAIDLIVEDB',uid='guy.billings')
sqlQuery(cn,'USE Razor')
factor_exposure_SQL <- "prFactorRisk_GetScores_TraderAnalytics"
factor_exposures <- sqlQuery(cn,factor_exposure_SQL)
close(cn)

#1. Mean allocation, turnover by strategy
psn_data <- unique(history_data[c('Strategy','Instrument','TradeDate','MarketValue','TodayPL')])
psn_data$Q <- quarter(psn_data$TradeDate)
strat_sz_smrry <- aggregate(psn_data['MarketValue'],list(Strategy=psn_data$Strategy,Quarter=psn_data$Q,Date=psn_data$TradeDate),function(x)sum(abs(x),na.rm=TRUE))
strat_sz_smrry <- aggregate(strat_sz_smrry['MarketValue'],list(Strategy=strat_sz_smrry$Strategy,Quarter=(strat_sz_smrry$Q==thisQ)),function(x)mean(x,na.rm=TRUE))
strat_pl_smrry <- aggregate(psn_data['TodayPL'],list(Strategy=psn_data$Strategy,Quarter=psn_data$Q),function(x)sum(x,na.rm=TRUE))
strat_pl_smrry <- aggregate(strat_pl_smrry['TodayPL'],list(Strategy=strat_pl_smrry$Strategy,Quarter=(strat_pl_smrry$Q==thisQ)),function(x)mean(x,na.rm=TRUE))
strat_n_psns <- aggregate(psn_data['Instrument'],list(Strategy=psn_data$Strategy,Quarter=psn_data$Q),function(x)length(unique(x[!is.na(x)])))
strat_n_psns <- aggregate(strat_n_psns['Instrument'],list(Strategy=strat_n_psns$Strategy,Quarter=(strat_n_psns$Q==thisQ)),function(x)mean(x,na.rm=TRUE))

trd_data <- unique(history_data[c('Strategy','Instrument','TradeDate','ValueUSD','TodayPL','TradeID')])
trd_data$Q <- quarter(trd_data$TradeDate)
trd_data <- trd_data[!is.na(trd_data$TradeID),]
trd_pl_smrry <- aggregate(trd_data[c('TodayPL','ValueUSD')],list(Strategy=trd_data$Strategy,Quarter=trd_data$Q),function(x)sum(x,na.rm=TRUE))
trd_pl_smrry <- aggregate(trd_pl_smrry[c('TodayPL','ValueUSD')],list(Strategy=trd_pl_smrry$Strategy,Quarter=(trd_pl_smrry$Q==thisQ)),function(x)mean(x,na.rm=TRUE))
trd_n_trds <- aggregate(trd_data['Instrument'],list(Strategy=trd_data$Strategy,Quarter=trd_data$Q),function(x)length(unique(x[!is.na(x)])))
trd_n_trds <- aggregate(trd_n_trds['Instrument'],list(Strategy=trd_n_trds$Strategy,Quarter=(trd_n_trds$Q==thisQ)),function(x)mean(x,na.rm=TRUE))

strategy_data <- rbind(cbind(Type='Position level',Quantity='Value',Value=strat_sz_smrry$MarketValue,strat_sz_smrry[c('Strategy','Quarter')]),
                       cbind(Type='Position level',Quantity='PL',Value=strat_pl_smrry$TodayPL,strat_pl_smrry[c('Strategy','Quarter')]),
                       cbind(Type='Position level',Quantity='Count',Value=strat_n_psns$Instrument,strat_n_psns[c('Strategy','Quarter')]),
                       cbind(Type='Trade level',Quantity='Value',Value=trd_pl_smrry$ValueUSD,trd_pl_smrry[c('Strategy','Quarter')]),
                       cbind(Type='Trade level',Quantity='PL',Value=trd_pl_smrry$TodayPL,trd_pl_smrry[c('Strategy','Quarter')]),
                       cbind(Type='Trade level',Quantity='Count',Value=trd_n_trds$Instrument,trd_n_trds[c('Strategy','Quarter')]))

plt_strat <- ggplot(data=strategy_data, aes(x=Strategy, fill=Quarter)) +
  geom_bar(aes(weight=Value),position="dodge") +
  facet_grid(Quantity~Type, scales="free_y") +
  ylab("") + xlab("Strategy") + ggtitle('Strategy breakdown') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

pie_data <- function(strategy_data){
  sd <- strategy_data
  sdagg <- aggregate(sd['Value'],list(Type=sd$Type),sum)
  sd <- merge(sd,sdagg,by='Type')
  sd$Value <- 100*(sd$Value.x/sd$Value.y)
  return(sd)
}
sd_quarter <- pie_data(strategy_data[strategy_data$Quantity=='Value'&strategy_data$Quarter,])
sd_history <- pie_data(strategy_data[strategy_data$Quantity=='Value'&!strategy_data$Quarter,])
sd <- merge(sd_quarter[c('Type','Value','Strategy')],sd_history[c('Type','Value','Strategy')],by=c('Type','Strategy'))
sd$Value <- sd$Value.x
sd$Delta <- sd$Value.x - sd$Value.y
sd$breaks[sd$Type=='Position level'] <- cumsum(sd$Value[sd$Type=='Position level']) - sd$Value[sd$Type=='Position level']/2
sd$breaks[sd$Type=='Trade level'] <- cumsum(sd$Value[sd$Type=='Trade level']) - sd$Value[sd$Type=='Trade level']/2
sd$angle[sd$Type=='Position level'] <- (90-360*(sd$breaks[sd$Type=='Position level']/sum(sd$Value[sd$Type=='Position level'])))
sd$angle[sd$Type=='Trade level'] <- (90-360*(sd$breaks[sd$Type=='Trade level']/sum(sd$Value[sd$Type=='Trade level'])))
sd$dlabel <- NA
sd$dlabel[abs(sd$Delta)>5] <- sd$Delta[abs(sd$Delta)>5]
lbels <- paste(round(sd$dlabel),"%",sep="")
lbels <- gsub("NA%","",lbels)

sd$Type <- as.character(sd$Type)
sd$Type[sd$Type=='Position level'] <- 'AUM'
sd$Type[sd$Type=='Trade level'] <- 'Turnover'
pie_smmry <- ggplot(data=sd, aes(x=factor(1), y=Value, fill=Strategy)) +
  geom_bar(stat="identity") +
  geom_text(aes(x= factor(1.2), y=breaks, label = Strategy, angle=angle),size=3) +  
  geom_text(aes(x= factor(1), y=breaks, label = lbels),size=4,fontface='bold') +  
  theme(legend.position = "none",axis.ticks = element_blank(),panel.background = element_rect(fill = "white")) +
  coord_polar(theta = "y") +
  scale_y_continuous(breaks=NULL) +
  scale_x_discrete(breaks=NULL) +
  labs(x="",y="") +
  ggtitle('AUM and turnover by strategy') +
  facet_grid(facets = Type~.) 

#2. PL by strategy and PL/turnover by signal
pl_data <- strategy_data[strategy_data$Type=='Position level'&strategy_data$Quantity=='PL',]
pl_data <- merge(pl_data[pl_data$Quarter,],pl_data[!pl_data$Quarter,],by=c('Type','Quantity','Strategy'))
pl_data$Value <- pl_data$Value.x
pl_data$Delta <- pl_data$Value.x - pl_data$Value.y
pl_data$Delta <- 100*(pl_data$Delta/abs(pl_data$Value.y))

lbls <- paste(round(pl_data$Delta),"%",sep="")
lbls <- gsub('NaN%','',lbls)
pl_smmry <- ggplot(data=pl_data, aes(x=reorder(Strategy,Value), fill=Strategy)) +
  geom_bar(aes(weight=Value)) +
  coord_flip() +
  geom_text(aes(x= Strategy, y=Value, label = lbls),size=3) +  
  theme(legend.position = "none") +
  ylab("PL $") + xlab("Strategy") + ggtitle('Total PL and PL change by strategy') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

sig_hist <- history_data[!is.na(history_data$TradeID),]
sig_hist <- unique(sig_hist[c("Instrument","TradeID","TradeDate","Strategy","Long","ValueUSD","TodayPL","Special.Dividend","Results","Close.Period","Dividend","Trading.Statement","AGM","Director.Sell.Non.Core","Conference.Call","Road.Show","Director.Buy.Non.Core", "Director.Sell.Core","Secondary.Placing" , "Director.Buy.Core","Stock.Split" ,"Shareholder.Meeting" , "Rights.Issue", "Index.Add.Confirmed", "Monthly.Unit.Sales","Index.Reweight.Increase.Expected","Index.Reweight.Reduce.Expected","Index.Reweight.Increase.Confirmed","Index.Remove.Expected" ,"Index.Remove.Confirmed",  "Primary.Placing", "New.Issue")])
sig_hist$Q <- quarter(sig_hist$TradeDate)
acols <- c("Special.Dividend","Results","Close.Period","Dividend","Trading.Statement","AGM","Director.Sell.Non.Core","Conference.Call","Road.Show","Director.Buy.Non.Core", "Director.Sell.Core","Secondary.Placing" , "Director.Buy.Core","Stock.Split" ,"Shareholder.Meeting" , "Rights.Issue", "Index.Add.Confirmed", "Monthly.Unit.Sales","Index.Reweight.Increase.Expected","Index.Reweight.Reduce.Expected","Index.Reweight.Increase.Confirmed","Index.Remove.Expected" ,"Index.Remove.Confirmed",  "Primary.Placing", "New.Issue")
first <-  TRUE
for(signal in acols){
  sh <- sig_hist[!is.na(sig_hist[[signal]]),]
  sh <- sh[sh[[signal]]==TRUE,]
  if(nrow(sh)>0){
    sig_to <- aggregate(sh[c('ValueUSD','TodayPL')],list(Long=sh$Long,Strategy=sh$Strategy,Quarter=sh$Q),function(x)sum(x,na.rm=TRUE))
    sig_to <- aggregate(sig_to[c('ValueUSD','TodayPL')],list(Long=sig_to$Long,Strategy=sig_to$Strategy,Quarter=(sig_to$Q==thisQ)),function(x)mean(x,na.rm=TRUE))
    sig_to <- cbind(Signal=signal,sig_to)
    if(first){
      all_sig <- sig_to
      first <- FALSE
    }
    else{
      all_sig <- rbind(all_sig,sig_to)
    } 
  }
}
all_sig$Signal <- gsub("\\."," ",all_sig$Signal)
plt_sig_value <- ggplot(data=all_sig, aes(x=Signal, fill=Quarter)) +
  geom_bar(aes(weight=ValueUSD),position="dodge") +
  facet_grid(Strategy~Long, scales="free_y") +
  ylab("Value traded") + xlab("Strategy") + ggtitle('Strategy breakdown') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
plt_sig_pl <- ggplot(data=all_sig, aes(x=Signal, fill=Quarter)) +
  geom_bar(aes(weight=TodayPL),position="dodge") +
  facet_grid(Strategy~Long, scales="free_y") +
  ylab("PL on trade day") + xlab("Strategy") + ggtitle('Strategy breakdown') +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

sig_increases <- all_sig[grep(paste(initial,'_L',sep=""),all_sig$Strategy),]
sig_increases <- sig_increases[sig_increases$Long==1,]
s_sig <- all_sig[grep(paste(initial,'_S',sep=""),all_sig$Strategy),]
s_sig <- s_sig[s_sig$Long==0,]
sig_increases <- rbind(sig_increases,s_sig)
sig_increases <- aggregate(sig_increases[c('ValueUSD','TodayPL')],list(Signal=sig_increases$Signal,Quarter=sig_increases$Quarter),sum)
sig_increases <- merge(sig_increases[sig_increases$Quarter,],sig_increases[!sig_increases$Quarter,],by=c('Signal'))
sig_increases$ValueUSD <- sig_increases$ValueUSD.x
sig_increases$TodayPL <- sig_increases$TodayPL.x
sig_increases$Delta <- 100*((sig_increases$TodayPL.x/sig_increases$ValueUSD.x)-(sig_increases$TodayPL.y/sig_increases$ValueUSD.y))/abs(sig_increases$TodayPL.y/sig_increases$ValueUSD.y)
sig_increases$Increase <- sig_increases$Delta > 0
sig_increases <- sig_increases[order(sig_increases$ValueUSD),]
sig_increases$Rank <- 1:nrow(sig_increases)
sig_increases <- rbind(cbind(Quantity="PL",sig_increases[c('Signal','Rank','Delta','Increase')],Value=sig_increases$TodayPL),
                       cbind(Quantity="Traded",sig_increases[c('Signal','Rank','Delta','Increase')],Value=sig_increases$ValueUSD))

sig_increases$Quantity <- as.character(sig_increases$Quantity)
sig_increases$Quantity[sig_increases$Quantity=="PL"] <- "PL on trade day"
sig_increases$Quantity[sig_increases$Quantity=="Traded"] <- "Turnover"
sig_smmry <- ggplot(data=sig_increases, aes(x=reorder(Signal,Rank),fill=Increase)) +
  geom_bar(aes(weight=Value)) +
  facet_grid(Quantity~., scales="free_y") +
  ylab("$") + xlab("") + ggtitle('PL and turnover by event') +
  labs(fill="Increased")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#3. Strategy signal characteristic and effeciveness
crt_cnv <-  function(x,y,r){
  ang <- asin(x/r)
  if(x>0&&y>0){
    rval <- ang
  }
  else if(x>0&&y<0){
    rval <- ang+(pi/2)
  }
  else if(x<0&&y<0){
    rval <- ang+(3*pi/2)
  }
  else{
    rval <- ang+2*pi
  }
  return(rval)
}
lst_cnv <- function(x,y,r)unlist(Map(crt_cnv,x,y,r))
signal_map <- data.frame(Type=c(rep('Company Event',3),rep('Corporate Action',6),rep('Dividend',2),rep('Index',5),rep('Insiders',4),rep('Results',4)),
                         Signal=c('AGM','Road Show','Shareholder Meeting','Primary Placing','Secondary Meeting','Close Period','Stock Split','Rights Issue','New Issue','Special Dividend','Dividend','Index Add Confirmed','Index Reweight Increase Expected','Index Reweight Reduce Expected','Index Reweight Increase Confirmed','Index Remove Expected','Director Buy Core','Director Sell Core','Director Buy Non Core','Director Sell Non Core','Trading Statement','Results','Conference Call','Monthly Unit Sales'),
                         SCoord=1:24)

sig_to <- aggregate(all_sig['ValueUSD'],list(Signal=all_sig$Signal,Strategy=all_sig$Strategy,Quarter=all_sig$Quarter),sum)
rtn <- all_sig[grep(paste(initial,'_L',sep=""),all_sig$Strategy),]
rtn <- rtn[rtn$Long==1,]
s_rtn <- all_sig[grep(paste(initial,'_S',sep=""),all_sig$Strategy),]
s_rtn <- s_rtn[s_rtn$Long==0,]
rtn <- rbind(rtn,s_rtn) 
rtn$Yeild <- 1+(rtn$TodayPL/rtn$ValueUSD)
sig_to <- sig_to[c('Quarter','Signal','Strategy','ValueUSD')]
sig_to <- rbind(data.frame(Quarter=TRUE,Signal=rep(unique(sig_to$Signal),length(unique(sig_to$Strategy))),Strategy=rep(unique(sig_to$Strategy),length(unique(sig_to$Signal))),ValueUSD=0),
                data.frame(Quarter=FALSE,Signal=rep(unique(sig_to$Signal),length(unique(sig_to$Strategy))),Strategy=rep(unique(sig_to$Strategy),length(unique(sig_to$Signal))),ValueUSD=0),
                sig_to)
sig_to <- aggregate(sig_to['ValueUSD'],list(Signal=sig_to$Signal,Strategy=sig_to$Strategy,Quarter=sig_to$Quarter),sum)
sig_to <- merge(sig_to,signal_map,by='Signal')
sig_to <- merge(sig_to,rtn[c('Signal','Strategy','Quarter','Yeild')],by=c('Signal','Strategy','Quarter'),all.x=TRUE)
sig_to$Yeild[is.na(sig_to$Yeild)] <-0
fd <- sig_to
sig_to$x <- sin(2*pi*(sig_to$SCoord-1)/24)*sig_to$ValueUSD
sig_to$y <- cos(2*pi*(sig_to$SCoord-1)/24)*sig_to$ValueUSD
sig_to$yld_y <- cos(2*(pi*sig_to$SCoord-1)/24)*sig_to$Yeild
sig_to$yld_x <- sin(2*(pi*sig_to$SCoord-1)/24)*sig_to$Yeild
sig_to$Yeild <- sig_to$Yeild*sig_to$ValueUSD
sig_to <- aggregate(sig_to[c('x','y','yld_x','yld_y','ValueUSD','Yeild')],list(Strategy=sig_to$Strategy,Quarter=sig_to$Quarter),sum)
sig_to$Direction <- lst_cnv(sig_to$x,sig_to$y,sig_to$ValueUSD)
sig_to$Direction <- 24*sig_to$Direction/(2*pi)
sig_to$ValueUSD <- sqrt(sig_to$x^2+sig_to$y^2)
sig_to$YeildDir <- lst_cnv(sig_to$yld_x,sig_to$yld_y,sig_to$Yeild)
sig_to$YeildDir <- 24*sig_to$YeildDir/(2*pi)
sig_to$Yeild <- sqrt(sig_to$yld_x^2+sig_to$yld_y^2)

sig_to <- cbind(Type="Turnover",sig_to[c('Strategy','Quarter')],Direction=sig_to$Direction,Value=log(sig_to$ValueUSD))
fd <- cbind(Type="Turnover",fd[c('Strategy','Quarter')],Direction=fd$SCoord,Value=log(fd$ValueUSD))

sig_to$Quarter <- as.character(sig_to$Quarter)
sig_to$Quarter[sig_to$Quarter=='TRUE'] <- 'This Q'
sig_to$Quarter[sig_to$Quarter=='FALSE'] <- 'Previous 3Q'
fd$Quarter <- as.character(fd$Quarter)
fd$Quarter[fd$Quarter=='TRUE'] <- 'This Q'
fd$Quarter[fd$Quarter=='FALSE'] <- 'Previous 3Q'
sig_strat <- ggplot(data=sig_to, aes(x = Direction, y = Value)) + 
  coord_polar() + 
  geom_segment(data=fd,aes(y=0, xend = Direction, yend = Value, colour=Quarter)) +
  geom_segment(aes(y = 0, xend = Direction, yend = Value, colour=Quarter),arrow = arrow(length = unit(0.1,"npc")), size = 1.2) +
  scale_x_continuous(breaks = c(2,6,10,14,18,22), labels = unique(as.character(signal_map$Type))) +
  ylab("") + xlab("") + ggtitle('Overall event type by strategy') +
  theme(axis.text.y = element_blank(),axis.ticks.y = element_blank(),legend.title=element_blank(),legend.position="bottom") +
  facet_wrap(~Strategy, ncol=3)

#4. Factor exposures
factor_data <- factor_exposures[c('lInstrumentID','dtDateTime','sFactorName','dblZScore')]
colnames(factor_data) <- c('Instrument','TradeDate','FactorName','ZScore')
port <- portfolio_decomposition(history_data)
factor_summary <- merge(port,factor_data,by=c('Instrument','TradeDate'))
only_factors <- c('rValue','rStrength','rGrowth','rSize','rSectorTrendExtension','rStreetSentiment','rPriceMomentum1M','rPriceMomentum12M','rTrendExtension','rEarnings','rVolatility')
factor_summary <- factor_summary[factor_summary$FactorName%in%only_factors,]
factor_summary$TotalExposure <- factor_summary$ZScore*factor_summary$Weight
factor_summary$TradedExposure <- factor_summary$ZScore*factor_summary$ActiveWeight
factor_summary$CoreExposure <- factor_summary$ZScore*factor_summary$CoreWeight
factor_summary$Q <- quarter(factor_summary$TradeDate)
fct_smmry <- aggregate(factor_summary[c('TotalExposure')],list(FactorName=factor_summary$FactorName,Quarter=factor_summary$Q,Date=factor_summary$TradeDate),function(x)sum(x,na.rm=TRUE))
fct_smmry <- aggregate(fct_smmry[c('TotalExposure')],list(Quarter=(fct_smmry$Q==thisQ),Factor=fct_smmry$FactorName),function(x)mean(x,na.rm=TRUE))
fct_smmry <- merge(fct_smmry[fct_smmry$Quarter,],fct_smmry[!fct_smmry$Quarter,],by='Factor')
fct_smmry$Delta <- 100*(fct_smmry$TotalExposure.x - fct_smmry$TotalExposure.y)/abs(fct_smmry$TotalExposure.y)
fct_smmry$TotalExposure <- fct_smmry$TotalExposure.x

clipper <- function(x)substr(x,2,nchar(x))
fct_smmry$Factor <- clipper(as.character(fct_smmry$Factor))
exprs_smmry <- ggplot(data=fct_smmry, aes(x=reorder(Factor,TotalExposure), fill=Factor)) +
  geom_bar(aes(weight=TotalExposure)) +
  ylab("Exposure") + xlab("Factor") + ggtitle('Factor exposure') +
  geom_text(aes(x= Factor, y=TotalExposure, label = paste(round(Delta),"%",sep="")),size=4) +  
  theme(axis.text.x = element_text(angle = 90, hjust = 1),legend.position = "none") +
  scale_fill_brewer(palette="Set3")

#5. Create visualisation

grid.newpage() 
pushViewport(viewport(layout = grid.layout(3, 4)))
print(pie_smmry, vp = vplayout(1, 1))
print(pl_smmry, vp = vplayout(1, 2))
print(sig_smmry, vp = vplayout(1, 3:4))
print(sig_strat, vp = vplayout(2:3, 1:2))
print(exprs_smmry, vp = vplayout(2:3, 3:4))

#Compute value traded long/short and in hedge
trd_data <- unique(history_data[c('Strategy','Instrument','TradeDate','ValueUSD','TodayPL','TradeID','MarketValue')])
hedge_strats <- c('BA_SHEDGE')
trd_data$ST <- NA
trd_data$ST[trd_data$MarketValue<0] <- 'S'
trd_data$ST[trd_data$MarketValue>0] <- 'L'
trd_data$ST[trd_data$Strategy%in%hedge_strats] <- 'HEDGE'
trd_data$Month <- format(trd_data$TradeDate,'%Y-%m')
ttl_pl_smrry <- aggregate(trd_data[c('TodayPL','ValueUSD')],list(Strategy=trd_data$ST,Month=trd_data$Month),function(x)sum(x,na.rm=TRUE))
cum_stats <- trd_data
cum_stats[is.na(cum_stats)] <- 0
cum_stats <- cum_stats[order(cum_stats$TradeDate),]
for(s in c('S','L','HEDGE')){
  cum_stats[cum_stats$ST==s,]$TodayPL <- cumsum(cum_stats[cum_stats$ST==s,]$TodayPL)
}
trd_data <- trd_data[!is.na(trd_data$TradeID),]

trd_pl_smrry <- aggregate(trd_data[c('TodayPL','ValueUSD')],list(Strategy=trd_data$ST,Month=trd_data$Month),function(x)sum(x,na.rm=TRUE))

turn_plot <- ggplot(data=trd_pl_smrry,aes(x=Month,y=ValueUSD,group=Strategy,colour=Strategy)) +
             geom_line(size=1)

pl_plot <- ggplot(data=ttl_pl_smrry,aes(x=Month,y=TodayPL,group=Strategy,colour=Strategy)) +
  geom_line(size=1) 

grid.newpage() 
pushViewport(viewport(layout = grid.layout(2, 1)))
print(pl_plot, vp = vplayout(1, 1))
print(turn_plot, vp = vplayout(2, 1))
