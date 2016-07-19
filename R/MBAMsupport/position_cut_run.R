setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
source("../analysis_modules_legacy/analysis_module_position_holding_period.r")
source("../scripts/coaching_review_functions.r")
source("../reporting/raid_data_import.r")
library(ggplot2)
library(plotly)

#There is something potentially a tad skewed whereby we are sitting in positions going down and sometimes adding or not cutting, 
#and yet on winners we seem pretty quick to be top slicing and exiting, and leaving residual small postions.

traders   <- c(101,11,70)
dates <- c("2016-05-01")

first <- TRUE
for(t in traders){
  kf <- function()dated_eighteen_monthly_lookback(t,dates[1])
  if(first){
    trader_data <- analysis_module_load_multiple(t,dates,history_analysis_module_builder,dated_eighteen_monthly_lookback)
    history_data <- cbind(Trader=t,trader_data)    
    first <- FALSE
  }
  else{
    trader_data <- analysis_module_load_multiple(t,dates,history_analysis_module_builder,dated_eighteen_monthly_lookback)
    history_data <- rbind.fill(history_data,cbind(Trader=t,trader_data))    
  }
}
history_data <- market_rel_pl(history_data)

psn_data <- unique(history_data[c('TradeID','Trader','Strategy','Instrument','TradeDate','MarketValue','TodayPL','ValueUSD','CumulativePL','CumulativeMarketRelPL','Long','ActiveTodayPL','MarketRelPL', 'EarliestMarketValue', 'CompoundReturnOutof','PnLOutof')])
psn_data[c('MarketValue','TodayPL','ValueUSD','CumulativePL','CumulativeMarketRelPL','ActiveTodayPL','MarketRelPL', 'EarliestMarketValue', 'CompoundReturnOutof','PnLOutof')][is.na(psn_data[c('MarketValue','TodayPL','ValueUSD','CumulativePL','CumulativeMarketRelPL','ActiveTodayPL','MarketRelPL', 'EarliestMarketValue', 'CompoundReturnOutof','PnLOutof')])] <- 0
aum_abs_byside <- aggregate(psn_data$MarketValue,list(Date=psn_data$TradeDate,Strategy=psn_data$Strategy,Trader=psn_data$Trader,OnOff=psn_data$CumulativePL<0),sum)
aum_abs_byside <- rbind(cbind(Type='Offside',aum_abs_byside[aum_abs_byside$OnOff==TRUE,]),
                        cbind(Type='Onside',aum_abs_byside[aum_abs_byside$OnOff==FALSE,]))
plt_aum_abs_byside <- ggplot(data=aum_abs_byside[aum_abs_byside$Trader==101,],aes(x=Date,y=x,group=Type,colour=Type)) +
                      geom_line(size=1) +
                      facet_grid(Strategy~.,scales="free_y")

trd_data <- psn_data[!is.na(psn_data$TradeID),]
trd_data$TradeType <- 'NA'
trd_data$TradeType[trd_data$Long==1&trd_data$MarketValue>0] <- 'Increase'
trd_data$TradeType[(trd_data$Long==0)&trd_data$MarketValue>0] <- 'Decrease'
trd_data$TradeType[trd_data$Long==1&trd_data$MarketValue<0] <- 'Decrease'
trd_data$TradeType[(trd_data$Long==0)&trd_data$MarketValue<0] <- 'Increase'
trd_data$OnOff <- trd_data$CumulativePL<0
trd_data$TopSlice <- !trd_data$OnOff&((trd_data$CumulativePL/abs(trd_data$EarliestMarketValue))>0.2)&(trd_data$TradeType=='Decrease')
#Compute the net $ dollar flow into onside positions
trd_data$ValueUSD[trd_data$TradeType=='Decrease'&(!trd_data$OnOff)] <- -1*trd_data$ValueUSD[trd_data$TradeType=='Decrease'&(!trd_data$OnOff)]
trd_data$ValueUSD[trd_data$TradeType=='Increase'&trd_data$OnOff] <- -1*trd_data$ValueUSD[trd_data$TradeType=='Increase'&trd_data$OnOff]
trd_data$OppCost <- (trd_data$CompoundReturnOutof/10000)*trd_data$ValueUSD

#1. Where are they top slicing, how often and when would they have made more by not doing it
counts <- merge(aggregate(trd_data$TradeID,list(Strategy=trd_data$Strategy),length),aggregate(trd_data$TradeID,list(Strategy=trd_data$Strategy,TopSlice=trd_data$TopSlice),length),by='Strategy')
counts <- counts[counts$TopSlice==TRUE,]
counts$value <- (counts$x.y/counts$x.x)
counts <- counts[order(counts$value),]
plt_data <- data.frame(Type='Frequency (%)',Quantity='TopSlicers',Strategy=counts$Strategy,Value=100*counts$value)
oppcounts <- merge(aggregate(trd_data$TradeID,list(Strategy=trd_data$Strategy),length),aggregate(trd_data$OppCost<0,list(Strategy=trd_data$Strategy,TopSlice=trd_data$TopSlice),sum),by=c('Strategy'))
oppcounts <- oppcounts[oppcounts$TopSlice==TRUE,]
oppcounts <- oppcounts[order(counts$value),]
plt_data <- rbind(plt_data,data.frame(Type='Frequency (%)',Quantity='EarlyTopSlicers',Strategy=oppcounts$Strategy,Value=100*(oppcounts$x.y/oppcounts$x.x)))
payoff <- aggregate(trd_data$OppCost,list(Strategy=trd_data$Strategy,TopSlice=trd_data$TopSlice,Early=trd_data$OppCost<0),sum)
allpayoff <- payoff[payoff$TopSlice==TRUE,]
payoff <- allpayoff[!allpayoff$Early,]
payoff <- payoff[order(counts$value),]
payoff_early <- allpayoff[allpayoff$Early,]
payoff_early <- payoff_early[order(counts$value),]
plt_data <- rbind(plt_data,
                  data.frame(Type='PayOff ($)',Quantity='EarlyTopSlicers',Strategy=payoff_early$Strategy,Value=payoff_early$x),
                  data.frame(Type='PayOff ($)',Quantity='TopSlicers',Strategy=payoff$Strategy,Value=payoff$x))
slicers <- ggplot(plt_data,aes(x=Strategy,fill=Quantity)) +
           geom_bar(aes(weight=Value),position="dodge") +
           xlab('Strategy') + ylab('') + ggtitle("Top slice trades Jan 2015 - May 2016") +
           theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
           facet_grid(Type~.,scales='free_y') 
ggplotly(slicers)
#2. Does this have an overall neagative effect on the PL
trn_abs_byside <- aggregate(trd_data$ValueUSD,list(Date=trd_data$TradeDate,Strategy=trd_data$Strategy,Trader=trd_data$Trader,Increase=trd_data$TradeType=='Increase',Offside=trd_data$OnOff),sum)
colnames(trn_abs_byside) <- c('Date','Strategy','Trader','Increase','Offside','Net')
trn_abs_byside <- merge(trn_abs_byside,aggregate(psn_data[c('CumulativePL','CumulativeMarketRelPL','ActiveTodayPL')],list(Date=psn_data$TradeDate,Strategy=psn_data$Strategy,Trader=psn_data$Trader),sum),by=c('Date','Strategy','Trader'))
#Consider only topslice trades: Condition data on being onside into the trade for trades decreasing position size
trn_abs_byside <- trn_abs_byside[(!trn_abs_byside$Increase)&(!trn_abs_byside$Offside),]
trn_abs_byside <- trn_abs_byside[c('Date','Strategy','Trader','Net','CumulativePL','CumulativeMarketRelPL','ActiveTodayPL')]

trn_abs_byside <- trn_abs_byside[order(trn_abs_byside$Date),]
trn_abs_byside$CumulativeActiveTodayPL <- trn_abs_byside$ActiveTodayPL
for(ins in unique(trn_abs_byside$Instrument)){
  trn_abs_byside[trn_abs_byside$Instrument==ins,]$CumulativeActiveTodayPL <- cumsum(trn_abs_byside[trn_abs_byside$Instrument==ins,]$ActiveTodayPL)
}

mean_trn_abs_byside <- trn_abs_byside
tau <- 4
for(s in unique(mean_trn_abs_byside$Strategy)){
  r <- nrow(mean_trn_abs_byside[mean_trn_abs_byside$Strategy==s,])
  if(r>tau){
    cs <- cumsum(mean_trn_abs_byside[mean_trn_abs_byside$Strategy==s,]$Net)
    mean_trn_abs_byside[mean_trn_abs_byside$Strategy==s,]$Net[1:(tau-1)] <- 0
    mean_trn_abs_byside[mean_trn_abs_byside$Strategy==s,]$Net[tau:r] <- (cs[tau:r]-cs[1:(r-tau+1)])/tau
  }
}

pdata <- rbind(cbind(data.frame(Quantity='Flow',Net=mean_trn_abs_byside$Net),mean_trn_abs_byside[c('Date','Strategy','Trader')]),
               cbind(data.frame(Quantity='PL',Net=mean_trn_abs_byside$CumulativePL),mean_trn_abs_byside[c('Date','Strategy','Trader')]),
               cbind(data.frame(Quantity='MarketRel',Net=mean_trn_abs_byside$CumulativeMarketRelPL),mean_trn_abs_byside[c('Date','Strategy','Trader')]),
               cbind(data.frame(Quantity='Active',Net=mean_trn_abs_byside$ActiveTodayPL),mean_trn_abs_byside[c('Date','Strategy','Trader')]))

plt_trn_abs_byside <- ggplot(data=pdata[pdata$Trader==70,],aes(x=Date,y=Net,group=Quantity,colour=Quantity)) +
  geom_line(size=1) +
  facet_grid(Strategy~.,scales="free_y")

all_static <- list()
all_lagged <- list()
windows <- 50
for(t in c(11,70,101)){
  for(s in unique(pdata[pdata$Trader==t,]$Strategy)){
    df <- trn_abs_byside[trn_abs_byside$Trader==t&trn_abs_byside$Strategy==s,]
    df <- df[order(df$Date),]
    if(nrow(df)>windows){
      first <- TRUE
      for(w in 1:windows){
        static <- df[(windows/2):(nrow(df)-windows/2),c('Net','CumulativePL','CumulativeMarketRelPL','CumulativeActiveTodayPL')]
        lagged <- df[w:(nrow(df)-windows+w),]
        if(first){
          cor_frame_static <- static
          cor_frame_lagged <- lagged
          first <- FALSE
        } else {
          cor_frame_static <- cbind(cor_frame_static,static)
          cor_frame_lagged <- cbind(cor_frame_lagged,lagged)
        }
      }
      all_static[[s]] <- cor_frame_static
      all_lagged[[s]] <- cor_frame_lagged
    }
  }
}

#A top slice would be where: Cumulative PL prior to the flow is positive; The flow is negative, i.e. toward the offside, (resulting in negative cor)
#and post flow active PL is negative (resulting in a positive correlation); use to create top slice indicator
all_cors <- list(list())
for(s in names(all_static)){
  first <- TRUE
  for(qty in unique(colnames(all_static[[s]]))){
    rw <- unique(cor(all_static[[s]][,names(all_static[[s]])=='Net'],all_lagged[[s]][,names(all_lagged[[s]])==qty]))
    if(first){
      all_cors[[s]] <- rw
      first <- FALSE
    } else {
      all_cors[[s]] <- rbind(all_cors[[s]],rw)  
    }
  }
  rownames(all_cors[[s]]) <- unique(colnames(all_static[[s]]))
}

#Score the number of days that the direction is trading toward offside
pdata_scr <- pdata[c('Net','Date','Strategy','Trader')]
pdata_scr$Score[pdata_scr$Net>0] <- 1
pdata_scr$Score[pdata_scr$Net<=0] <- -1
pdata_scr$Month <- format(pdata$Date,'%Y-%m')
pdata_plt <- aggregate(pdata_scr['Score'],list(Month=pdata_scr$Month,Strategy=pdata_scr$Strategy,Trader=pdata_scr$Trader),sum)

plt_scr <- ggplot(data=pdata_plt,aes(x=Strategy,fill=Strategy)) +
           geom_bar(aes(weight=Score)) +
           facet_wrap(~Month,ncol=3) +
           theme(axis.text.x = element_text(angle = 90, hjust = 1))
