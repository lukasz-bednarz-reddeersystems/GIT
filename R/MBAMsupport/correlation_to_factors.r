setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules/analysis_module_position_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("prototype_portfolio_core_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(ggplot2)
library(quantmod)
library(RODBC)

#Get exposure data from database
cn <- odbcConnect('RAIDLIVEDB',uid='guy.billings')
sqlQuery(cn,'USE Razor')
factor_exposure_SQL <- "prFactorRisk_GetScores_TraderAnalytics"
factor_exposures <- sqlQuery(cn,factor_exposure_SQL)
close(cn)

#Get factor returns from middleware
factor_rtns <- data_request("risk_factor_returns",data.frame(dtDateTime=as.Date(unique(factor_exposures$dtDateTime))),c("sFactorName","dblChangePercent"))
factor_rtns <- factor_rtns@data

#Get index data
getSymbols("^SX5E")
index <- data.frame(TradeDate=as.Date(rownames(as.data.frame(SX5E)))[2:nrow(SX5E)],diff(SX5E$SX5E.Close)/SX5E$SX5E.Close[2:nrow(SX5E)])

#Merge data
factors <- c('rValue','rStrength','rGrowth','rSize','rSectorTrendExtension','rStreetSentiment','rPriceMomentum1M','rPriceMomentum12M','rTrendExtension','rEarnings','rVolatility')
factor_exposures <- merge(factor_exposures,data.frame(sFactorName=factors),by=c('sFactorName'))
factor_exposures$sFactorName <- as.character(factor_exposures$sFactorName)
factor_exposures$dtDateTime <- as.Date(factor_exposures$dtDateTime)
factor_rtns <- merge(factor_rtns,data.frame(sFactorName=factors),by=c('sFactorName'))
factor_exposures <- merge(factor_exposures,factor_rtns,by=c('sFactorName','dtDateTime'))
factor_exposures <- factor_exposures[c('sFactorName','dtDateTime','lInstrumentID','dblZScore','dblChangePercent')]
factor_exposures$dblChangePercent <- factor_exposures$dblChangePercent/100
colnames(factor_exposures) <- c('FactorName','TradeDate','Instrument','ZScore','FactorReturn')

#Get trader holdings and trade history
trader   <- 101
dates <- c("2016-01-01")
history_data <- analysis_module_load_mutiple(trader,dates,history_analysis_module_builder,dated_twelve_monthly_lookback)
history_data <- history_data[history_data$TradeDate>='2015-01-01'&history_data$TradeDate<='2015-12-31',]

#Compute portfolio statistics and merge
port <- portfolio_decomposition(history_data,new_only = FALSE)
port[is.na(port)] <- 0
port$StkRelRtn <- port$TotalReturn - port$Weight*port$StockReturn
factor_port <- merge(port,factor_exposures,by=c('Instrument','TradeDate'))
factor_port$FactorAttrib <- factor_port$ZScore*factor_port$FactorReturn*factor_port$Weight

#Compute portfolio level factor returns
factor_performance<- function(factor_port,port){
  factor_perf <- aggregate(factor_port$FactorAttrib,list(FactorName=factor_port$FactorName,Date=factor_port$TradeDate),function(x)sum(x,na.rm=TRUE))
  colnames(factor_perf) <- c('FactorName','Date','PfoFactorReturn')
  overall_perf <- aggregate(port[c('TotalReturn','StkRelRtn')],list(Date=port$TradeDate),function(x)sum(x,na.rm=TRUE))
  colnames(overall_perf) <- c('Date','PfoReturn','StockRel')
  overall_perf <- merge(overall_perf,index,by.x=c('Date'),by.y=c('TradeDate'))
  overall_perf$MarketRel <- overall_perf$PfoReturn - overall_perf$SX5E.Close
  overall_perf <- merge(overall_perf,factor_perf,by=c('Date'))
  fp <- unique(factor_port[c('FactorReturn','FactorName','TradeDate')])
  colnames(fp) <- c('FactorReturn','FactorName','Date')
  overall_perf <- merge(overall_perf,fp,by=c('Date','FactorName'))
  overall_perf$PfoReturn[is.infinite(overall_perf$PfoReturn)] <- NA
  overall_perf$MarketRel[is.infinite(overall_perf$MarketRel)] <- NA
  overall_perf$StockRel[is.infinite(overall_perf$StockRel)] <- NA
  return(overall_perf)
}
overall_perf <- factor_performance(factor_port,port)

#Compute regression between factor and pfo return
rfits <- list()
corr <- matrix(NA,nrow=length(factors),ncol=3)
for(fct in factors){
  df <- overall_perf[overall_perf$FactorName==fct,]
  if(nrow(df)){
    ft_total <- lm(PfoReturn ~ FactorReturn, df)
    ft_mkt <- lm(MarketRel ~ FactorReturn, df)
    ft_stk <- lm(StockRel ~ FactorReturn, df)
    rfits[[fct]] <- list(Total=ft_total,MktRel=ft_mkt,StkRel=ft_stk)
    corr[which(factors==fct),1] <- cor(df$PfoReturn[!is.na(df$PfoReturn)&!is.na(df$FactorReturn)&abs(df$PfoReturn)>0&abs(df$FactorReturn)>0],df$FactorReturn[!is.na(df$PfoReturn)&!is.na(df$FactorReturn)&abs(df$PfoReturn)>0&abs(df$FactorReturn)>0])
    corr[which(factors==fct),2] <- cor(df$MarketRel[!is.na(df$MarketRel)&!is.na(df$FactorReturn)&abs(df$MarketRel)>0&abs(df$FactorReturn)>0],df$FactorReturn[!is.na(df$MarketRel)&!is.na(df$FactorReturn)&abs(df$MarketRel)>0&abs(df$FactorReturn)>0])
    corr[which(factors==fct),3] <- cor(df$StockRel[!is.na(df$StockRel)&!is.na(df$FactorReturn)&abs(df$StockRel)>0&abs(df$FactorReturn)>0],df$FactorReturn[!is.na(df$StockRel)&!is.na(df$FactorReturn)&abs(df$StockRel)>0&abs(df$FactorReturn)>0])
  }
}

#Plot factor returns portfolio return
total <- ggplot(data=overall_perf, aes(x=FactorReturn,y=PfoReturn)) +
  geom_point(shape=1) + 
  ylim(-0.05, 0.07) +
  facet_grid( FactorName~.) +
  ylab("Portfolio") + xlab('Factor') + ggtitle('Factor return vs portfolio return') 
mktrel <- ggplot(data=overall_perf, aes(x=FactorReturn,y=MarketRel)) +
  geom_point(shape=1) + 
  ylim(-3, 3) +
  facet_grid( FactorName~.) +
  ylab("Portfolio") + xlab('Factor') + ggtitle('Factor return vs portfolio return') 
stkrel <- ggplot(data=overall_perf, aes(x=FactorReturn,y=StockRel)) +
  geom_point(shape=1) + 
  ylim(-1, 1) +
  facet_grid( FactorName~.) +
  ylab("Portfolio") + xlab('Factor') + ggtitle('Factor return vs portfolio return') 

#Summary
first <- TRUE
for(f in unique(factor_port$FactorName)){
  if(first){
    fct_sum <- rbind(data.frame(Factor=f,Measure='Alpha',Return=names(rfits[[f]]),Value=unlist(Map(function(x)x[[1]][[1]],rfits[[f]]))),
                     data.frame(Factor=f,Measure='Beta',Return=names(rfits[[f]]),Value=unlist(Map(function(x)x[[1]][[2]],rfits[[f]]))),
                     data.frame(Factor=f,Measure='RSquared',Return=names(rfits[[f]]),Value=unlist(Map(function(x)summary(x)$r.squared,rfits[[f]]))),
                     data.frame(Factor=f,Measure='Correlation',Return=names(rfits[[f]]),Value=corr[which(factors==f),]))
    first <- FALSE
  }
  else{
    fct_sum <- rbind(fct_sum,
               rbind(data.frame(Factor=f,Measure='Alpha',Return=names(rfits[[f]]),Value=unlist(Map(function(x)x[[1]][[1]],rfits[[f]]))),
                     data.frame(Factor=f,Measure='Beta',Return=names(rfits[[f]]),Value=unlist(Map(function(x)x[[1]][[2]],rfits[[f]]))),
                     data.frame(Factor=f,Measure='RSquared',Return=names(rfits[[f]]),Value=unlist(Map(function(x)summary(x)$r.squared,rfits[[f]])))),
                     data.frame(Factor=f,Measure='Correlation',Return=names(rfits[[f]]),Value=corr[which(factors==f),]))
  }
}
summry <- ggplot(data=fct_sum, aes(x=Return)) +
  geom_bar(aes(weight=Value)) +
  facet_grid( Measure~Factor, scales = 'free_y') +
  ylab("") + xlab("Return type") + ggtitle('Portfolio/factor regression') 

crr_only <- ggplot(data=fct_sum[fct_sum$Measure=='Correlation',], aes(x=Return)) +
  geom_bar(aes(weight=Value)) +
  facet_grid( .~Factor) +
  ylab("Correlation") + xlab("Return type") + ggtitle('Portfolio/factor return correlation') 

#Calendar returns
overall_perf$Market <- overall_perf$PfoReturn-overall_perf$MarketRel
overall_perf[is.na(overall_perf)] <- 0
idx_rtns <- unique(overall_perf[c('Date','Market')])
idx_rtns$Market[is.infinite(idx_rtns$Market)] <- 0
idx_rtns <- cbind(FactorName='Market',idx_rtns['Date'],FactorReturn=exp(cumsum(log(idx_rtns$Market+1))))
fct_rtns <- overall_perf[c('Date','FactorName','FactorReturn')]
fct_rtns$FactorReturn[is.infinite(fct_rtns$FactorReturn)] <- 0
fct_rtns <- fct_rtns[order(fct_rtns$Date),]
for(fct in unique(fct_rtns$FactorName)){
  fct_rtns[fct_rtns$FactorName==fct,]$FactorReturn <- exp(cumsum(log(fct_rtns[fct_rtns$FactorName==fct,]$FactorReturn+1)))
}
pfo_rtns <- unique(overall_perf[c('Date','PfoReturn')])
pfo_rtns$PfoReturn[is.infinite(pfo_rtns$PfoReturn)] <- 0
pfo_rtns <- cbind(FactorName='Portfolio',pfo_rtns['Date'],FactorReturn=exp(cumsum(log(pfo_rtns$PfoReturn+1))))
cdar_rtns <- rbind(idx_rtns,fct_rtns,pfo_rtns)
rtn_cmp <- ggplot(data=cdar_rtns, aes(x=Date, y=FactorReturn, group = FactorName, colour = FactorName)) +
  geom_line(size = 0.7) + 
  ylab("Cumulative return") + xlab('Date') + ggtitle('Factor return comparison') + scale_linetype_discrete(name = "") 

#Correlation broken out by strategy
factor_port_strat <- merge(factor_port,unique(history_data[c('TradeDate','Instrument','PsnLong','Strategy')]),by=c('TradeDate','Instrument','PsnLong'))
port_strat <- merge(port,unique(history_data[c('TradeDate','Instrument','PsnLong','Strategy')]),by=c('TradeDate','Instrument','PsnLong'))
first <- TRUE
for(st in unique(factor_port_strat$Strategy)){
  p <- port_strat[port_strat$Strategy==st,]
  fp <-factor_port_strat[factor_port_strat$Strategy==st,]
  overall_perf_strat <- factor_performance(fp,p)
  for(fct in unique(fp$FactorName)){
    df <- overall_perf_strat[overall_perf_strat$FactorName==fct,]
    if(nrow(df)){
      df$PfoReturn[is.infinite(df$PfoReturn)] <- 0
      df$MarketRel[is.infinite(df$MarketRel)] <- 0
      df$StockRel[is.infinite(df$StockRel)] <- 0
      if(first){
        strat_corr <- data.frame(ReturnType=c("Total","MarketRel","StockRel"),
                           Values = c(cor(df$PfoReturn[!is.na(df$PfoReturn)&!is.na(df$FactorReturn)&abs(df$PfoReturn)>0&abs(df$FactorReturn)>0],df$FactorReturn[!is.na(df$PfoReturn)&!is.na(df$FactorReturn)&abs(df$PfoReturn)>0&abs(df$FactorReturn)>0]),
                                      cor(df$MarketRel[!is.na(df$MarketRel)&!is.na(df$FactorReturn)&abs(df$MarketRel)>0&abs(df$FactorReturn)>0],df$FactorReturn[!is.na(df$MarketRel)&!is.na(df$FactorReturn)&abs(df$MarketRel)>0&abs(df$FactorReturn)>0]),
                                      cor(df$StockRel[!is.na(df$StockRel)&!is.na(df$FactorReturn)&abs(df$StockRel)>0&abs(df$FactorReturn)>0],df$FactorReturn[!is.na(df$StockRel)&!is.na(df$FactorReturn)&abs(df$StockRel)>0&abs(df$FactorReturn)>0])),
                           Factor = fct,
                           Strategy = st)  
        first <- FALSE
      }
      else{
       strat_corr <- rbind(strat_corr,
                     data.frame(ReturnType=c("Total","MarketRel","StockRel"),
                                Values = c(cor(df$PfoReturn[!is.na(df$PfoReturn)&!is.na(df$FactorReturn)&abs(df$PfoReturn)>0&abs(df$FactorReturn)>0],df$FactorReturn[!is.na(df$PfoReturn)&!is.na(df$FactorReturn)&abs(df$PfoReturn)>0&abs(df$FactorReturn)>0]),
                                           cor(df$MarketRel[!is.na(df$MarketRel)&!is.na(df$FactorReturn)&abs(df$MarketRel)>0&abs(df$FactorReturn)>0],df$FactorReturn[!is.na(df$MarketRel)&!is.na(df$FactorReturn)&abs(df$MarketRel)>0&abs(df$FactorReturn)>0]),
                                           cor(df$StockRel[!is.na(df$StockRel)&!is.na(df$FactorReturn)&abs(df$StockRel)>0&abs(df$FactorReturn)>0],df$FactorReturn[!is.na(df$StockRel)&!is.na(df$FactorReturn)&abs(df$StockRel)>0&abs(df$FactorReturn)>0])),
                                Factor = fct,
                                Strategy = st)) 
      }
    }
  }
}
for(fct in unique(strat_corr$Factor)){
  strat_corr <- rbind(strat_corr,data.frame(ReturnType='Total',Values=corr[which(factors==fct),1],Factor=fct,Strategy='Overall'))
  strat_corr <- rbind(strat_corr,data.frame(ReturnType='MarketRel',Values=corr[which(factors==fct),2],Factor=fct,Strategy='Overall'))
  strat_corr <- rbind(strat_corr,data.frame(ReturnType='StockRel',Values=corr[which(factors==fct),1],Factor=fct,Strategy='Overall'))
}
crr_strat <- ggplot(data=strat_corr, aes(x=ReturnType)) +
  geom_bar(aes(weight=Values)) +
  facet_grid( Strategy~Factor) +
  ylab("Correlation") + xlab("Return type") + ggtitle('Portfolio/factor return correlation') 

#Stacked subset of correlation data
simple_data  <- strat_corr[strat_corr$ReturnType!='StockRel'&strat_corr$Strategy=='Overall',]
stacked_plot <- ggplot(data=simple_data, aes(x=reorder(Factor,Values),fill=Factor)) +
  geom_bar(aes(weight=Values)) +
  facet_grid( ~ReturnType) +
  ylab("Correlation") + xlab("") + 
  ggtitle('Factor return correlation') +
  theme(text = element_text(size=15)) +
  theme(legend.position="none") +
  coord_flip()

#Position return vs ordered attributed factor return
psn_rtns <- aggregate(port$TotalReturn,list(PsnLong=port$PsnLong,Name=as.character(port$Name)),function(x)exp(sum(log(x+1))))
psn_rtns$x <- (psn_rtns$x-1)*10000
colnames(psn_rtns) <- c('PsnLong','Stock','PsnReturn')
psn_rtns <- psn_rtns[order(psn_rtns$PsnReturn),]
psn_rtns$PsnRank <- 1:nrow(psn_rtns)
fct_rtns <- aggregate(factor_port$FactorAttrib,list(PsnLong=factor_port$PsnLong,Name=as.character(factor_port$Name),FactorName=factor_port$FactorName),function(x)exp(sum(log(x+1))))
fct_rtns$x <- (fct_rtns$x-1)*10000
colnames(fct_rtns) <- c('PsnLong','Stock','Factor','FctReturn')
all_rtns <- merge(psn_rtns,fct_rtns,by=c('PsnLong','Stock'))

ldata <- all_rtns[all_rtns$PsnLong,]
ldata <- ldata[order(ldata$PsnReturn),]
dec <- round(nrow(psn_rtns)/10)
ldata <- ldata[ldata$PsnRank>=(nrow(psn_rtns)-dec)|ldata$PsnRank<=dec,]
ldata$Decile <- NA
ldata$Decile[ldata$PsnRank>=(nrow(psn_rtns)-dec)] <- 'Top Decile'
ldata$Decile[ldata$PsnRank<=dec] <- 'Bottom Decile'
ldata[abs(ldata$FctReturn)<100,]$Stock <- ""

lng <- ggplot(data=ldata,aes(x = PsnRank,y=PsnReturn)) +
       geom_point(aes(colour=FctReturn,size = abs(FctReturn))) +
       scale_colour_gradientn(colours=heat.colors(10)) +
       geom_text(data=ldata,aes(label=Stock),angle=25,size=3) +
       facet_grid(Factor~Decile,scales="free_x")
       
sdata <- all_rtns[!all_rtns$PsnLong,]
sdata <- sdata[order(sdata$PsnReturn),]
dec <- round(nrow(psn_rtns)/10)
sdata <- sdata[sdata$PsnRank>=(nrow(psn_rtns)-dec)|sdata$PsnRank<=dec,]
sdata$Decile <- NA
sdata$Decile[sdata$PsnRank>=(nrow(psn_rtns)-dec)] <- 'Top Decile'
sdata$Decile[sdata$PsnRank<=dec] <- 'Bottom Decile'

sht <- ggplot(data=sdata,aes(x = PsnRank,y=PsnReturn)) +
  geom_point(aes(colour=FctReturn,size = abs(FctReturn))) +
  scale_colour_gradientn(colours=heat.colors(10)) +
  facet_grid(Factor~Decile,scales="free_x")
       