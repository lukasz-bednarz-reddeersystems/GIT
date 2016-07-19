setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules_legacy/analysis_module_position_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../reporting/raid_data_import.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/dataplex.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("prototype_portfolio_core_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("coaching_review_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../MBAMsupport/hedging_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(lubridate)
library(quantmod)
library(ggplot2)
library(useful)
library(grid)
library(RODBC)

trader   <- 101
hedge_strats <- c("DK_SHEDGE","DK_LHEDGE")
dates <- c("2016-04-01")
history_data <- analysis_module_load_multiple(trader,dates,history_analysis_module_builder,dated_twelve_monthly_lookback)
history_data <- market_rel_pl(history_data,trade_rel=FALSE)
history_data <- market_day_age(history_data)

first <- TRUE
for(hs in hedge_strats){
  if(first){
    hedge <- history_data[history_data$Strategy==hs,]    
    non_hedge <- history_data[history_data$Strategy!=hs,]    
    first <- FALSE
  }
  else{
    hedge <- rbind(hedge,history_data[history_data$Strategy==hs,])
    non_hedge <- history_data[history_data$Strategy!=hs,]    
  }
}

hedge_ins <- unique(hedge[c('TradeDate','Instrument','MarketValue','TodayPL','MarketRelPL','Name')])
hedge_ins <- aggregate(hedge_ins[c('TodayPL','MarketValue','MarketRelPL')],list(Date=hedge_ins$TradeDate,Instrument=hedge_ins$Instrument,Name=hedge_ins$Name),function(x)sum(x,na.rm=TRUE))
hedge_ttls <- unique(hedge[c('TradeDate','Instrument','MarketValue','TodayPL','MarketRelPL','Name')])
hedge_ttls <- aggregate(hedge_ttls[c('TodayPL','MarketValue','MarketRelPL')],list(Date=hedge_ttls$TradeDate),function(x)sum(x,na.rm=TRUE))

hedge_dtr <- rbind(data.frame(Type='Value',Quantity='Market Value',Date=hedge_ttls$Date,Value=hedge_ttls$MarketValue),
                    data.frame(Type='PL',Quantity='Total PL',Date=hedge_ttls$Date,Value=cumsum(hedge_ttls$TodayPL)),
                    data.frame(Type='PL',Quantity='Relative PL',Date=hedge_ttls$Date,Value=cumsum(hedge_ttls$MarketRelPL)))

plt_hedge_ttl <- ggplot(data=hedge_dtr,aes(x=Date,y=Value,group=Quantity,colour=Quantity)) +
                 geom_line(size=1) +
                 theme(text = element_text(size=15)) +
                 xlab("Date") + 
                 facet_grid(Type~.,scales="free_y")

non_hedge_ttls <- unique(non_hedge[c('TradeDate','Instrument','MarketValue','TodayPL','MarketRelPL','Name')])
non_hedge_ttls <- aggregate(non_hedge_ttls[c('TodayPL','MarketValue','MarketRelPL')],list(Date=non_hedge_ttls$TradeDate),function(x)sum(x,na.rm=TRUE))

non_hedge_dtr <- rbind(data.frame(Type='Value',Quantity='Market Value',Date=non_hedge_ttls$Date,Value=non_hedge_ttls$MarketValue),
                    data.frame(Type='PL',Quantity='Total PL',Date=non_hedge_ttls$Date,Value=cumsum(non_hedge_ttls$TodayPL)),
                    data.frame(Type='PL',Quantity='Relative PL',Date=non_hedge_ttls$Date,Value=cumsum(non_hedge_ttls$MarketRelPL)),
                    data.frame(Type='Residual',Quantity='PL',Date=non_hedge_ttls$Date,Value=(cumsum(hedge_ttls$TodayPL)+(cumsum(non_hedge_ttls$TodayPL-non_hedge_ttls$MarketRelPL)))))

plt_non_hedge_ttl <- ggplot(data=non_hedge_dtr,aes(x=Date,y=Value,group=Quantity,colour=Quantity)) +
                 geom_line(size=1) +
                 theme(text = element_text(size=15)) +
                 xlab("Date") + 
                 facet_grid(Type~.,scales="free_y")

rm_date_start = '2015-10-01'
rm_date_end   = '2016-03-24'
lookback      = 150

instruments <- unique(unique(history_data$Instrument))
lback = ymd(rm_date_start) %m-% days(lookback)
all_stk <- get_stock_returns(instruments,as.Date(lback),rm_date_end)
all_fct <- get_risk_factor_returns(as.Date(lback),rm_date_end)
all_fx  <- get_FX_returns(as.Date(lback),rm_date_end)
all_oil <- get_commodity_returns(as.Date(lback),rm_date_end)
all_sct <- get_sector_returns(as.Date(lback),rm_date_end)

first <- TRUE
for(d in 1:(as.Date(rm_date_end)-as.Date(rm_date_start))){
  rm_date <- ymd(rm_date_start) %m+% days(d)
  rm_date <- as.Date(rm_date)
  if(wday(rm_date)!=7&wday(rm_date)!=1){
    message(paste("Computing:",rm_date))
    lback = ymd(rm_date) %m-% days(150)
    lback <- as.Date(lback)
    stk <- all_stk[all_stk$Date>=lback&all_stk$Date<=rm_date,]
    fct <- all_fct[all_fct$Date>=lback&all_fct$Date<=rm_date,]
    fct <- pivot_frame(fct,'FactorName','Return','Date')
    message("Computing betas ...")
    fct_betas <- stock_betas(stk,fct)
    fx <- all_fx[all_fx$Date>=lback&all_fx$Date<=rm_date,]
    fx_betas <- stock_betas(stk,fx)
    oil <- all_oil[all_oil$Date>=lback&all_oil$Date<=rm_date,]
    oil_betas<- stock_betas(stk,oil)
    sct <- all_sct[all_sct$Date>=lback&all_sct$Date<=rm_date,]
    sct <- pivot_frame(sct,'FactorName','Return','Date')
    sct_betas <- stock_betas(stk,sct)
    message("Building composite regression model ...")
    ir <- tryCatch({
                    composite_model(stk[stk$Date==rm_date,],list(fx_betas,oil_betas,fct_betas,sct_betas))   
                   },error=function(cond){
                    message(paste("Regression failed:",cond)) 
                   })
    bt <- merge(fx_betas,oil_betas,by='Instrument')
    bt <- merge(bt,fct_betas,by='Instrument')
    bt <- merge(bt,sct_betas,by='Instrument')
    if(length(ir)>0){
      if(first){
        implied_fct_rtns <- cbind(Date=rm_date,ir[[1]])
        betas <- cbind(Date=rm_date,bt)
        first <- FALSE
      }
      else{
        implied_fct_rtns <- rbind(implied_fct_rtns,cbind(Date=rm_date,ir[[1]]))
        betas <- rbind(betas,cbind(Date=rm_date,bt))
      } 
    }
  }
}

hedge_data <- unique(history_data[c('Strategy','TradeDate','Instrument','MarketValue','TodayPL','MarketRelPL','Name')])
colnames(hedge_data)[colnames(hedge_data)=='TradeDate'] <- 'Date'
hedge_values <- aggregate(hedge_data[c('TodayPL','MarketValue','MarketRelPL')],list(Date=hedge_data$Date),function(x)sum(abs(x),na.rm=TRUE))
hedge_data <- rbind(hedge_data,cbind(Strategy='HEDGE',hedge_data[unlist(Map(function(x)x%in%hedge_strats,hedge_data$Strategy)),c('Date','Instrument','MarketValue','TodayPL','MarketRelPL','Name')]))
hedge_data <- rbind(hedge_data,cbind(Strategy='NONHEDGE',hedge_data[!unlist(Map(function(x)x%in%hedge_strats,hedge_data$Strategy)),c('Date','Instrument','MarketValue','TodayPL','MarketRelPL','Name')]))
strats <- unique(hedge_data$Strategy)

first <- TRUE
for(d in 1:(as.Date(rm_date_end)-as.Date(rm_date_start))){
  rm_date <- ymd(rm_date_start) %m+% days(d)
  rm_date <- as.Date(rm_date)
  if(wday(rm_date)!=7&wday(rm_date)!=1){
    hedge_ttls <- hedge_values[hedge_values$Date==rm_date,]
    for(s in strats){
      hedge_ins <- hedge_data[hedge_data$Strategy==s,]
      hedge_pfo <- hedge_ins[hedge_ins$Date==rm_date,]
      if(nrow(hedge_pfo)>0){
        hedge_pfo <- merge(hedge_pfo,hedge_ttls[c('Date','MarketValue')],by=c('Date'))
        hedge_pfo$Weight <- hedge_pfo$MarketValue.x/hedge_pfo$MarketValue.y
        hedge_pfo <- hedge_pfo[c('Instrument','Weight')]
        pfo_betas <- merge(hedge_pfo,betas[betas$Date==rm_date,2:ncol(betas)],by=c('Instrument'))
        pfo_betas <- t(pfo_betas[,2])%*%as.matrix(pfo_betas[,3:ncol(pfo_betas)])
        fr <- implied_fct_rtns[implied_fct_rtns$Date==rm_date,2:ncol(implied_fct_rtns)]
        rtns <- pfo_betas*fr
        if(first){
          pfo       <- cbind(Strategy=s,Date=rm_date,hedge_pfo)
          all_betas <- cbind(Strategy=s,Date=rm_date,as.data.frame(pfo_betas))
          all_rtns  <- cbind(Strategy=s,Date=rm_date,as.data.frame(rtns))
          first <- FALSE
        }
        else{
          pfo       <- rbind(pfo,cbind(Strategy=s,Date=rm_date,hedge_pfo))
          all_betas <- rbind(all_betas,cbind(Strategy=s,Date=rm_date,as.data.frame(pfo_betas)))
          all_rtns  <- rbind(all_rtns,cbind(Strategy=s,Date=rm_date,as.data.frame(rtns)))
        }
       }
     }
   }
}

all_rtns <- all_rtns[order(all_rtns$Date),]
all_betas <- all_betas[order(all_betas$Date),]
all_rtns[is.na(all_rtns)] <- 0
rtypes <- c('FX','Commodity','Factor','Sector')

llim = list(FX=1,Commodity=17,Factor=18,Sector=28)
ulim = list(FX=16,Commodity=17,Factor=27,Sector=45)

first <- TRUE
for(s in strats){
  dtr  <- all_betas[all_betas$Strategy==s,] 
  rn   <- all_rtns[all_rtns$Strategy==s,] 
  for(type in rtypes){
    for(fct in colnames(pfo_betas)[llim[[type]]:ulim[[type]]]){
      b <- cbind(Strategy=s,Quantity=fct,Type=type,data.frame(Date=dtr$Date,Value=dtr[[fct]]))
      r <- cbind(Strategy=s,Quantity=fct,Type=type,data.frame(Date=rn$Date,Value=exp(cumsum(rn[[fct]]))))
      if(first){
        first <- FALSE
        beta_plt_frame <- b
        rtn_plt_frame  <- r
      }
      else{
        beta_plt_frame <- rbind(beta_plt_frame,b)
        rtn_plt_frame  <- rbind(rtn_plt_frame,r)
      }
    } 
  }
}

rtn_plts <- list()
for(s in strats){
  typ_plts <- list()
  for(t in rtypes){
    typ_plts[[t]] <- ggplot(rtn_plt_frame[rtn_plt_frame$Strategy==s&rtn_plt_frame$Type==t,],aes(x=Date,y=Value,group=Strategy,colour=Strategy)) +
      geom_line(size=1) +
      labs(colour=t) +
      facet_grid(Quantity~Strategy,scales="free_y") 
  }
  rtn_plts[[s]] <- typ_plts
}

beta_plts <- list()
for(s in strats){
  typ_plts <- list()
  for(t in rtypes){
    typ_plts[[t]] <- ggplot(beta_plt_frame[beta_plt_frame$Strategy==s&beta_plt_frame$Type==t,],aes(x=Date,y=Value,group=Strategy,colour=Strategy)) +
      geom_line(size=1) +
      labs(colour=t) +
      facet_grid(Quantity~Strategy,scales="free_y") 
  }
  beta_plts[[s]] <- typ_plts
}

mnthly_beta_hdg <- beta_plt_frame[beta_plt_frame$Strategy=='HEDGE'|beta_plt_frame$Strategy=='NONHEDGE',]
mnthly_beta_hdg <- mnthly_beta_hdg[order(mnthly_beta_hdg$Date),]
mnthly_beta_hdg$Month <- format(mnthly_beta_hdg$Date,'%Y-%m')
mnthly_beta_hdg[is.na(mnthly_beta_hdg)] <- 0
mnthly_beta_hdg <- aggregate(mnthly_beta_hdg$Value,list(Month=mnthly_beta_hdg$Month,Strategy=mnthly_beta_hdg$Strategy,Type=mnthly_beta_hdg$Type,Quantity=mnthly_beta_hdg$Quantity),function(x)(prod(1+x)^(1/length(x)))-1)
typ_plts <- list()
for(t in rtypes){
  typ_plts[[t]] <- ggplot(mnthly_beta_hdg[mnthly_beta_hdg$Type==t,],aes(x=Quantity,fill=Strategy)) +
    geom_bar(aes(weight=x),position="dodge") +
    labs(fill=t) +
    coord_flip() +
    facet_wrap(~Month,ncol=3) 
}