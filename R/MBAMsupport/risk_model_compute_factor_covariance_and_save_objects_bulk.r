sourceTo("risk_model_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/daily_riskmodel_objectstore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(lubridate)
library(quantmod)
library(ggplot2)
library(useful)
library(grid)

rm_date_start  <- '2015-05-01'
rm_date_end    <- '2016-05-01'
lookback       <- 150
rm_prefix      <- 'developed_europe_prototype' 

#Assumes that risk data is loaded
risk_dates <- c()
first <- TRUE
for(d in 1:(as.Date(rm_date_end)-as.Date(rm_date_start)-lookback)){
  rm_date_s <- as.Date(ymd(rm_date_start) %m+% days(d-1))
  rm_date_e <- as.Date(ymd(rm_date_s) %m+% days(lookback))
  if(wday(rm_date_e)!=7&wday(rm_date_e)!=1){
    risk_dates <- c(risk_dates,as.character(rm_date_e))
    ir <- implied_fct_rtns[implied_fct_rtns$Date>=rm_date_s&implied_fct_rtns$Date<=rm_date_e,]
    fct_cor<- factor_correlation(ir)
    fct_sd <- unlist(Map(sd,head(ir[setdiff(colnames(ir),'Date')])))
    fct_cov<- factor_covariance(fct_cor,fct_sd)
    es <- eigen(fct_cov)
    ev <- cbind(Date=rm_date_e,as.data.frame(t(as.numeric(scale(Re(es[[2]][,1]))))))
    colnames(ev) <- colnames(ir)
    if(first){
      all_fct_cor <- cbind(Date=as.Date(rm_date_e),as.data.frame(fct_cor))
      all_fct_sd <- cbind(Date=as.Date(rm_date_e),as.data.frame(t(fct_sd)))
      all_market_drivers <- ev
      first <- FALSE
    }
    else{
      all_fct_cor <- rbind(all_fct_cor,cbind(Date=as.Date(rm_date_e),as.data.frame(fct_cor)))
      all_fct_sd <- rbind(all_fct_sd,cbind(Date=as.Date(rm_date_e),as.data.frame(t(fct_sd))))
      all_market_drivers <- rbind(all_market_drivers,ev)
    }
  }
}

#Save risk model objects
risk_months <- unique(format(as.Date(risk_dates),format='%Y-%m'))
risk_months <- as.Date(paste(risk_months,'-01',sep=""))
for(m in 2:length(risk_months)){
  mnth      <- risk_months[m]
  last_mnth <- risk_months[m-1]  
  rm_name <-paste(rm_prefix,"_",format(last_mnth,format='%Y-%m'),sep="") 
  rm_store<-risk_model_objectstore_factory(rm_name,lookback)
  rm_store<-pushRiskModelComponent(rm_store,betas[betas$Date>=(last_mnth-lookback)&betas$Date<mnth,],rm_name,lookback,"Betas")
  rm_store<-pushRiskModelComponent(rm_store,residual_rtns[residual_rtns$Date>=(last_mnth-lookback)&residual_rtns$Date<mnth,],rm_name,lookback,"ResidualReturns")
  rm_store<-pushRiskModelComponent(rm_store,implied_fct_rtns[implied_fct_rtns$Date>=(last_mnth-lookback)&implied_fct_rtns$Date<mnth,],rm_name,lookback,"ImpliedFactorReturns")
  rm_store<-pushRiskModelComponent(rm_store,all_fct_cor[all_fct_cor$Date>=last_mnth&all_fct_cor$Date<mnth,],rm_name,lookback,"FactorCorrelation")
  rm_store<-pushRiskModelComponent(rm_store,all_fct_sd[all_fct_sd$Date>=last_mnth&all_fct_sd$Date<mnth,],rm_name,lookback,"FactorVariance")
  rm_store<-pushRiskModelComponent(rm_store,all_market_drivers[all_market_drivers$Date>=last_mnth&all_market_drivers$Date<mnth,],rm_name,lookback,"MarketStyle")
  commitDailyRiskModelObjectStore(rm_store)
}
rm_name <-paste(rm_prefix,"_",format(mnth,format='%Y-%m'),sep="") 
rm_store<-risk_model_objectstore_factory(rm_name,lookback)
rm_store<-pushRiskModelComponent(rm_store,betas[betas$Date>=(mnth-lookback),],rm_name,lookback,"Betas")
rm_store<-pushRiskModelComponent(rm_store,residual_rtns[residual_rtns$Date>=(mnth-lookback),],rm_name,lookback,"ResidualReturns")
rm_store<-pushRiskModelComponent(rm_store,implied_fct_rtns[implied_fct_rtns$Date>=(mnth-lookback),],rm_name,lookback,"ImpliedFactorReturns")
rm_store<-pushRiskModelComponent(rm_store,all_fct_cor[all_fct_cor$Date>=mnth,],rm_name,lookback,"FactorCorrelation")
rm_store<-pushRiskModelComponent(rm_store,all_fct_sd[all_fct_sd$Date>=mnth,],rm_name,lookback,"FactorVariance")
rm_store<-pushRiskModelComponent(rm_store,all_market_drivers[all_market_drivers$Date>=mnth,],rm_name,lookback,"MarketStyle")
commitDailyRiskModelObjectStore(rm_store)

