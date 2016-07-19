sourceTo("risk_model_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(lubridate)
library(quantmod)
library(ggplot2)
library(useful)
library(grid)

rm_date_start  <- '2016-01-01'
rm_date_end    <- '2016-04-29'
lookback      <- 150
rm_name        <- 'developed_europe_prototype' 

#Assumes that risk data is loaded
first <- TRUE
for(d in 1:(as.Date(rm_date_end)-as.Date(rm_date_start))){
  rm_date_s <- as.Date(ymd(rm_date_start) %m+% days(d-lookback))
  rm_date_e <- as.Date(ymd(rm_date_start) %m+% days(d-1))
  if(wday(rm_date_e)!=7&wday(rm_date_e)!=1){
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


