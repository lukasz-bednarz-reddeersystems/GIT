sourceTo("../common/daily_riskmodel_objectstore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("risk_model_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(lubridate)
library(quantmod)
library(ggplot2)
library(useful)
library(grid)

lookback      <- 150
month         <- '2016-05-01'

rm_name       <- paste('developed_europe_prototype',format(as.Date(month),'%Y-%m'),sep="_")
rm_store      <- risk_model_objectstore_factory(rm_name,lookback)
rm_date_start <- getMostRecentRiskModelDate(rm_store,rm_name,lookback)
if(length(rm_date_start)==0){
  rm_date_start <- as.Date(month)-1
  rn       <- paste('developed_europe_prototype',format(rm_date_start,'%Y-%m'),sep="_")
  rs       <- risk_model_objectstore_factory(rn,lookback)
  rd       <- getMostRecentRiskModelDate(rs,rn,lookback)
  last_implied_fct_rtns <- getRiskModelComponentOnDate(rs,rn,'ImpliedFactorReturns',rd,lookback)
} else{
  last_implied_fct_rtns <- getRiskModelComponentOnDate(rm_store,rm_name,'ImpliedFactorReturns',rm_date_start,lookback) 
}
rm_date_start <- rm_date_start+1
rm_date_end   <- Sys.Date()-1

lback   <- ymd(rm_date_start) %m-% days(lookback-1)
all_stk <- get_region_stock_returns(as.Date(lback),rm_date_end,c(3))
all_fct <- get_risk_factor_returns(as.Date(lback),rm_date_end)
all_fct <- pivot_frame(all_fct,'FactorName','Return','Date')
all_fx  <- get_FX_returns(as.Date(lback),rm_date_end)
all_oil <- get_commodity_returns(as.Date(lback),rm_date_end)
all_sct <- unique(get_sector_returns(as.Date(lback),rm_date_end))
all_sct <- pivot_frame(all_sct,'FactorName','Return','Date')

cl <- declare_local_cluster(5)
prepare_cluster(cl)

first <- TRUE
for(d in 3:(as.Date(rm_date_end)-as.Date(rm_date_start))){
  rm_date <- ymd(rm_date_start) %m+% days(d)
  rm_date <- as.Date(rm_date)
  if(wday(rm_date)!=7&wday(rm_date)!=1){
    message(paste("Computing:",rm_date))
    lback = ymd(rm_date) %m-% days(lookback)
    lback <- as.Date(lback)
    stk <- all_stk[all_stk$Date>=lback&all_stk$Date<=rm_date,]
    fct <- all_fct[all_fct$Date>=lback&all_fct$Date<=rm_date,]
    fx  <- all_fx[all_fx$Date>=lback&all_fx$Date<=rm_date,]
    oil <- all_oil[all_oil$Date>=lback&all_oil$Date<=rm_date,]
    sct <- all_sct[all_sct$Date>=lback&all_sct$Date<=rm_date,]
    beta_fct_frame <- merge(fct,fx,by='Date')
    beta_fct_frame <- merge(beta_fct_frame,oil,by='Date')
    beta_fct_frame <- merge(beta_fct_frame,sct,by='Date')
    message("Computing betas ...")
    universe_betas <- stock_betas(stk,beta_fct_frame,cl)
    message("Building composite regression model ...")
    ir <- tryCatch({
                    composite_model(stk[stk$Date==rm_date,],list(universe_betas))   
                   },error=function(cond){
                    message(paste("Factor block regression failed:",cond)) 
                   })
    if(length(ir)>0){
      implied_fct_rtns <- cbind(Date=rm_date,ir[[1]])
      residual_rtns <- cbind(Date=rm_date,ir[[2]])
      betas <- cbind(Date=rm_date,universe_betas)
      implied_fct_rtns <- rbind(last_implied_fct_rtns,cbind(Date=rm_date,ir[[1]]))
      implied_fct_rtns <- implied_fct_rtns[2:nrow(implied_fct_rtns),]
      rm_store<-pushRiskModelComponent(rm_store,betas,rm_name,lookback,"Betas")
      rm_store<-pushRiskModelComponent(rm_store,residual_rtns,rm_name,lookback,"ResidualReturns")
      rm_store<-pushRiskModelComponent(rm_store,implied_fct_rtns,rm_name,lookback,"ImpliedFactorReturns")
      fct_cor<- factor_correlation(implied_fct_rtns)
      fct_sd <- unlist(Map(sd,head(implied_fct_rtns[setdiff(colnames(implied_fct_rtns),'Date')])))
      fct_cov<- factor_covariance(fct_cor,fct_sd)
      es <- eigen(fct_cov)
      ev <- cbind(Date=rm_date,as.data.frame(t(as.numeric(scale(Re(es[[2]][,1]))))))
      colnames(ev) <- colnames(implied_fct_rtns)
      rm_store<-pushRiskModelComponent(rm_store,cbind(Date=as.Date(rm_date),as.data.frame(fct_cor)),rm_name,lookback,"FactorCorrelation")
      rm_store<-pushRiskModelComponent(rm_store,cbind(Date=as.Date(rm_date),as.data.frame(t(fct_sd))),rm_name,lookback,"FactorVariance")
      rm_store<-pushRiskModelComponent(rm_store,ev,rm_name,lookback,"MarketStyle")
      last_implied_fct_rtns <- implied_fct_rtns
    }
  }
}
#commitDailyRiskModelObjectStore(rm_store)
stopCluster(cl)


