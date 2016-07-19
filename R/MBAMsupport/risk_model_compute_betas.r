#setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
#sourceTo("../analysis_modules/analysis_module_position_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
#sourceTo("../reporting/raid_data_import.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("risk_model_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(lubridate)
library(quantmod)
library(ggplot2)
library(useful)
library(grid)

rm_date_start = '2015-05-01'
rm_date_end   = '2016-05-01'
lookback      = 150

lback = ymd(rm_date_start) %m-% days(lookback)
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
for(d in 1:(as.Date(rm_date_end)-as.Date(rm_date_start))){
  rm_date <- ymd(rm_date_start) %m+% days(d-1)
  rm_date <- as.Date(rm_date)
  if(wday(rm_date)!=7&wday(rm_date)!=1){
    message(paste("Computing:",rm_date))
    lback = ymd(rm_date) %m-% days(150)
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
    #Check that factore split into blocks here
    universe_betas <- stock_betas(stk,beta_fct_frame,cl)
    message("Building composite regression model ...")
    ir <- tryCatch({
                    composite_model(stk[stk$Date==rm_date,],list(universe_betas))   
                   },error=function(cond){
                    message(paste("Factor block regression failed:",cond)) 
                   })
    if(length(ir)>0){
      if(first){
        implied_fct_rtns <- cbind(Date=rm_date,ir[[1]])
        residual_rtns <- cbind(Date=rm_date,ir[[2]])
        betas <- cbind(Date=rm_date,universe_betas)
        first <- FALSE
      }
      else{
        implied_fct_rtns <- rbind(implied_fct_rtns,cbind(Date=rm_date,ir[[1]]))
        residual_rtns <- rbind(residual_rtns,cbind(Date=rm_date,ir[[2]]))
        betas <- rbind(betas,cbind(Date=rm_date,universe_betas))
      } 
    }
  }
}

stopCluster(cl)
save.image(paste("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/MBAMsupport/risk_model_",rm_date_start,"_",rm_date_end,".RData",sep=""))
