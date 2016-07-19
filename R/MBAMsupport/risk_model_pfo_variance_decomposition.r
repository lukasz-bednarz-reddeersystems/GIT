sourceTo("../common/dataplex.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("risk_model_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(ggplot2)
library(grid)
library(useful)

trader <- 101
rm_date_start  <- '2016-01-01'
rm_date_end    <- '2016-04-29'
lookback       <- 150
rm_date_start        <- as.Date(ymd(rm_date_start) %m-% days(lookback))

market_factors <- c('Earnings','Growth','PriceMomentum12M','PriceMomentum1M','Size','StreetSentiment','Strength','TrendExtension','Value','Volatility')
currency <- c('JPY','GBP','EUR','CNY','RUB','ZAR','HKD','AUD','DKK','NOK','SEK','CHF','ILS','PLN','HUF','TRY')
commodity <- c('WTI')
sector <- c('SX3P','SX4P','SX6P','SX7P','SX86P','SX8P','SXAP','SXDP','SXEP','SXFP','SXKP','SXMP','SXNP','SXOP','SXPP','SXQP','SXRP','SXTP')

history_data <- build_portfolio_history(trader,rm_date_start,rm_date_end)

#Assumes that risk data is loaded and factor correlation matricies 
#have been computed
first <- TRUE
for(d in 1:(as.Date(rm_date_end)-as.Date(rm_date_start)-lookback)){
  rm_date <- as.Date(ymd(rm_date_start) %m+% days(d+lookback))
  if(wday(rm_date)!=7&wday(rm_date)!=1){
    bt <- betas[betas$Date==rm_date,setdiff(colnames(betas),'Date')]
    bt[is.na(bt)] <- 0
    wt <- history_data[history_data$Date==rm_date,c('InstrumentID','Weight')]
    colnames(wt) <- c('Instrument','Weight')
    fct_cor <- all_fct_cor[all_fct_cor$Date==rm_date,setdiff(colnames(all_fct_cor),'Date')]
    fct_sd  <- all_fct_sd[all_fct_sd$Date==rm_date,setdiff(colnames(all_fct_cor),'Date')]
    if(nrow(fct_cor)>0 && nrow(fct_sd)>0){
      fct_cov <- factor_covariance(fct_cor,fct_sd)
      total_sys_var <- pfo_variance_decomposition(wt,bt,fct_cov)
      factor_var <- pfo_variance_decomposition(wt,bt,fct_cov,market_factors)
      currency_var <- pfo_variance_decomposition(wt,bt,fct_cov,currency)
      commodity_var <- pfo_variance_decomposition(wt,bt,fct_cov,commodity)
      sector_var <- pfo_variance_decomposition(wt,bt,fct_cov,sector)
      vd <- data.frame(Date=rm_date,TotalSystematicVar=total_sys_var[1],MarketFactorVar=factor_var[1],CurrencyVar=currency_var[1],CommodityVar=commodity_var[1],SectorVar=sector_var[1])
      if(first){
        variance_decomposition <- vd
        first <- FALSE
      }
      else{
        variance_decomposition <- rbind(variance_decomposition,vd)
      } 
    }
   }
}

risk_plot_data <- rbind(data.frame(Date=variance_decomposition$Date,RiskType='TotalSystematic',Value=sqrt(variance_decomposition$TotalSystematicVar)*10000),
                        data.frame(Date=variance_decomposition$Date,RiskType='MarketRiskFactor',Value=sqrt(variance_decomposition$MarketFactorVar)*10000),
                        data.frame(Date=variance_decomposition$Date,RiskType='Currency',Value=sqrt(variance_decomposition$CurrencyVar)*10000),
                        data.frame(Date=variance_decomposition$Date,RiskType='Commodity',Value=sqrt(variance_decomposition$CommodityVar)*10000),
                        data.frame(Date=variance_decomposition$Date,RiskType='Currency',Value=sqrt(variance_decomposition$CurrencyVar)*10000),
                        data.frame(Date=variance_decomposition$Date,RiskType='Sector',Value=sqrt(variance_decomposition$SectorVar)*10000))
plt_risk <- ggplot(data=risk_plot_data[risk_plot_data$Date<'2015-04-28',],aes(x=Date,y=Value,colour=RiskType)) +
            geom_line(size=1) + ylab("Daily risk attribution (bps)") + xlab("Date") + labs(fill="Risk type") 
