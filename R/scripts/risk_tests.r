factor_summary <- readRDS("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/model_data/factor_summary_2015.rds")
library(plotly)

factor_rtns <- data_request("risk_factor_returns",data.frame(dtDateTime=factor_summary$TradeDate),c("sFactorName","dblChangePercent"))
factor_rtns <- factor_rtns@data

plot_ly(factor_summary[factor_summary$FactorName=='rPriceMomentum12M'&!is.na(factor_summary$ZScore),],x=ZScore,type="histogram")
plot_ly(factor_summary[factor_summary$FactorName=='rPriceMomentum1M'&!is.na(factor_summary$ZScore),],x=ZScore,type="histogram")
plot_ly(factor_summary[factor_summary$FactorName=='rStreetSentiment'&!is.na(factor_summary$ZScore),],x=ZScore,type="histogram")
plot_ly(factor_summary[factor_summary$FactorName=='rSize'&!is.na(factor_summary$ZScore),],x=ZScore,type="histogram")
plot_ly(factor_summary[factor_summary$FactorName=='rSectorTrendExtension'&!is.na(factor_summary$ZScore),],x=ZScore,type="histogram")
plot_ly(factor_summary[factor_summary$FactorName=='rTrendExtension'&!is.na(factor_summary$ZScore),],x=ZScore,type="histogram")
plot_ly(factor_summary[factor_summary$FactorName=='rEarnings'&!is.na(factor_summary$ZScore),],x=ZScore,type="histogram")
plot_ly(factor_summary[factor_summary$FactorName=='rGrowth'&!is.na(factor_summary$ZScore),],x=ZScore,type="histogram")
plot_ly(factor_summary[factor_summary$FactorName=='rValue'&!is.na(factor_summary$ZScore),],x=ZScore,type="histogram")
plot_ly(factor_summary[factor_summary$FactorName=='rVolatility'&!is.na(factor_summary$ZScore),],x=ZScore,type="histogram")
plot_ly(factor_summary[factor_summary$FactorName=='rStrength'&!is.na(factor_summary$ZScore),],x=ZScore,type="histogram")

plot_ly(factor_rtns[factor_rtns$sFactorName=='rPriceMomentum12M'&!is.na(factor_rtns$dblChangePercent),],x=dblChangePercent,type="histogram")
plot_ly(factor_rtns[factor_rtns$sFactorName=='rPriceMomentum1M'&!is.na(factor_rtns$dblChangePercent),],x=dblChangePercent,type="histogram")
plot_ly(factor_rtns[factor_rtns$sFactorName=='rStreetSentiment'&!is.na(factor_rtns$dblChangePercent),],x=dblChangePercent,type="histogram")
plot_ly(factor_rtns[factor_rtns$sFactorName=='rSize'&!is.na(factor_rtns$dblChangePercent),],x=dblChangePercent,type="histogram")
plot_ly(factor_rtns[factor_rtns$sFactorName=='rSectorTrendExtension'&!is.na(factor_rtns$dblChangePercent),],x=dblChangePercent,type="histogram")
plot_ly(factor_rtns[factor_rtns$sFactorName=='rTrendExtension'&!is.na(factor_rtns$dblChangePercent),],x=dblChangePercent,type="histogram")
plot_ly(factor_rtns[factor_rtns$sFactorName=='rEarnings'&!is.na(factor_rtns$dblChangePercent),],x=dblChangePercent,type="histogram")
plot_ly(factor_rtns[factor_rtns$sFactorName=='rGrowthScore'&!is.na(factor_rtns$dblChangePercent),],x=dblChangePercent,type="histogram")
plot_ly(factor_rtns[factor_rtns$sFactorName=='rValueScore'&!is.na(factor_rtns$dblChangePercent),],x=dblChangePercent,type="histogram")
plot_ly(factor_rtns[factor_rtns$sFactorName=='rVolatility'&!is.na(factor_rtns$dblChangePercent),],x=dblChangePercent,type="histogram")
plot_ly(factor_rtns[factor_rtns$sFactorName=='rStrength'&!is.na(factor_rtns$dblChangePercent),],x=dblChangePercent,type="histogram")

#Check risk budget for recent positions
