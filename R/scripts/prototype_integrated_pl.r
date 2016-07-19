setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules/analysis_module_position_holding_period.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(plotly)

trader   <- 11
dates <- c("2016-01-01")
history_data <- analysis_module_load_mutiple(trader,dates,history_analysis_module_builder,dated_three_monthly_lookback)


instruments <- unique(history_data$Instrument)
history_data$CumulativePL <- history_data$TodayPL
history_data$CumulativePL[is.na(history_data$CumulativePL)] <- 0
history_data$IntegratedPL <- NA
for(ins in instruments){
  history_data[history_data$Instrument==ins,'CumulativePL'] <- cumsum(history_data[history_data$Instrument==ins,'CumulativePL'])
  history_data[history_data$Instrument==ins,'IntegratedPL'] <- cumsum(history_data[history_data$Instrument==ins,'CumulativePL'])
}

instruments_overall_offside <- c()
first <- TRUE
for(ins in instruments){
  if(history_data[history_data$Instrument==ins,'IntegratedPL'][length(history_data[history_data$Instrument==ins,'IntegratedPL'])]<0){
    instruments_overall_offside <- c(instruments_overall_offside,ins)
    if(first){
      offside_data <- history_data[history_data$Instrument==ins,] 
      first <-  FALSE
    }
    else{
      offside_data <- rbind(offside_data,history_data[history_data$Instrument==ins,] )
    }
  }
}

all_trades <- history_data[!is.na(history_data$TradeID),]
trades_in_offside_psns <- offside_data[!is.na(offside_data$TradeID)&offside_data$CumulativePL<0,]

p <- plot_ly(x=c('Offside','Total'),y=c(length(instruments_overall_offside),length(instruments)),type="bar")
q <- plot_ly(y=trades_in_offside_psns$DeltaSkew,type="box")
add_trace(q,y=all_trades$DeltaSkew,type='box')

plot_ly(x=all_trades$DeltaSkew,y=all_trades$DeltaSwing,mode="markers",color=all_trades$PnLOutof)
plot(x=all_trades$MarketValue*sign(all_trades$SkewInto)*(abs(all_trades$SkewInto)^0.33)*(all_trades$VolInto/10000)/1000,y=all_trades$MarketValue*(all_trades$VolInto/10000)/1000)

