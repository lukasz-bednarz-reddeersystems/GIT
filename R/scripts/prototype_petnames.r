setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
sourceTo("../analysis_modules/analysis_module_petnames_traded.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
pet_names <- pet_names_traded_analysis_module_builder

trader   <- 11
to_date  <- Sys.Date()
key_func <- function(){dated_three_monthly_lookback(trader,'2016-01-01')}

#Get data
pet_names <- createAnalysisModule(pet_names,key_func)
pet_names <- updateAnalysisModel(pet_names)
pet_names <- runAnalysisModule(pet_names)
df <- pet_names@ppmdl@modeldata@data

#Average monthly position level return
cdata <- unique(df[c('TradeID','NLegs','LegSpan','PsnReturn')])
cmbn <- expand.grid(x=1:max(cdata$NLegs,na.rm=TRUE),y=0:max(cdata$LegSpan,na.rm=TRUE))
data_matrix <- matrix(data=0,nrow=max(cdata$NLegs,na.rm=TRUE),ncol=1+max(cdata$LegSpan,na.rm=TRUE))
list_df <- data.frame(x=cmbn[,1],y=cmbn[,2],z=0)
for(r in 1:nrow(cdata)){
  cd <- cdata[r,]
  data_matrix[cd$NLegs,cd$LegSpan+1] <- data_matrix[cd$NLegs,cd$LegSpan+1]+cd$PsnReturn
  if(!is.na(cd$NLegs)&!is.na(cd$LegSpan)&!is.na(cd$PsnReturn)){
    list_df[list_df$x==cd$NLegs&list_df$y==cd$LegSpan,'z'] <- list_df[list_df$x==cd$NLegs&list_df$y==cd$LegSpan,'z']+cd$PsnReturn
  }
}
data_matrix <- data_matrix/nrow(cdata)
list_df$z <- list_df$z/nrow(cdata)
image(data_matrix)
psn_surface <- loess(z ~ x*y, data = list_df, degree = 2, span = 0.4)
z = predict(psn_surface, newdata = cmbn)
image(z)

#Average pnl out of trades 
tdata <- unique(df[c('TradeDate','Instrument','LegSpan','NLegs','PnLOutof')])
trade_data_matrix <- matrix(data=0,nrow=max(tdata$NLegs,na.rm=TRUE),ncol=max(tdata$LegSpan,na.rm=TRUE))
for(r in 1:nrow(tdata)){
  cd <- tdata[r,]
  trade_data_matrix[cd$NLegs,cd$LegSpan] <- trade_data_matrix[cd$NLegs,cd$LegSpan]+cd$PnLOutof
}
trade_data_matrix <- trade_data_matrix/nrow(tdata)

#Define context
pets <- df[df$LegSpan>24&df$NLegs>2,]


