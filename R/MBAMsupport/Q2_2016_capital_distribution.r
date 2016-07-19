setwd("C:/Development/AllRaid/Services/Raid.Services.TradingEnhancementEngine/R/scripts")
source("../analysis_modules_legacy/analysis_module_position_holding_period.r")
source("../scripts/coaching_review_functions.r")
library(plotly)

trader <- 11
dates <- "2016-05-01"
history_data <- load_and_compute_market_rel(dated_twelve_monthly_lookback,trader,dates)
use_cols <- c('TradeDate','Instrument','Strategy','TodayPL','PassiveTodayPL','ActiveTodayPL','MarketRelPL','MinDate','MarketValue','EarliestMarketValue')
history_data <- position_age_from_flats(history_data,return_cols=use_cols)

pl_hd <- history_data
pl_hd <- pl_hd[pl_hd$PsnAge<70,]
pl_hd$Indicator <- 1
pl_by_age <- aggregate(pl_hd[c('TodayPL','PassiveTodayPL','ActiveTodayPL','MarketRelPL','Indicator')],list(Age=pl_hd$PsnAge),function(x)sum(x,na.rm=TRUE))
pl_by_age <- merge(pl_by_age,aggregate(pl_hd['MarketValue'],list(Age=pl_hd$PsnAge),function(x)mean(x,na.rm=TRUE)),by='Age')
pl_by_age <- remove_nan(pl_by_age)
pl_by_age[is.na(pl_by_age)] <- 0

plt_cum_pl_data <- rbind(cbind(Type='Cumulative PL',Quantity='Total PL',data.frame(Age=pl_by_age$Age,PL=cumsum(pl_by_age$TodayPL))),
                         cbind(Type='Cumulative PL',Quantity='MarketRel PL',data.frame(Age=pl_by_age$Age,PL=cumsum(pl_by_age$MarketRelPL))),
                         cbind(Type='Cumulative PL',Quantity='Passive PL',data.frame(Age=pl_by_age$Age,PL=cumsum(pl_by_age$PassiveTodayPL))),
                         cbind(Type='Cumulative PL',Quantity='Active PL',data.frame(Age=pl_by_age$Age,PL=cumsum(pl_by_age$ActiveTodayPL))),
                         cbind(Type='Av. Market Value',Quantity='$ Value',data.frame(Age=pl_by_age$Age,PL=pl_by_age$MarketValue)),
                         cbind(Type='Av. Number positions',Quantity='N. Psns',data.frame(Age=pl_by_age$Age,PL=(pl_by_age$Indicator/252))),
                         cbind(Type='Av. Capital distribution',Quantity='$ at age',data.frame(Age=pl_by_age$Age,PL=pl_by_age$MarketValue*(pl_by_age$Indicator/252))))
cum_tpl_smmry <- ggplot(plt_cum_pl_data,aes(x=as.numeric(Age),y=PL,group=Quantity,colour=Quantity)) + 
  geom_line(size=1) +
  ylab("") + 
  xlab("Position Age") + 
  labs(colour="") +
  ggtitle('Position PL and size by age') +
  theme(legend.position = "bottom") +
  theme(text = element_text(size=15)) +
  facet_grid(Type~.,scales="free_y")