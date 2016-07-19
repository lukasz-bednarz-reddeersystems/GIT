sourceTo("../common/factor_datastore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

get <- c('rDaysSinceRel3MHigh','rDaysSinceRel3MLow','rDaysSinceRel12MHigh')
instruments <- c('46136')
dates <- c(as.Date('2015-02-02'),as.Date('2015-02-20'))
get_keys <- data.frame(lInstrumentID=instruments,dtDateTime=dates)

datastore <- new("StaticFactorDataStore")
datastore <- queryStore(datastore,get_keys,get)
#Repeat call does not query URL
datastore <- queryStore(datastore,get_keys,get)

sourceTo("../common/dealing_datastore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

get <- c('sInputDirection','sTradeRationale','dtTradeDate')
trader <- c('70')
dates <- c(as.Date('2015-07-01'),as.Date('2015-09-30'))
get_keys <- data.frame(dtTradeDate=dates,lTraderID=trader)

datastore <- new("DealingDataStore")
datastore <- queryStore(datastore,get_keys,get)
#Repeat call does not query URL
datastore <- queryStore(datastore,get_keys,get)