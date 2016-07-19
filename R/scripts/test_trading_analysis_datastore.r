sourceTo("../common/trading_analysis_datastore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

#trader performance
trader <- 11
dates <- c(as.Date('2015-10-01'),as.Date('2015-10-31'))
get_keys <- data.frame(TraderID=trader,DateTime=dates)
get <- c("PL","Sharpe")

tp_datastore <- new("TraderPerformanceDataStore")
tp_datastore <- queryStore(tp_datastore,get_keys,get)
trader_performance <- getLastResult(tp_datastore)

#portfolio analysis
trader <- 11
dates <- c(as.Date('2015-10-01'),as.Date('2015-10-31'))
get_keys <- data.frame(TraderID=trader,DateTime=dates)
get <- c("WinLossRatio","PainGainRatio")

pa_datastore <- new("PortfolioAnalysisDataStore")
pa_datastore <- queryStore(pa_datastore,get_keys,get)
portfolio_analysis <- getLastResult(pa_datastore)

#top positions
trader <- 11
dates <- c(as.Date('2015-10-01'),as.Date('2015-10-31'))
get_keys <- data.frame(TraderID=trader,DateTime=dates)
get <- c("PL","Ticker")

tpsn_datastore <- new("TopPositionDataStore")
tpsn_datastore <- queryStore(tpsn_datastore,get_keys,get)
top_positions <- getLastResult(tpsn_datastore)