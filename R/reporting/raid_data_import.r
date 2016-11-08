#sourceTo("../common/trading_analysis_datastore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

get_from_new_store <- function(store_type,get_keys,get){
	datastore <- new(store_type)
	datastore <- queryStore(datastore,get_keys,get)
	dataset   <- getLastResult(datastore)
	return(dataset@data)
}

get_trader_performance <- function(id,start,end){
	all_data <- get_trader_performance_simple(id,start,end)
	all_data <- all_data[c('PL','RelativeReturnOnInvested','ReturnOnInvested','Sharpe','MarketReturn')]
	all_data <- all_data[all_data$DateTime==start,setdiff(colnames(all_data),colnames(key_frame))]
	colnames(all_data) <- c('MONEY|PL.','PERCENT_1DP|Rl.Rtn.Inv.','PERCENT_1DP|Rtn.Inv.','QUANTITY_1DP|Shrp.','PERCENT_1DP|Mkt.')
	return(all_data)
}

get_trader_performance_simple <- function(id,start,end,mbam=FALSE){
	key_frame <- data.frame(TraderID=id,DateTime=c(start,end),MBAMLevel=mbam)  
	columns <- c('Trader','PL','RelativeReturnOnInvested','ReturnOnInvested','ReturnOnAllocated','Sharpe','MarketReturn','StdDeviationOfReturns','AnnualisedStdDeviationOfReturns','AnnualisedReturnOnInvested','AverageAllocated','AverageInvestedUSD')
	all_data <- get_from_new_store("TraderPerformanceDataStore",key_frame,columns)
	return(all_data)
}

get_portfolio_analysis <- function(id,start,end){
	key_frame <- data.frame(TraderID=id,DateTime=c(start,end))  
	columns <- c('AverageNumberOfPositions','AveragePositionSize','UpDays','DownDays','HitRatio','WinLossRatio','PainGainRatio','WinLossRatioExtracted')
	all_data <- get_from_new_store("PortfolioAnalysisDataStore",key_frame,columns)
	all_data <- all_data[all_data$DateTime==start,setdiff(colnames(all_data),colnames(key_frame))]
	colnames(all_data) <- c('QUANTITY_1DP|Av. Psns.','MONEY|Psn. Val.','INT|Up Days','INT|Down Days','PERCENT_1DP|Hit Ratio','QUANTITY_1DP|Win Loss','QUANTITY_1DP|Pain Gain','QUANTITY_1DP|Win Loss Extracted')
	return(all_data)
}

get_top_positions <- function(id,start,end){
	key_frame <- data.frame(TraderID=id,DateTime=c(start,end))  
	columns <- c('PL','Ticker','Age','AverageMarketValue','Return','RelativeReturn','Average1DVaRPosition')
	all_data <- get_from_new_store("TopPositionDataStore",key_frame,columns)
	all_data <- all_data[all_data$DateTime==start,setdiff(colnames(all_data),colnames(key_frame))]
	colnames(all_data) <- c('MONEY|PL','STRING|Ticker','INT|Age','QUANTITY_1DP|Av. Val','PERCENT_1DP|Return','PERCENT_1DP|Rel. Return','MONEY|Av. 1D VaR')
	return(all_data)	
}

get_bottom_positions <- function(id,start,end){
	key_frame <- data.frame(TraderID=id,DateTime=c(start,end))  
	columns <- c('PL','Ticker','Age','AverageMarketValue','Return','RelativeReturn','Average1DVaRPosition')
	all_data <- get_from_new_store("BottomPositionDataStore",key_frame,columns)
	all_data <- all_data[all_data$DateTime==start,setdiff(colnames(all_data),colnames(key_frame))]
	colnames(all_data) <- c('MONEY|PL','STRING|Ticker','INT|Age','QUANTITY_1DP|Av. Val','PERCENT_1DP|Return','PERCENT_1DP|Rel. Return','MONEY|Av. 1D VaR')
	return(all_data)	
}

get_single_url <- function(url){
	 urlparser <- new("URLParser",parser_type = "XMLToFrame")
     urlparser <- runURLs(urlparser,c(url))
     return(urlparser@objects[[url]]@converter@data_obj)
}