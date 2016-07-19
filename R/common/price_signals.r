source("../common/encoder.r")

setClass(
	Class          = "PriceSignals",
	representation = representation(
		signals    = "numeric",
		owner      = "character"
	)
	contains       = "Encoder" 
)

setGeneric("scanSignals",function(object,start_date,end_date){standardGeneric("scanSignals")})
setMethod("scanSignals","PriceSignals",
		 function(object,start_date,end_date){
		 	first <- TRUE
		 	message("Scanning signals...")
			for(d in traded_dates){
  				for(s in signal_subset$ID){
    				instruments <- data_request("watchlist",data.frame(TraderID=11,WatchlistID=s,Date=as.Date(d)),c('InstrumentID'))
    				if(nrow(instruments@data)>0){
      					if(first){
        					watchlist_content <- cbind(Signal=s,instruments@data)
        					first <- FALSE
      					} else{
        					watchlist_content <- rbind(watchlist_content,cbind(Signal=s,instruments@data))
      					} 
    				}
  				}
			}
			object <- setOrGrowDictionary(object,watchlist_content)	
		 }
)
