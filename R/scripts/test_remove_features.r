sourceTo("../common/warehouse_objectstore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

trader <- 101
start <- '2015-10-30'
end <- '2015-11-30'

store_name <- paste(trader,'_',start,'_',end,sep="")
wh_str <- warehouse_objectstore_factory(store_name)
wh <- getFromObjectStore(wh_str,wh_str@id)
wh_str <- updateWarehouseStoreForKey(wh_str,wh)

#wh_str <- tearDownAllFeatures(trader,as.Date(start),as.Date(end))

commitWarehouseStore(wh_str)