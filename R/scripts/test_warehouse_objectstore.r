sourceTo("../common/dataplex.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/warehouse_objectstore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)


trader <- as.integer(70)
start  <- as.Date("2015-10-30")
end    <- as.Date("2015-11-30")

wh_str <- warehouse_objectstore_factory("70_2015-10-30_2015-11-30")


wh_str <- queryWarehouseStore(wh_str,trader,start,end)
wh <- getWarehouseFromStore(wh_str,trader,start,end)
#wh_str <- pushSummary(wh_str,wh)
#commitWarehouseStore(wh_str)
