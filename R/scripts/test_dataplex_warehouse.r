sourceTo("../common/dataplex.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/trade_factory.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/composite_warehouse.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/trade_feature_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/preprocessor_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

trader <- as.integer(11)
start  <- as.Date("2015-10-01")
end    <- as.Date("2015-10-15")

wh <- warehouse_request("trade_warehouse",trader,start,end)
wh <- attachFeatures(wh,c("Age","CompoundReturnOutof","MarketCap"))

warehouse_push_features("trade_warehouse",wh)