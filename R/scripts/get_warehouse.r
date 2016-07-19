sourceTo("../common/composite_warehouse.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
cwh <- readRDS("C:/Development/AllRaid/trunk/Services/Raid.Services.TradingEnhancementEngine/R/model_data/11_2016-02-29_2016-03-31_objectstore.rds")
cwh <- cwh[["11_2016-02-29_2016-03-31"]]
wh <- getWarehouse(cwh,'11_2016-02-29_2016-03-31')