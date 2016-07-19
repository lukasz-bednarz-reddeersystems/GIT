sourceTo("../analysis_modules/module_allocation_turnover_by_strategy.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/ppmodel_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

trader   <- 101
date     <- '2016-05-01'
key_func <- function(){dated_four_monthly_lookback(trader,date)}
key_hash <- as.character(murmur3.32(as.character(key_func())))
kv <- key_func()
hrname <- paste(kv[[1,1]],"_",as.character(min(kv[['start']])),"_",as.character(max(kv[['end']])),sep="")
analysis_store <- analysis_objectstore_factory(paste("analysis_store_",hrname,sep=""))
ppmdl <- queryAnalysisStore(analysis_store,data.frame(key_hash=key_hash,analysis_module="TradeHistory"))

test_module <- new("AllocationTurnoverByStrategy",date=as.Date(date))
test_module <- generateBaseData(test_module,ppmdl@modeldata@data)
test_module <- generateOutputData(test_module)
test_module <- generateOutputObjects(test_module)