sourceTo("../models/ppmodel_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/trade_feature_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

pp_model_test <- "StopTargetsGatherer"

keys <- last_month(11)
test_batch_gather <- new(pp_model_test,keys=keys)
test_batch_gather@warehouse_store_name <- "11_2015-11-30_2015-12-31"
test_batch_gather <- runPreProcessorModel(test_batch_gather)