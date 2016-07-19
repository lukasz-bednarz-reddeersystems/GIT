sourceTo("../models/ppmodel_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../features/trade_feature_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/visualisation_snapshot.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

keys <- last_month(11)
test_batch_gather <- new("PriceBySnapshotGatherer",keys=keys)
test_batch_gather@warehouse_store_name <- "11_2015-11-30_2015-12-31"
test_batch_gather <- runPreProcessorModel(test_batch_gather)

snapshot <- new("SnapShot",aggregate_by=c('Long','AgeCategory'),subset_by=c('Long','AgeCategory'),subset_with=list(TRUE,'New'),y_label='Rel. Rtn',x_label='Days',title='Price snapshot')
#debug(snapshot@visuln_dspl)
snapshot <- triggerVisualisationComputation(snapshot,test_batch_gather@modeldata)
snapshot <- createVisualisation(snapshot)