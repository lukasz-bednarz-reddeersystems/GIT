sourceTo("../common/ppmodel_objectstore/ppmodel_objectstore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/ppmodel_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(testthat)

#########################
#
# Test  Vectors
#
#########################

tested.class  <-  "PPModelObjectStore"
valid.trader_id <- 11
valid.end_date  <- "2016-04-01"
valid.key_func <- dated_three_day_lookback
valid.keys <- valid.key_func(valid.trader_id, valid.end_date)
valid.ppmodel <- new('TradeHistorySimple', keys = valid.keys )

valid.store_keys <- data.frame(model_class = "TradeHistorySimple",
                               id = valid.trader_id, 
                         start = min(valid.keys$start),
                         end = max(valid.keys$end))

valid.ppmodel <- runPreProcessorModel(valid.ppmodel)
valid.ids <- get_ppmodel_objectstore_name(valid.store_keys)

#########################
#
# PPModelObjectStore Tests
#
#########################


test_that(paste("Can create", tested.class, "object"), {
  for(id in valid.ids) {
    expect_is(new(tested.class, id), tested.class)
  }
})

test_that(paste("Store the model in the ", tested.class) ,{
  
  ppm_store <- ppmodel_objectstore_factory(valid.ids) 
  
  ppm_store <- updatePPModelStore(ppm_store, valid.ppmodel, valid.store_keys, force = true)
  
  ppm_store <- commitPPModelStore(ppm_store)
  
})

test_that(paste("Can retrieve the model of the class ", tested.class) ,{
  
  ppm_store <- ppmodel_objectstore_factory(valid.ids) 
  
  
  
})