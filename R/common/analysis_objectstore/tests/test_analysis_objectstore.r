library(R.utils)
options(modifiedOnlySource=TRUE)
sourceTo("../common/analysis_objectstore/analysis_objectstore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/strategy_breakdown_aum_and_turnover/strategy_breakdown_aum_and_turnover.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../models/key_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(testthat)

tested.class  <-  "AnalysisObjectStore"
valid.trader_id <- 11
valid.end_date  <- "2016-01-01"
valid.key_func <- dated_three_monthly_lookback
valid.key_values <- valid.key_func(valid.trader_id, valid.end_date)
colnames(valid.key_values) <- c("TraderID", "start", "end")
valid.block_class <- "StrategyBreakdownAnalysisBlock"
valid.block <- new(valid.block_class)
valid.block <- dataRequest(valid.block, valid.key_values)
valid.block <- Process(valid.block)

#########################
#
# AnalysisObjectStore Tests
#
#########################

test_that(paste("Can create", tested.class, "object"), {
  
  block_store_name <- get_analysis_objectstore_name(valid.key_values) 
  
  expect_is(new(tested.class,id=block_store_name), tested.class)
})

test_that(paste("Store the block in the ", tested.class) ,{
  
  block_store_name <- get_analysis_objectstore_name(valid.key_values) 
  
  block_store <- analysis_objectstore_factory(block_store_name)
  
  kh <- as.character(murmur3.32(as.character(valid.key_values)))
  block_store <- updateAnalysisStore(block_store,valid.block,data.frame(key_hash=kh,analysis_module=class(valid.block)[[1]]))

  block_store <- commitAnalysisStore(block_store)
  
})

test_that(paste("Can retrieve the block from the class ", tested.class) ,{
  
  block_store_name <- get_analysis_objectstore_name(valid.key_values) 
  
  block_store <- analysis_objectstore_factory(block_store_name)

  kh <- as.character(murmur3.32(as.character(valid.key_values)))
  block <- queryAnalysisStore(block_store,data.frame(key_hash=kh,analysis_module=class(valid.block)[[1]]))

  expect_is(block,valid.block_class)

})