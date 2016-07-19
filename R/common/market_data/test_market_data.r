sourceTo("../common/market_data/market_data.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(testthat)
library(plyr)
library(lubridate)

#########################
#
# MarketData Tests
#
#########################
tested.class          <-  "MarketData"
valid.key_cols        <- c("symbol", "start", "end")
valid.values          <- c("Close")
valid.required_colnms <- c("Date")
init.key_values       <-  data.frame(symbol = character(), 
                                     start = as.Date(character()), 
                                     end = as.Date(character()))


test_that(paste("Canot create", tested.class, "object with invalid initializer"), {
  expect_error(new(tested.class), tested.class, regexp = "argument \"symbol\" is missing, with no default")
})


test_that(paste("Can create", tested.class, "object with alid initializer"), {
  expect_is(new(tested.class, "^SX5P", today() - 10, today()), tested.class)
})


#########################
#
# MarketDataSP5X Tests
#
#########################

tested.class          <-  "MarketDataSX5P"

test_that(paste("Canot create", tested.class, "object with invalid initializer"), {
  expect_error(new(tested.class), tested.class, regexp = "argument \"start\" is missing, with no default")
})


test_that(paste("Can create", tested.class, "object with valid initializer"), {
  expect_is(new(tested.class, today() - 10, today()), tested.class)
})