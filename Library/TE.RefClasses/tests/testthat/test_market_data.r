context("Testing MarketData")

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


test_that(paste("Can create", tested.class, "object with valid initializer"), {
  expect_is(new(tested.class, "^SX5P", today() - 10, today()), tested.class)
})


#########################
#
# MarketDataSX5P Tests
#
#########################

tested.class          <-  "MarketDataSX5P"

test_that(paste("Canot create", tested.class, "object with invalid initializer"), {
  expect_error(new(tested.class), tested.class, regexp = "argument \"start\" is missing, with no default")
})


test_that(paste("Can create", tested.class, "object with valid initializer"), {
  expect_is(new(tested.class, today() - 10, today()), tested.class)
})


#########################
#
# MarketDataSX5E Tests
#
#########################

tested.class          <-  "MarketDataSX5E"

test_that(paste("Canot create", tested.class, "object with invalid initializer"), {
  expect_error(new(tested.class), tested.class, regexp = "argument \"start\" is missing, with no default")
})


test_that(paste("Can create", tested.class, "object with valid initializer"), {
  expect_is(new(tested.class, today() - 10, today()), tested.class)
})
