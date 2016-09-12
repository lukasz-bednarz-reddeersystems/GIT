context("Testing VirtualStrategyData")

#############################
#
# VirtualStrategyData Tests
#
#############################

tested.class          <-  "VirtualStrategyData"

test_that(paste("Canot create", tested.class, "object"), {
  expect_error(new(tested.class), regexp = "trying to generate an object from a virtual class")
})
