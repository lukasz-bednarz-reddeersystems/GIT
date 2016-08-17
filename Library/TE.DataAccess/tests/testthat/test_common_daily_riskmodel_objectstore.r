context("Test Daily Risk Model Objectstore")

#############################
#
# FactorCorrelationTests
#
#############################

valid.component       <- "FactorCorrelation"
valid.model_prefix    <- "developed_europe_prototype"
valid.lookback        <- 150


test_that("Can get_most_recent_model_objectstore() with valid key_values", {

  valid.key_vals <- expand.grid(Date = seq(from = as.Date('2016-06-20'),
                                           to = as.Date('2016-06-23'),
                                           by = "1 day"))

  # create valid return data.frame
  start        <- min(valid.key_vals$Date)
  end          <- max(valid.key_vals$Date)
  rm_str       <- get_most_recent_model_objectstore(valid.model_prefix, end, valid.lookback)

  expect_is(rm_str, "DailyRiskModelObjectStore")

})



