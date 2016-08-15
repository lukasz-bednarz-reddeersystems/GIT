
##############################################
#
# VirtualRiskModel Tests
#
##############################################
tested.class          <-  "VirtualRiskModel"


test_that(paste("Canot create", tested.class, "object"), {
  expect_error(new(tested.class), regexp = "trying to generate an object from a virtual class")
})


##############################################
#
# RiskModel.DevelopedEuropePrototype150 Tests
#
##############################################
tested.class          <- "RiskModel.DevelopedEuropePrototype150"
valid.model_prefix    <- "developed_europe_prototype"
valid.model_name      <- "developed_europe_prototype"
valid.model_universe  <- "developed_europe"
valid.lookback        <- 150L
valid.factor_names    <-  c(risk_model_market_factors,
                            risk_model_currency_factors,
                            risk_model_commodity_factors,
                            risk_model_sector_factors)

test_that(paste("Can create", tested.class, "object"), {
  expect_is(new(tested.class), "RiskModel.DevelopedEuropePrototype150")
})


test_that(paste("Can use basic accessors of ", tested.class, "object"), {

  object <- new(tested.class)
  expect_is(object, tested.class)

  expect_equal(getRiskModelPrefix(object), valid.model_prefix)

  expect_equal(getRiskModelName(object), valid.model_name)

  expect_equal(getRiskModelUniverse(object), valid.model_universe)

  expect_equal(getRiskModelLookback(object), valid.lookback)

  expect_equal(getRiskModelFactorNames(object), valid.factor_names)


})

