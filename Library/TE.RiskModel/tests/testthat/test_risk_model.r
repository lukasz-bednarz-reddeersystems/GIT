context("Testing Risk Model Classes")

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

valid.risk_model_market_factors    <- risk_model_market_factors
valid.risk_model_currency_factors  <- risk_model_currency_factors
valid.risk_model_commodity_factors <- risk_model_commodity_factors
valid.risk_model_sector_factors    <- risk_model_sector_factors

test_that(paste("Can create", tested.class, "object"), {
  expect_is(new(tested.class), tested.class)
})


test_that(paste("Can use basic accessors of ", tested.class, "object"), {

  object <- new(tested.class)
  expect_is(object, tested.class)

  expect_equal(getRiskModelPrefix(object), valid.model_prefix)

  expect_equal(getRiskModelName(object), valid.model_name)

  expect_equal(getRiskModelUniverse(object), valid.model_universe)

  expect_equal(getRiskModelLookback(object), valid.lookback)

  expect_equal(getRiskModelFactorNames(object), valid.factor_names)

  expect_equal(getRiskModelCommodityFactorNames(object), valid.risk_model_commodity_factors)

  expect_equal(getRiskModelCurrencyFactorNames(object), valid.risk_model_currency_factors)

  expect_equal(getRiskModelMarketFactorNames(object), valid.risk_model_market_factors)

  expect_equal(getRiskModelSectorFactorNames(object), valid.risk_model_sector_factors)
})

##############################################
#
# RiskModel.DevelopedEuropePrototype150.1.1 Tests
#
##############################################
tested.class          <- "RiskModel.DevelopedEuropePrototype150.1.1"
valid.model_prefix    <- "developed_europe_prototype.1.1"
valid.model_name      <- "developed_europe_prototype.1.1"
valid.model_universe  <- "developed_europe"
valid.lookback        <- 150L
valid.factor_names    <-  c(risk_model_market_factors,
                            setdiff(risk_model_currency_factors, c("HKD", "DKK")),
                            risk_model_commodity_factors,
                            risk_model_sector_factors)

valid.risk_model_market_factors    <- risk_model_market_factors
valid.risk_model_currency_factors  <- setdiff(risk_model_currency_factors, c("HKD", "DKK"))
valid.risk_model_commodity_factors <- risk_model_commodity_factors
valid.risk_model_sector_factors    <- risk_model_sector_factors

test_that(paste("Can create", tested.class, "object"), {
  expect_is(new(tested.class), tested.class)
})


test_that(paste("Can use basic accessors of ", tested.class, "object"), {

  object <- new(tested.class)
  expect_is(object, tested.class)

  expect_equal(getRiskModelPrefix(object), valid.model_prefix)

  expect_equal(getRiskModelName(object), valid.model_name)

  expect_equal(getRiskModelUniverse(object), valid.model_universe)

  expect_equal(getRiskModelLookback(object), valid.lookback)

  expect_equal(getRiskModelFactorNames(object), valid.factor_names)

  expect_equal(getRiskModelCommodityFactorNames(object), valid.risk_model_commodity_factors)

  expect_equal(getRiskModelCurrencyFactorNames(object), valid.risk_model_currency_factors)

  expect_equal(getRiskModelMarketFactorNames(object), valid.risk_model_market_factors)

  expect_equal(getRiskModelSectorFactorNames(object), valid.risk_model_sector_factors)


})



