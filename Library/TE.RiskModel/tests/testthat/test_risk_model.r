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
valid.factor_names    <-  sort(c(risk_model_market_factors,
                                 risk_model_currency_factors,
                                 risk_model_commodity_factors,
                                 risk_model_sector_factors))

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

test_that(paste("Can use getRiskModeCommodityFactorReturns() of ",
                tested.class, "object"), {

  date_start <- as.Date("2016-06-01")
  date_end   <- as.Date("2016-06-30")

  object <- new(tested.class)
  expect_is(object, tested.class)


  factors <- getRiskModelCommodityFactorNames(object)
  ret <- getRiskModelCommodityFactorReturns(object, date_start, date_end)

  expect_is(ret, "data.frame")
  expect_gt(nrow(ret), 0)
  expect_true(setequal(intersect(colnames(ret), factors),factors))

})

test_that(paste("Can use getRiskModeCurrencyFactorReturns() of ",
                tested.class, "object"), {

  date_start <- as.Date("2016-06-01")
  date_end   <- as.Date("2016-06-30")

  object <- new(tested.class)
  expect_is(object, tested.class)


  factors <- getRiskModelCurrencyFactorNames(object)
  ret <- getRiskModelCurrencyFactorReturns(object, date_start, date_end)

  expect_is(ret, "data.frame")
  expect_gt(nrow(ret), 0)
  expect_true(setequal(intersect(colnames(ret), factors),factors))

})


test_that(paste("Can use getRiskModeMarketFactorReturns() of ",
                tested.class, "object"), {

  date_start <- as.Date("2016-06-01")
  date_end   <- as.Date("2016-06-30")

  object <- new(tested.class)
  expect_is(object, tested.class)


  factors <- getRiskModelMarketFactorNames(object)
  ret <- getRiskModelMarketFactorReturns(object, date_start, date_end)

  expect_is(ret, "data.frame")
  expect_gt(nrow(ret), 0)
  expect_true(setequal(intersect(colnames(ret), factors),factors))

})


test_that(paste("Can use getRiskModeSectorFactorReturns() of ",
                tested.class, "object"), {

  date_start <- as.Date("2016-06-01")
  date_end   <- as.Date("2016-06-30")

  object <- new(tested.class)
  expect_is(object, tested.class)


  factors <- getRiskModelSectorFactorNames(object)
  ret <- getRiskModelSectorFactorReturns(object, date_start, date_end)

  expect_is(ret, "data.frame")
  expect_gt(nrow(ret), 0)
  expect_true(setequal(intersect(colnames(ret), factors),factors))

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
valid.factor_names    <-  sort(c(risk_model_market_factors,
                                 setdiff(risk_model_currency_factors, c("HKD", "DKK")),
                                 risk_model_commodity_factors,
                                 risk_model_sector_factors))

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



