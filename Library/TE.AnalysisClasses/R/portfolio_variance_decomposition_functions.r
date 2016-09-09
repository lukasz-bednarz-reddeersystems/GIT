get_portfolio_decomposition_factor_groups <- function(risk_model) {

  # Lists for factor names
  market_factors    <- getRiskModelMarketFactorNames(risk_model)
  currency_factors  <- getRiskModelCurrencyFactorNames(risk_model)
  commodity_factors <- getRiskModelCommodityFactorNames(risk_model)
  sector_factors    <- getRiskModelSectorFactorNames(risk_model)

  # List of all portfolio decomposition factors
  all_factors <- c(market_factors,
                   currency_factors,
                   commodity_factors,
                   sector_factors)

  # List of all portfolio decomposition composed factors
  composition_factors <- c('TotalSystematic',
                           'MarketFactor',
                           'Currency',
                           'Commodity',
                           'Sector')

  # List of all portfolio decomposition factor groups
  factor_groups <- list(Composition = composition_factors,
                        Market = market_factors,
                        Currency = currency_factors,
                        Commodity = commodity_factors,
                        Sector= sector_factors)

 return(factor_groups)
}
