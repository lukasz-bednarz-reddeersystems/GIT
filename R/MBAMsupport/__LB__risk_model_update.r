sourceTo("risk_model_update_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("risk_model_load.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
library(gtools)

###### Update Object Store
lookback      <- 150
month         <- '2016-05-13'

model_prefix       <- 'developed_europe_prototype'

#update_risk_model_on_date(model_prefix, month, lookback, force = FALSE)




###### Load Updated model
dwh <- risk_model_objectstore_factory("developed_europe_prototype_2016-05")



###### Read back data

rm <- list()

for (cmp_name in dwh@components) {
  
  rm[[cmp_name]] <- queryDailyRiskModelObjectStore(dwh,"developed_europe_prototype_2016-05", lookback,cmp_name)
}


rm_day <- list()

for (cmp_name in dwh@components) {
   # getRiskModelComponentOnDate(object,name,component,date,lookback=150)
  rm_day[[cmp_name]] <- getRiskModelComponentOnDate(dwh,"developed_europe_prototype_2016-05",cmp_name, month, lookback)
}



###### Get or Generate Model ID from the database

insert_model_type(model_prefix, lookback)

rm_type <- get_model_type_id(model_prefix, lookback)

insert_model_definition(month, "2016-05-19" , lookback, model_prefix)

model_id <- query_model_id(rm_type, month, "2016-05-19" )

model_id <- get_model_id(month, "2016-05-19" , lookback, model_prefix)


###### Get or Generate Factor ID from the database

query_factor_id('GBP')

factors <- unique(colnames(rm_day$ImpliedFactorReturns)[-1])

for (factor in factors) {
  insert_factor(factor)
}

###### Get or Generate FactorCouples ID from the database

query_factor_couple_id('GBP', 'GBP')

factors <- get_factors()

factors <- factors[1:4,]

factor_combinations <- combinations( r= 2, v = factors$lFactorID, n = length(factors$lFactorID), repeats.allowed = TRUE)

apply(factor_combinations, 1, function(x){insert_factor_couple(x[1],x[2])})

get_factor_couples()


###### bulk load data to database

bulk_load_factor_betas(rm_day$Betas, rm_type)

bulk_load_implied_factor_returns(rm_day$ImpliedFactorReturns, rm_type)

bulk_load_factor_variances(rm_day$FactorVariance, model_id)

bulk_load_factor_correlations(rm_day$FactorCorrelation, model_id)

bulk_load_market_style(rm_day$MarketStyle, model_id)

bulk_load_residual_returns(rm_day$ResidualReturns, model_id)

###### query correlation matrix

