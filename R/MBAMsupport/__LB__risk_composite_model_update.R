library(TE.RiskModel)
library(gtools)
library(lubridate)

###### Update Object Store

lookback      <- 150
start         <- as.Date('2015-01-01')
end        <-as.Date('2015-12-31')
comp_date     <- today()
model_prefix       <- 'developed_europe_prototype'


dates <-  seq(from= start, to = end, by = '1 month')
dates <-  c(dates, seq(from= start, to = end, by = '1 month') %m+% months(1) %m-% days(1))
dates <- sort(dates)
copy_history <-  FALSE
force <-  TRUE
first  <-  TRUE

# profiling
#Rprof(filename = paste0(getwd(), "/Rprof.out"), memory.profiling = TRUE, gc.profiling = TRUE)

for (month in dates) {
  update_risk_model_on_date(model_prefix, as.Date(month), lookback, force, copy_history)
  force <- !force
  if (first) {
    first <- FALSE
  } else {
    copy_history <- !copy_history
  }

}

#Rprof(NULL)

###### Load Updated model

dwh <- risk_model_objectstore_factory("developed_europe_prototype_2016-05")

store_name <- getID(dwh)


###### Read back data

rm <- list()

for (cmp_name in dwh@components) {

  rm[[cmp_name]] <- queryDailyRiskModelObjectStore(dwh,store_name, lookback,cmp_name)
}


rm_day <- list()

for (cmp_name in dwh@components) {
   # getRiskModelComponentOnDate(object,name,component,date,lookback=150)
  rm_day[[cmp_name]] <- getRiskModelComponentOnDate(dwh,store_name,cmp_name, today() -3, lookback)
}



###### Get or Generate Model ID from the database

insert_model_type(model_prefix, lookback)

rm_type <- get_model_type_id(model_prefix, lookback)

insert_model_definition(month, comp_date , lookback, model_prefix)

model_id <- query_model_id(rm_type, month, comp_date )

model_id <- get_model_id(month, comp_date , lookback, model_prefix)


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


###### push all components to database:

rm_type <- get_model_type_id(model_prefix, lookback)
for (day in seq(from= start, to = today(), by = '5 months')) {

  dwh <- get_most_recent_model_objectstore(model_prefix, day, lookback)
  store_name <- getID(dwh)

  betas <- getData(queryDailyRiskModelObjectStore(dwh,getID(dwh), lookback,'Betas'))
  bulk_load_factor_betas(betas, rm_type)

  returns <- getData(queryDailyRiskModelObjectStore(dwh,getID(dwh), lookback,'ImpliedFactorReturns'))
  bulk_load_implied_factor_returns(returns, rm_type)
}


rm_type <- get_model_type_id(model_prefix, lookback)
days <-  seq(from= start, to = today() -1, by = '1 day')
days <- days[wday(days)!=7&wday(days)!=1]

first <-TRUE

for (day in days) {

  day <- as.Date(day)

  name  <- paste(model_prefix,format(day,'%Y-%m'),sep="_")

  if (first) {
    dwh <- get_most_recent_model_objectstore(model_prefix, day, lookback)
    first <- FALSE
  } else if(name != getID(dwh)) {
    dwh <- get_most_recent_model_objectstore(model_prefix, day, lookback)
  }

  insert_model_definition(as.Date(day), today(), lookback, model_prefix)

  model_id <- query_model_id(rm_type, as.Date(day), today() )



  store_name <- getID(dwh)

  data <- getRiskModelComponentOnDate(dwh,store_name, 'FactorVariance', day, lookback)
  bulk_load_factor_variances(data, model_id)

  data <- getRiskModelComponentOnDate(dwh,store_name, 'FactorCorrelation', day, lookback)
  bulk_load_factor_correlations(data, model_id)

  data <- getRiskModelComponentOnDate(dwh,store_name, 'MarketStyle', day, lookback)
  bulk_load_market_style(data, model_id)

  data <- getRiskModelComponentOnDate(dwh,store_name, 'ResidualReturns', day, lookback)
  bulk_load_residual_returns(data, model_id)

}

###### push all components to database for one day only:

day <- today() -1
dwh <- get_most_recent_model_objectstore(model_prefix, day, lookback)

update_risk_model_db(model_prefix, dwh, day, day, lookback)


