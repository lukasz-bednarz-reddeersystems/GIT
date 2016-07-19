sourceTo("../common/daily_riskmodel_objectstore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

test_data <- data.frame(Date=as.Date('2015-01-01'),Instrument=1,Return=NA)

rm_store <- risk_model_objectstore_factory("developed_europe_test")
rm_store <- pushRiskModelComponent(rm_store,test_data,"developed_europe_test",150,'ResidualReturns')
commitDailyRiskModelObjectStore(rm_store)
data <- queryDailyRiskModelObjectStore(rm_store,"developed_europe_test",150,'ResidualReturns')