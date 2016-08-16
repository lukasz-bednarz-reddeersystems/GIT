sourceTo("../lib/referencedata/referencedata.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/risk_model_objectstore_client/risk_model_objectstore_client.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)


####################################
#
# InstrumentBetasData Class
#
####################################


setClass(
  Class                = "VirtualInstrumentBetasData",
  prototype = list(
    required_colnms = c('InstrumentID','Date',
                        risk_model_market_factors,
                        risk_model_currency_factors,
                        risk_model_commodity_factors,
                        risk_model_sector_factors)
  ),
  contains = c("VirtualReferenceData", "VIRTUAL")
)



setClass(
  Class             = "InstrumentBetasData",
  prototype      = list(
    component          = "Betas", # name of component
    key_cols        = c(risk_model_objecstore_keys, "InstrumentID"),
    key_values      = data.frame(Date = as.Date(character()),
                                 InstrumentID = integer()),
    values             = c("Date", "Instrument",
                           risk_model_market_factors,
                           risk_model_currency_factors,
                           risk_model_commodity_factors,
                           risk_model_sector_factors), # columns that neeed to be returned from datastore
    column_name_map = hash(c("Instrument", "InstrumentID"), 
                           c("InstrumentID","Instrument"))

    ),
  
  contains = c("VirtualInstrumentBetasData", "VirtualRiskModelObjectstoreClient")
)

