sourceTo("../lib/referencedata/referencedata.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/risk_model_objectstore_client/risk_model_objectstore_client.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)


####################################
#
# VirtualMarketStyleData Class
#
####################################


setClass(
  Class                = "VirtualMarketStyleData",
  prototype = list(
    required_colnms = c('Date',
                        risk_model_market_factors,
                        risk_model_currency_factors,
                        risk_model_commodity_factors,
                        risk_model_sector_factors)
  ),
  contains = c("VirtualReferenceData", "VIRTUAL")
)


setClass(
  Class             = "MarketStyleData",
  prototype      = list(
    component          = "MarketStyle", # name of component
    key_cols        = c(risk_model_objecstore_keys),
    key_values      = data.frame(Date = as.Date(character())),
    values             = c("Date",
                           risk_model_market_factors,
                           risk_model_currency_factors,
                           risk_model_commodity_factors,
                           risk_model_sector_factors), # columns that neeed to be returned from datastore
    column_name_map = hash()

    ),
  
  contains = c("VirtualMarketStyleData", "VirtualRiskModelObjectstoreClient")
)

