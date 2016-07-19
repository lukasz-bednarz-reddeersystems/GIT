sourceTo("../lib/referencedata/referencedata.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/datastore_client/datastore_client.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

####################################
#
# DealerData Class
#
####################################

setClass(
  Class             = "DealerData",
  prototype      = list(
    datastore_name = "dealing_datastore",
    key_cols        = c("lTraderID", "dtTradeDate"),
    values          = c("lTraderID","dtTradeDate","lInstrumentID","sTradeRationale","sInputDirection"),
    required_colnms = c("Date","InstrumentID","Rationale","InputDirection"),
    factorized_cols = c("InputDirection"),
    factorization_keys = c("Date","InstrumentID"), # key columns that are used as a unique keys in factorization
    non_na_cols     = character(),
    column_name_map = hash(c("lTraderID","dtTradeDate","lInstrumentID","sTradeRationale","sInputDirection"), 
                           c("TraderID","Date","InstrumentID","Rationale","InputDirection")),
    key_values      = data.frame(lTraderID = integer(), 
                                 dtTradeDate = as.Date(character()))
    ),
  
  contains = c("VirtualDataStoreClient")
)


