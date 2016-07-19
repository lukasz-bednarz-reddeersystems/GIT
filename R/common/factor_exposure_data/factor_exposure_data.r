sourceTo("../lib/referencedata/referencedata.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/rodbc_client/rodbc_client.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/rodbc_client/rodbc_client_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)


####################################
#
# FactorExposureDataSQLQuery Class
#
####################################

setClass(
  Class     = "FactorExposureDataSQLQuery",
  prototype = list(
    db_schema      = "Razor", 
    key_cols       = c("InstrumentID", "Date"),
    key_values     = data.frame(lInstrumentID = integer(), 
                                dtDateTime = as.Date(character())),
    query_parser   = parse_instrument_date_keys,
    results_parser = convert_column_class,
    arguments    = c("@sInstrumentIDs", "@dtStart", "@dtEnd"),
    procedure    = "prFactorRisk_GetScores_TraderAnalytics_InstrumentList"
  ),
  contains  = c("VirtualSQLProcedureCall")
)


setClass(
  Class             = "VirtualFactorExposureData",
  prototype      = list(
    required_colnms = c("FactorName", "Date", "InstrumentID",
                        "ZScore", "Value")
  ),
  
  contains = c("VirtualReferenceData")
)

setClass(
  Class             = "FactorExposureData",
  prototype      = list(
    key_cols        = c("InstrumentID", "Date"),
    values          = c("lInstrumentID","dtDateTime","lFactorRiskInstrumentID", "sFactorName",  
                        "dblZScore", "dblValue"),
    column_name_map = hash(c("lFactorRiskInstrumentID", "sFactorName", "dtDateTime", "lInstrumentID",
                             "dblZScore", "dblValue"), 
                           c("FactorRiskInstrumentID", "FactorName", "Date", "InstrumentID",
                             "ZScore", "Value")),
    key_values      = data.frame(InstrumentID = integer(), 
                                 Date = as.Date(character())),
    sql_query       = new("FactorExposureDataSQLQuery")
  ),
  
  contains = c("VirtualFactorExposureData", "VirtualRODBCClient")
)


# setMethod("dataRequest",  
#           signature(object = "FactorExposureData", key_values = "data.frame"),
#           function(object, key_values){
#             
#             object <- callNextMethod()
#             
#             data <- getReferenceData(object)
#             
#             colnames(key_values) <- .translateDataSourceColumnNames(object, colnames(key_values))
#             
#             ret_data <- merge(data, key_values)
#             
#             object <- setReferenceData(object, ret_data)
#             
#             return(object)
#           }
# )