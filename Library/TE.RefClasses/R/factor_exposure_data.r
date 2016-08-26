#' @include rodbc_client.r
NULL

####################################
#
# FactorExposureData Class
#
####################################

setClass(
  Class     = "FactorExposureDataSQLQuery",
  prototype = list(
    db_name        = RISK_MODEL_DB(),
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


#' Concrete S4 class handling FactorExposureData
#'
#' @export

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

