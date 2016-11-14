#' @include datastore.r global_configs.r datastore_sql.r
NULL

#ToDo: could benefit from initialisation via a factory, using a configuration
#      class and a set of pre-defined key generation functions. That way the URL field
#      to XML tag name could be explicity defined.
setClass(
  Class          = "StaticFactorDataStore",
  prototype      = prototype(
    dataset      = new("DataSet",key_cols=c('lInstrumentID','dtDateTime'), unique_rows = TRUE),
    urlparser    = new("URLParser",parser_type = "XMLToFrame",cstm_body_tag="prInstrumentMatrixStaticHistory_GetFields"),
    urlquery     = new("URLQuery",root_url=middleware_urls@static_factors1,fields=c('id','start','end')),
    key_map      = new("KeyMap",key_generator=date_instrument_kgen_fn,key_columns=c('lInstrumentID','dtDateTime'))
  ), contains = c("DataStore.URL")
)


setClass(
  Class          = "StaticFeaturesFactorsDataStore",
  prototype      = prototype(
    dataset      = new("DataSet",key_cols=c('lInstrumentID','dtDateTime'), unique_rows = TRUE),
    key_map      = new("KeyMap",key_generator=date_instrument_kgen_fn,key_columns=c('lInstrumentID','dtDateTime')),
    sql_query    = new("DataStore.InstrumentMatrixStaticHistory_GetTEFeaturesFields")
  ), contains = c("DataStore.SQL")
)

#ToDo: could benefit from initialisation via a factory, using a configuration
#      class and a set of pre-defined key generation functions. That way the URL field
#      to XML tag name could be explicity defined.
setClass(
  Class          = "DynamicFactorDataStore",
  prototype      = prototype(
    dataset      = new("DataSet",key_cols=c('lInstrumentID','dtDateTime'), unique_rows = TRUE),
    urlparser    = new("URLParser",parser_type = "XMLToFrame",cstm_body_tag="prInstrumentMatrixHistory_GetFields"),
    urlquery     = new("URLQuery",root_url=middleware_urls@dynamic_factors,fields=c('id','start','end')),
    key_map      = new("KeyMap",key_generator=date_instrument_kgen_fn,key_columns=c('lInstrumentID','dtDateTime'))
  ), contains = c("DataStore.URL")
)
