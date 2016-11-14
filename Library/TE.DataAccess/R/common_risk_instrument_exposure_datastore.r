#' @include datastore.r global_configs.r
NULL

setClass(
  Class          = "RiskInstrumentExposuresDataStore",
  prototype      = prototype(
    dataset      = new("DataSet",key_cols=c('lInstrumentID','dtDateTime')),
    urlparser    = new("URLParser",parser_type = "XMLToFrame",cstm_body_tag="prFactorRisk_GetScores_TraderAnalytics_Instrument"),
    urlquery     = new("URLQuery",root_url=middleware_urls@risk_exp_url,fields=c('id','start','end')),
    key_map      = new("KeyMap",key_generator=date_instrument_kgen_fn,key_columns=c('lInstrumentID','dtDateTime'))
  ), contains = c("DataStore.URL")
)
