#' @include datastore.r global_configs.r
NULL

setClass(
  Class          = "RiskFactorReturnsDataStore",
  prototype      = prototype(
    dataset      = new("DataSet",key_cols=c('dtDateTime'), unique_rows = TRUE),
    urlparser    = new("URLParser",parser_type = "XMLToFrame",cstm_body_tag="prFactorRisk_GetFactorReturns_TraderAnalytics_Instrument"),
    urlquery     = new("URLQuery",root_url=middleware_urls@risk_fct_rtn_url,fields=c('start','end')),
    key_map      = new("KeyMap",key_generator=date_only_kgen_fn,key_columns=c('dtDateTime'))
  ), contains = c("DataStore.URL")
)
