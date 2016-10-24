#' @include datastore.r global_configs.r
NULL

setClass(
  Class          = "TraderPerformanceDataStore",
  prototype      = prototype(
    dataset      = new("DataSet",key_cols=c('TraderID','DateTime')),
    urlparser    = new("URLParser",parser_type = "XMLToFrame",cstm_body_tag="DocumentElement"),
    urlquery     = new("URLQuery",root_url=middleware_urls@trader_perf_url,fields=c('id','start','end','mbam')),
    key_map      = new("KeyMap",key_generator=date_trader_mbam_kgen_fn,key_columns=c('TraderID','DateTime','MBAMLevel'))
  ), contains = c("DataStore.URL")
)

setClass(
  Class          = "PortfolioAnalysisDataStore",
  prototype      = prototype(
    dataset      = new("DataSet",key_cols=c('TraderID','DateTime')),
    urlparser    = new("URLParser",parser_type = "XMLToFrame",cstm_body_tag="DocumentElement"),
    urlquery     = new("URLQuery",root_url=middleware_urls@ratios_url,fields=c('id','start','end')),
    key_map      = new("KeyMap",key_generator=date_trader_kgen_fn,key_columns=c('TraderID','DateTime'))
  ), contains = c("DataStore.URL")
)

setClass(
  Class          = "TopPositionDataStore",
  prototype      = prototype(
    dataset      = new("DataSet",key_cols=c('TraderID','DateTime')),
    urlparser    = new("URLParser",parser_type = "XMLToFrame",cstm_body_tag="DocumentElement"),
    urlquery     = new("URLQuery",root_url=middleware_urls@top_psn_url,fields=c('id','start','end')),
    key_map      = new("KeyMap",key_generator=date_trader_kgen_fn,key_columns=c('TraderID','DateTime'))
  ), contains = c("DataStore.URL")
)

setClass(
  Class          = "BottomPositionDataStore",
  prototype      = prototype(
    dataset      = new("DataSet",key_cols=c('TraderID','DateTime')),
    urlparser    = new("URLParser",parser_type = "XMLToFrame",cstm_body_tag="DocumentElement"),
    urlquery     = new("URLQuery",root_url=middleware_urls@bottom_psn_url,fields=c('id','start','end')),
    key_map      = new("KeyMap",key_generator=date_trader_kgen_fn,key_columns=c('TraderID','DateTime'))
  ), contains = c("DataStore.URL")
)
