#' @include datastore.r global_configs.r
NULL

setClass(
  Class          = "ExtPosDataStore",
  prototype      = prototype(
    dataset      = new("DataSet",key_cols=c('Strategy','Date'),unique_rows=TRUE),
    urlparser    = new("URLParser",parser_type = "XMLToFrame"),
    urlquery     = new("URLQuery",root_url=middleware_urls@ext_psns_url,fields=c('strategy','start','end')),
    key_map      = new("KeyMap",key_generator=date_strat_kgen_fn,key_columns=c('Strategy','Date'))
  ), contains = c("DataStore.URL")
)
