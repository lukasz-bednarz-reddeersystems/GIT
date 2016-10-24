#' @include datastore.r global_configs.r
NULL

setClass(
  Class          = "TraderAllocationDataStore",
  prototype      = prototype(
    dataset      = new("DataSet",key_cols=c('lUserID','dtDateFrom'),unique_rows=TRUE),
    urlparser    = new("URLParser",parser_type = "XMLToFrame",cstm_body_tag="prTraderAllocatedCapitalHistory_Between"),
    urlquery     = new("URLQuery",root_url=middleware_urls@allocation_url,fields=c('start','end')),
    key_map      = new("KeyMap",key_generator=date_only_kgen_fn,key_columns=c('dtDateFrom'))
  ), contains = c("DataStore.URL")
)

