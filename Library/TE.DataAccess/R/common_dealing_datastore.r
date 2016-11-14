#' @include datastore.r global_configs.r
NULL

setClass(
  Class          = "DealingDataStore",
  prototype      = prototype(
    dataset      = new("DataSet",key_cols=c('lTraderID','dtTradeDate')),
    urlparser    = new("URLParser",parser_type = "XMLToFrame",cstm_body_tag="prDealingOrder_SelectByCreatedDate"),
    urlquery     = new("URLQuery",root_url=middleware_urls@dealing_url,fields=c('id','start','end')),
    key_map      = new("KeyMap",key_generator=date_trader_kgen_fn,key_columns=c('lTraderID','dtTradeDate'))
  ), contains = c("DataStore.URL")
)
