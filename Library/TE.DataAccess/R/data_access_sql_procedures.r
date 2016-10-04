#' @include TE.DataAccess.r
NULL


#' DataAccess.SQLProcedureCall.Query_HistoricalTrades_WithInstrumentIDAndOrderID class
#'
#' Implements handling querries for raw trade data for given trader_id and date range
#'
#' Inherits from "VirtualSQLProcedureCall"
#' @rdname Query_HistoricalTrades_WithInstrumentIDAndOrderID-class
#' @export
setClass(
  Class     = "DataAccess.SQLProcedureCall.Query_HistoricalTrades_WithInstrumentIDAndOrderID",
  prototype = list(
    db_name    = .__DEFAULT_ODBC_DB_NAME__.,
    db_schema  = .__DEFAULT_DB_SCHEMA__.,
    key_cols   = c("TraderID", "DateStart", "DateEnd"),
    key_values = data.frame(TraderID = integer(),
                            DateStart = as.Date(character()),
                            DateStart = as.Date(character())),
    arguments    = c("@sTraderIDs", "@dtFrom", "@dtTo"),
    column_name_map = hash(c("dtCreated", "lOrderID", "dtTradeDate", "sTrader", "sStrategy", "sTicker", "lInstrumentID", "sUlyTicker", "sDirection", "dblValueUSD"),
                           c("Created", "OrderID", "TradeDate", "Trader", "Strategy", "Ticker", "InstrumentID", "UlyTicker", "Direction", "ValueUSD")),
    procedure    = "prQuery_HistoricalTrades_WithInstrumentIDAndOrderID",
    results_parser = TE.SQLQuery:::convert_column_class
  ),
  contains  = c("VirtualSQLProcedureCall")
)


#' DataAccess.SQLProcedureCall.Query_HistoricalTrades_WithInstrumentIDAndOrderID class
#'
#' Implements handling querries for raw trade data for given trader_id and date range
#'
#' Inherits from "VirtualSQLProcedureCall"
#' @rdname Query_HistoricalTrades_WithInstrumentIDAndOrderID-class
#' @export
setClass(
  Class     = "DataAccess.SQLProcedureCall.InstrumentHistoryRequired_QueryPriceHistoryFromTQA",
  prototype = list(
    db_name    = "RAIDQUANTDB",
    db_schema  = "QuantSplus",
    key_cols   = c("InstrumentID", "DateStart", "DateEnd"),
    key_values = data.frame(InstrumentID = integer(),
                            DateStart = as.Date(character()),
                            DateStart = as.Date(character())),
    arguments    = c("@lInstrumentID", "@dtFrom", "@dtTo"),
    column_name_map = hash(c("lInstrumentID",	"dtDateTime",	"dblClosePrice",	"dblOpenPrice",	"dblHigh",	"dblLow",	"dblVolume",	"lOutstandingShares",	"dblTurnover",	"dblVWAP"),
                           c("InstrumentID",	"DateTime",	"ClosePrice",	"OpenPrice",	"High",	"Low",	"Volume",	"OutstandingShares",	"Turnover",	"VWAP")),
    procedure    = "prInstrumentHistoryRequired_QueryPriceHistoryFromTQA",
    results_parser = TE.SQLQuery:::convert_column_class
  ),
  contains  = c("VirtualSQLProcedureCall")
)








# lInstrumentID	dtDateTime	dblClosePrice	dblOpenPrice	dblHigh	dblLow	dblVolume	lOutstandingShares	dblTurnover	dblVWAP
# 4454	2016-09-01	264.5	267.2	267.6	263.5	352630	197515200	93270635	264.92
# 4454	2016-09-02	266.4	264.5	266.8	263	370508	197515200	98703297.85428	264.93
