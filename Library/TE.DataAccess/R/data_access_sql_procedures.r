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
                           c("Created", "OrderID", "TradeDate", "Trader", "Strategy", "Ticker", "InstrumentID", "UlyTicker", "BuySell", "ValueUSD")),
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


#' DataAccess.SQLProcedureCall.PositionHistory_SelectByTraderDate" class
#'
#' Implements handling querries for raw trade data for given trader_id and date range
#'
#' Inherits from "VirtualSQLProcedureCall"
#' @rdname PositionHistory_SelectByTraderDate-class
#' @export
setClass(
  Class     = "DataAccess.SQLProcedureCall.PositionHistory_SelectByTraderDate",
  prototype = list(
    db_name    = .__DEFAULT_ODBC_DB_NAME__.,
    db_schema  = .__DEFAULT_DB_SCHEMA__.,
    key_cols   = c("TraderID", "DateStart", "DateEnd"),
    key_values = data.frame(TraderID = integer(),
                            DateStart = as.Date(character()),
                            DateStart = as.Date(character())),
    arguments    = c("@lUserID", "@dtFrom", "@dtTo"),
    column_name_map = hash(c("lInstrumentID",	"Ticker",	"Date",	"lPosition",	"dblMarketValue",	"dblTodayPL",	"sStrategy",	"lStrategyID",	"lFunctionID",	"lDaysPositionHeld"),
                           c("InstrumentID",	"Ticker",	"Date",	"Position",	"MarketValue",	"TodayPL",	"Strategy",	"StrategyID",	"FunctionID",	"DaysPositionHeld")),
    procedure    = "prPositionHistory_SelectByTraderDate",
    results_parser = TE.SQLQuery:::convert_column_class
  ),
  contains  = c("VirtualSQLProcedureCall")
)


#' DataAccess.SQLProcedureCall.Strategy_SelectAll" class
#'
#' Implements handling querries for raw strategy data
#'
#' Inherits from "VirtualSQLProcedureCall"
#' @export
setClass(
  Class     = "DataAccess.SQLProcedureCall.Strategy_SelectAll",
  prototype = list(
    db_name    = .__DEFAULT_ODBC_DB_NAME__.,
    db_schema  = .__DEFAULT_DB_SCHEMA__.,
    key_cols   = c(),
    key_values = data.frame(),
    arguments    = c("@lUserID", "@dtFrom", "@dtTo"),
    column_name_map = hash(c("lUserID",	"lStrategyTypeID",	"lStrategyGroupID",	"lStrategyAliasID",	"lInitiatorID",	"lStrategyID",	"sStrategy",	"lFundGroupID"),
                           c("UserID",	"StrategyTypeID",	"StrategyGroupID",	"StrategyAliasID",	"InitiatorID",	"StrategyID",	"Strategy",	"FundGroupID")),
    procedure    = "prStrategy_SelectAll",
    results_parser = TE.SQLQuery:::convert_column_class
  ),
  contains  = c("VirtualSQLProcedureCall")
)

