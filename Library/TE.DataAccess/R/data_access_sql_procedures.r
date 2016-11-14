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
    arguments    = c("@lUserID", "@dtFrom"       , "@dtTo"),
    column_name_map = hash("STR:Description"    = "Description",
                           "STR:Direction"      = "Direction",
                           "STR:Strategy Type"  = "Type",
                           "STR:Strategy Group" = "Group",
                           "STR:Strategy Alias" = "Alias",
                           "STR:FundGroup"      = "FundGroup",
                           "STR:Trader"         = "Trader",
                           "STR:Active"         = "Active",
                           "STR:Strategy"       = "Name",
                           "lUserID"            = "UserID",
                           "lStrategyTypeID"    = "TypeID",
                           "lStrategyGroupID"   = "GroupID",
                           "lStrategyAliasID"   = "AliasID",
                           "lInitiatorID"       = "InitiatorID",
                           "lStrategyID"        = "StrategyID",
                           "sStrategy"          = "Strategy",
                           "lFundGroupID"       = "FundGroupID"
                           ),
    procedure    = "prStrategy_SelectAll",
    results_parser = TE.SQLQuery:::convert_column_class
  ),
  contains  = c("VirtualSQLProcedureCall")
)


#' DataAccess.SQLProcedureCall.PositionLevel_SelectFromHistoryByDate" class
#'
#' Implements handling querries for trade position level data
#'
#' Inherits from "VirtualSQLProcedureCall"
#' @rdname PositionLevel_SelectFromHistoryByDate-class
#' @export
setClass(
  Class     = "DataAccess.SQLProcedureCall.PositionLevel_SelectFromHistoryByDate",
  prototype = list(
    db_name    = .__DEFAULT_ODBC_DB_NAME__.,
    db_schema  = .__DEFAULT_DB_SCHEMA__.,
    key_cols   = c("InstrumentID", "DateStart", "DateEnd"),
    key_values = data.frame(InstrumentID = integer(),
                            DateStart = as.Date(character()),
                            DateStart = as.Date(character())),
    arguments    = c("@lInstrumentID", "@dtFrom" , "@dtTo"),
    column_name_map = hash("lInstrumentID"         = "InstrumentID",
                           "lStrategyID"           = "StrategyID",
                           "lTraderID"             = "TraderID",
                           "dtDateFrom"            = "DateFrom",
                           "dtDateTo"              = "DateTo",
                           "dblStopLoss"           = "StopLoss",
                           "dblProfitTarget"       = "ProfitTarget",
                           "sPlan"                 = "Plan",
                           "lPositionLevelTypeID"  = "PositionLevelTypeID",
                           "sPositionLevelType"    = "PositionLevelType",
                           "lPositionID"           = "PositionID"
                           ),
    procedure    = "prPositionLevel_SelectFromHistoryByDate",
    results_parser = TE.SQLQuery:::convert_column_class
  ),
  contains  = c("VirtualSQLProcedureCall")
)



#' DataAccess.SQLProcedureCall.PositionService_SelectHistoryBetweenForStrategy" class
#'
#' Implements handling querries for position history for given strategy
#' and date range
#'
#' Inherits from "VirtualSQLProcedureCall"
#' @rdname PositionService_SelectHistoryBetweenForStrategy-class
#' @export
setClass(
  Class     = "DataAccess.SQLProcedureCall.PositionService_SelectHistoryBetweenForStrategy",
  prototype = list(
    db_name    = .__DEFAULT_ODBC_DB_NAME__.,
    db_schema  = .__DEFAULT_DB_SCHEMA__.,
    key_cols   = c("Strategy", "DateStart", "DateEnd"),
    key_values = data.frame(sStrategy = integer(),
                            DateStart = as.Date(character()),
                            DateStart = as.Date(character())),
    arguments    = c("@sStrategy", "@dtFrom" , "@dtTo"),
    column_name_map = hash("sPositionKey"          = "PositionKey",
                           "dtDate"                = "Date",
                           "lInstrumentID"         = "InstrumentID",
                           "lQuantity"             = "Quantity",
                           "sExternalRef"          = "ExternalRef",
                           "dblUnitCostPLCurrency" = "UnitCostPLCurrency",
                           "lPLCurrencyID"         = "PLCurrencyID",
                           "sStrategy"             = "Strategy",
                           "lStrategyID"           = "StrategyID",
                           "lAge"                  = "Age",
                           "lAgeWorkingDays"       = "AgeWorkingDays",
                           "dblBeauchampTodayPL"   = "BeauchampTodayPL",
                           "dblBeauchampMTDPL"     = "BeauchampMTDPL",
                           "dblMarketValue"        = "MarketValue",
                           "dblSwing"              = "Swing",
                           "dblBeauchampMid"       = "BeauchampMid",
                           "lFunctionID"           = "FunctionID"
                           ),


    procedure    = "prPositionService_SelectHistoryBetweenForStrategy",
    results_parser = TE.SQLQuery:::convert_column_class
  ),
  contains  = c("VirtualSQLProcedureCall")
)


