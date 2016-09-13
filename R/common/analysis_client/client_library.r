sourceTo("../common/analysis_objectstore/analysis_objectstore.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/analysis_client/analysis_client.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/strategy_breakdown_aum_and_turnover/strategy_breakdown_aum_and_turnover.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/average_down_trades/average_down_trades.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/average_down_trades_focus/average_down_trades_focus.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/buys_and_sells/buys_and_sells.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/extended_trades/extended_trades.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/market_return/market_return.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/offside_positions/offside_positions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/offside_positions_cumulative_pnl/offside_positions_cumulative_pnl.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/pnl_traded_in_long_short_hedge/pnl_traded_in_long_short_hedge.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/portfolio_factor_exposure/portfolio_factor_exposure.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

#Convention is to name the client classes with the Client suffix so that the engine
#can specify analysis modules to load by class name.

setClass(
  Class                = "StrategyBreakdownAnalysisBlockClient",
  prototype = prototype(
    analysis_class = "StrategyBreakdownAnalysisBlock"
    
  ),
  contains = c("VirtualAnalysisClient")
)

setClass(
  Class 			   = "AverageDownTradesAnalysisBlockClient",
  prototype = prototype(
  	analysis_class = "AverageDownTradesAnalysisBlock"
  ),
  contains = c("VirtualAnalysisClient")
)

setClass(
  Class 			   = "AverageDownTradesFocusAnalysisBlockClient",
  prototype = prototype(
  	analysis_class = "AverageDownTradesFocusAnalysisBlock"
  ),
  contains = c("VirtualAnalysisClient")
)

setClass(
  Class 			   = "BuysAndSellsAnalysisBlockClient",
  prototype = prototype(
  	analysis_class = "BuysAndSellsAnalysisBlock"
  ),
  contains = c("VirtualAnalysisClient")
)

setClass(
  Class 			   = "ExtendedTradesAnalysisBlockClient",
  prototype = prototype(
  	analysis_class = "ExtendedTradesAnalysisBlock"
  ),
  contains = c("VirtualAnalysisClient")
)

setClass(
  Class 			   = "MarketReturnAnalysisBlockClient",
  prototype = prototype(
  	analysis_class = "MarketReturnAnalysisBlock"
  ),
  contains = c("VirtualAnalysisClient")
)

setClass(
  Class 			   = "OffsidePositionsAnalysisBlockClient",
  prototype = prototype(
  	analysis_class = "OffsidePositionsAnalysisBlock"
  ),
  contains = c("VirtualAnalysisClient")
)

setClass(
  Class          = "OffsidePositionsCumulativePnLAnalysisBlockClient",
  prototype = prototype(
    analysis_class = "OffsidePositionsCumulativePnLAnalysisBlock"
  ),
  contains = c("VirtualAnalysisClient")
)

setClass(
  Class          = "PnLTradedInLongShortHedgeAnalysisBlockClient",
  prototype = prototype(
    analysis_class = "PnLTradedInLongShortHedgeAnalysisBlock"
  ),
  contains = c("VirtualAnalysisClient")
)

setClass(
  Class          = "PortfolioFactorExposureAnalysisBlockClient",
  prototype = prototype(
    analysis_class = "PortfolioFactorExposureAnalysisBlock"
  ),
  contains = c("VirtualAnalysisClient")
)

setClass(
  Class          = "PositionRevisitsAnalysisBlockClient",
  prototype = prototype(
    analysis_class = "PositionRevisitsAnalysisBlock"
  ),
  contains = c("VirtualAnalysisClient")
)