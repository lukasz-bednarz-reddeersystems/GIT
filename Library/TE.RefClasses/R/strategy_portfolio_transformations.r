#' @include referencedata_transformation.r
#' @include strategy_portfolio.r
NULL

################################################
#
# DaysSinceLastFlatTransformationn Class
#
################################################

#' computes number of days since last flat
#'
#' Adds column with number of days since last flat
#' for each position.
#' Inherits from "VirtualTransformationComputation"

setClass(
  Class = "DaysSinceLastFlatTransformationComputation",
  prototype      = list(
    required_colnms= c('Date','InstrumentID','Weight'),
    computed_colnms = c("DaysSinceLastFlat"),
    compute = days_since_last_flat

  ),
  contains = c( "VirtualTransformationComputation")
)

#' computes number of days since last flat
#'
#' Adds column with number of days since last flat
#' for each position
#' Inherits from "VirtualReferenceDataTransformation"
#' uses "DaysSinceLastFlatTransformationComputation"
#' @export
setClass(
  Class          = "DaysSinceLastFlatTransformation",
  slots = c(
    ref_data     = 'StrategyPortfolio'
  ),
  prototype = prototype(
    required_colnms = c('Date','InstrumentID','Weight'),
    computed_colnms = c("DaysSinceLastFlat"),
    computation = new("DaysSinceLastFlatTransformationComputation")
  ),
  contains = c("VirtualReferenceDataTransformation")
)
