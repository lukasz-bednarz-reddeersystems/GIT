#' @include virtual_transformation.r
NULL

################################################
#
# CompoundImpliedFactorReturnsTransformationComputation Class
#
################################################

#' computes compounded factor return
#'
#' Adds cumulant columns.
#'
#' Inherits from "VirtualTransformationComputation"

setClass(
  Class = "CompoundImpliedFactorReturnsTransformationComputation",
  prototype      = list(
    required_colnms= c('Date'),
    #computed_colnms = c(),
    compute = tseries_excess_compound
  ),
  contains = c( "VirtualTransformationComputation")
)

#' computes compound implied factor return
#'
#' Adds columns for the compound return for each
#' factor.
#'
#' Inherits from "VirtualTransformation"
#' uses "CompoundImpliedFactorReturnsTransformationComputation"
#' @export

setClass(
  Class          = "CompoundImpliedFactorReturnsTransformation",
  prototype = prototype(
    required_colnms = c('Date'),
    #computed_colnms = c(),
    computation = new("CompoundImpliedFactorReturnsTransformationComputation")
  ),
  contains = c("VirtualTransformation")
)

################################################
#
# ImpliedFactorReturnsMAVGTransformationComputation Class
#
################################################

#' computes moving averages of the compounded factor return
#'
#' Adds columns holding the MAVG values.
#'
#' Inherits from "VirtualTransformationComputation"

setClass(
  Class = "ImpliedFactorReturnsMAVGTransformationComputation",
  prototype      = list(
    required_colnms= c('Date'),
    #computed_colnms = c(),
    compute = tseries_mavgs
  ),
  contains = c( "VirtualTransformationComputation")
)

#' computes compound implied factor return
#'
#' Adds columns for the compound return for each
#' factor.
#'
#' Inherits from "VirtualTransformation"
#' uses "CompoundImpliedFactorReturnsTransformationComputation"
#' @export

setClass(
  Class          = "ImpliedFactorReturnsMAVGTransformation",
  prototype = prototype(
    required_colnms = c('Date'),
    #computed_colnms = c(),
    computation = new("ImpliedFactorReturnsMAVGTransformationComputation")
  ),
  contains = c("VirtualTransformation")
)

################################################
#
# ImpliedFactorReturnsMAVGSpreadTransformationComputation Class
#
################################################

#' computes spread between moving averages of the compounded factor return
#'
#' Adds columns holding the spread and the diff of the spread
#'
#' Inherits from "VirtualTransformationComputation"

setClass(
  Class = "ImpliedFactorReturnsMAVGSpreadTransformationComputation",
  prototype      = list(
    required_colnms= c('Date'),
    #computed_colnms = c(),
    compute = tseries_mavgs_spread
  ),
  contains = c( "VirtualTransformationComputation")
)

#' computes return mavg spread
#'
#' Adds columns for the spread and spread diff
#' for each factor
#'
#' Inherits from "VirtualTransformation"
#' uses "CompoundImpliedFactorReturnsMAVGSpreadTransformationComputation"
#' @export

setClass(
  Class          = "ImpliedFactorReturnsMAVGSpreadTransformation",
  prototype = prototype(
    required_colnms = c('Date'),
    #computed_colnms = c(),
    computation = new("ImpliedFactorReturnsMAVGSpreadTransformationComputation")
  ),
  contains = c("VirtualTransformation")
)

################################################
#
# ImpliedFactorReturnsQuartileTransformationComputation Class
#
################################################

#' computes quartile of the compound factor return level
#'
#' Adds columns holding the quartile for each day
#'
#' Inherits from "VirtualTransformationComputation"

setClass(
  Class = "ImpliedFactorReturnsQuartileTransformationComputation",
  prototype      = list(
    required_colnms= c('Date'),
    #computed_colnms = c(),
    compute = ftiler
  ),
  contains = c( "VirtualTransformationComputation")
)

#' computes return quantile
#'
#' Adds columns for the quantile of compounded return
#' for each factor
#'
#' Inherits from "VirtualTransformation"
#' uses "CompoundImpliedFactorReturnsQuartileTransformationComputation"
#' @export

setClass(
  Class          = "ImpliedFactorReturnsQuartileTransformation",
  prototype = prototype(
    required_colnms = c('Date'),
    #computed_colnms = c(),
    computation = new("ImpliedFactorReturnsQuartileTransformationComputation")
  ),
  contains = c("VirtualTransformation")
)


