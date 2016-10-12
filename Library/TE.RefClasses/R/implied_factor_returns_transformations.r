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
