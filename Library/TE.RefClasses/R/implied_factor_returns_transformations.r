#' @include referencedata_transformation.r
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
  contains = c("VirtualReferenceDataTransformation")
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
#' Inherits from "VirtualReferenceDataTransformation"
#' uses "ImpliedFactorReturnsMAVGTransformationComputation"
#' @export

setClass(
  Class          = "ImpliedFactorReturnsMAVGTransformation",
  prototype = prototype(
    required_colnms = c('Date'),
    #computed_colnms = c(),
    computation = new("ImpliedFactorReturnsMAVGTransformationComputation")
  ),
  contains = c("VirtualReferenceDataTransformation")
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
#' Inherits from "VirtualReferenceDataTransformation"
#' uses "ImpliedFactorReturnsMAVGSpreadTransformationComputation"
#' @export

setClass(
  Class          = "ImpliedFactorReturnsMAVGSpreadTransformation",
  prototype = prototype(
    required_colnms = c('Date'),
    #computed_colnms = c(),
    computation = new("ImpliedFactorReturnsMAVGSpreadTransformationComputation")
  ),
  contains = c("VirtualReferenceDataTransformation")
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
#' Inherits from "VirtualReferenceDataTransformation"
#' uses "ImpliedFactorReturnsQuartileTransformationComputation"
#' @export

setClass(
  Class          = "ImpliedFactorReturnsQuartileTransformation",
  prototype = prototype(
    required_colnms = c('Date'),
    #computed_colnms = c(),
    computation = new("ImpliedFactorReturnsQuartileTransformationComputation")
  ),
  contains = c("VirtualReferenceDataTransformation")
)

################################################
#
# ImpliedFactorReturnsTrendIndicatorTransformationComputation Class
#
################################################

#' computes trend indicator quantifier
#'
#' Adds columns holding trend indication category for each factor
#'
#' Inherits from "VirtualTransformationComputation"

setClass(
  Class = "ImpliedFactorReturnsTrendIndicatorTransformationComputation",
  prototype      = list(
    required_colnms= c('Date'),
    #computed_colnms = c(),
    compute = rsquared_trend_ind
  ),
  contains = c( "VirtualTransformationComputation")
)

#' computes return quantile
#'
#' Adds columns for the quantile of compounded return
#' for each factor
#'
#' Inherits from "VirtualReferenceDataTransformation"
#' uses "ImpliedFactorReturnsTrendIndicatorTransformationComputation"
#' @export

setClass(
  Class          = "ImpliedFactorReturnsTrendIndicatorTransformation",
  prototype = prototype(
    required_colnms = c('Date'),
    #computed_colnms = c(),
    computation = new("ImpliedFactorReturnsTrendIndicatorTransformationComputation")
  ),
  contains = c("VirtualReferenceDataTransformation")
)


################################################
#
# ImpliedFactorReturnsStateEncoderTransformationComputation Class
#
################################################

#' computes factor state based on substates space
#'
#' Adds columns holding factor state
#'
#' Inherits from "VirtualTransformationComputation"

setClass(
  Class = "ImpliedFactorReturnsStateEncoderTransformationComputation",
  prototype      = list(
    required_colnms= c('Date'),
    #computed_colnms = c(),
    compute = factor_space_encoder
  ),
  contains = c( "VirtualTransformationComputation")
)

#' computes factor state based on substates space
#'
#' Adds columns holding factor state
#'
#' Inherits from "VirtualReferenceDataTransformation"
#' uses "ImpliedFactorReturnsStateEncoderTransformationComputation"
#' @export

setClass(
  Class          = "ImpliedFactorReturnsStateEncoderTransformation",
  prototype = prototype(
    required_colnms = c('Date'),
    #computed_colnms = c(),
    computation = new("ImpliedFactorReturnsStateEncoderTransformationComputation")
  ),
  contains = c("VirtualReferenceDataTransformation")
)



