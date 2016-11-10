#' @include implied_factor_returns_data.r
#' @include transformations_handler.r
#' @include implied_factor_returns_transformations.r
NULL


####################################
#
# ImpliedFactorReturnsState Class
#
####################################

#' S4 class implementing encoding of
#' factor return state in terms of the
#' quartile of the compound return level
#' and whether the factor is sideways trending
#' or reverting.
#'
#' This is concrete class handling  computation
#' of the factor return state.
#' Inherits from:
#'  "ImpliedFactorReturnsData"
#'  "VirtualTransformationsHandler"
#'
#' @export

setClass(
  Class          = "ImpliedFactorReturnsState",
  prototype      = list(
    transformations = list(new("CompoundImpliedFactorReturnsTransformation", NULL),
                           new("ImpliedFactorReturnsMAVGTransformation", NULL),
                           new("ImpliedFactorReturnsMAVGSpreadTransformation", NULL),
                           new("ImpliedFactorReturnsTrendIndicatorTransformation", NULL),
                           new("ImpliedFactorReturnsQuartileTransformation", NULL),
                           new("ImpliedFactorReturnsStateEncoderTransformation", NULL))
  ),
  contains = c("ImpliedFactorReturnsData",
               "VirtualTransformationsHandler")
)

#' Apply transformations in sequence in order to compute the
#' factor return state.
#'
#' @param object object of class 'ImpliedFactorReturnsState'.
#' @return \code{object} object of class 'ImpliedFactorReturnsState'.
#' @export

setGeneric("computeImpliedFactorReturnsState", function(object){standardGeneric("computeImpliedFactorReturnsState")})

#' @describeIn computeImpliedFactorReturnsState
#' Compute factor return state
#'
#' Applies transformations in the transformation handler
#'
#' @inheritParams computeImpliedFactorReturnsState
#' @return \code{object} object of class 'ImpliedFactorReturnsState'.
#' @export
setMethod("computeImpliedFactorReturnsState",
          signature(object = "ImpliedFactorReturnsState"),
          function(object){

            transformations <- getTransformations(object)
            data <- getReferenceData(object)
            all_data <- data
            orig_cols <- colnames(data)
            compd_cols <- c()
            for(transformation in transformations[1:3]){
              transformation <- setComputationInput(transformation,data[setdiff(colnames(data),compd_cols)])
              transformation <- triggerComputation(transformation)
              compd_cols <- setdiff(colnames(data),"Date")
              data <- getComputationOutput(transformation)
              all_data <- merge(all_data,data[setdiff(colnames(data),compd_cols)],by=c("Date"))
            }
            transformation <- transformations[[4]]
            df <- all_data[c('Date',paste(setdiff(orig_cols,'Date'),'_cmpnd',sep=""))]
            transformation <- setComputationInput(transformation,df)
            transformation <- triggerComputation(transformation)
            data <- getComputationOutput(transformation)
            all_data <- merge(all_data,data[c("Date",setdiff(colnames(data),colnames(all_data)))],by=c("Date"))


            transformation <- transformations[[5]]
            df <- all_data[c('Date',paste(setdiff(orig_cols,'Date'),'_cmpnd',sep=""))]
            names(df) <- orig_cols
            transformation <- setComputationInput(transformation,df)
            transformation <- triggerComputation(transformation)
            data <- getComputationOutput(transformation)
            all_data <- merge(all_data,data[c("Date",setdiff(colnames(data),orig_cols))],by=c("Date"))

            browser()
            transformation <- transformations[[6]]
            df <- all_data[c('Date',
                             paste(setdiff(orig_cols,'Date'),'_ti',sep=""),
                             paste(setdiff(orig_cols,'Date'),'_ftile',sep=""))]
            transformation <- setComputationInput(transformation,df)
            transformation <- triggerComputation(transformation)
            data <- getComputationOutput(transformation)
            all_data <- merge(all_data,data[c("Date",setdiff(colnames(data),orig_cols))],by=c("Date"))

            object <- setReferenceData(object,all_data)

            return(object)
          }
)






