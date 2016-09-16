#' @include risk_model_handler.r
NULL

####################################
#
# RiskModelBuilder Class
#
####################################

#' Virtual S4 Class implementing methods to handle building of
#' Risk Models historical data
#'
#' Handles building up historical model data and updating
#'
#' Inherits from VirtualRiskModelHandler

setClass(
  Class     = "VirtualRiskModelBuilder",
  contains  = c("VirtualRiskModelHandler", "VIRTUAL")
)


#' Concrete S4 class implementing methods to handle building of
#' Risk Models historical data
#'
#' Handles building up historical model data and updating
#'
#' Inherits from VirtualRiskModelBuilder
#'
#' @export

setClass(
  Class     = "RiskModelBuilder",
  prototype = list(
    risk_model      = new("RiskModel.DevelopedEuropePrototype150")
  ),
  contains  = c("VirtualRiskModelBuilder")
)


#' Update risk model on specific date
#'
#' updates risk model up to specified date for month of the date.
#'
#' @param object object of class 'VirtualRiskModelBuilder'.
#' @param date "Date" date of the model for which the entries should be computed
#' @param force "logical" should the values be replaced if already existing
#' @param copy_history "logical" should we look for some components history if computed earlier
#'
#' @export

setGeneric("updateRiskModelOnDate", function(object,
                                              date,
                                              force = FALSE,
                                              copy_history = FALSE){standardGeneric("updateRiskModelOnDate")})

#' @describeIn updateRiskModelOnDate
#' Update risk model on specific date
#'
#' updates risk model up to specified date for month of the date.
#'
#' @inheritParams updateRiskModelOnDate
#' @return \code{status} 'logical', TRUE if model was updated, FALSE otherwise
#' @export
setMethod("updateRiskModelOnDate",
          signature(object = "VirtualRiskModelBuilder",
                    date   = "Date",
                    force  = "logical",
                    copy_history = "logical"),
          function(object, date = today() -1, force, copy_history){

            risk_model <- getRiskModelObject(object)


            ret <- update_risk_model_on_date(risk_model, date, force, copy_history)

            return(object)
          }
)

#' Build risk model data for specific date range
#'
#' Builds risk model data for specific risk model and specific date range
#'
#' @param object object of class 'VirtualRiskModelBuilder'.
#' @param date_start "Date" start date of computation
#' @param date_end "Date" end date of the model
#'
#' @export

setGeneric("buildRiskModelForDateRange", function(object,
                                                  date_start,
                                                  date_end){standardGeneric("buildRiskModelForDateRange")})

#' @describeIn buildRiskModelForDateRange
#' Build risk model data for specific date range
#'
#' Builds risk model data for specific risk model and specific date range
#'
#' @inheritParams buildRiskModelForDateRange
#' @return \code{status} 'logical', TRUE if model was updated, FALSE otherwise
#' @export
setMethod("buildRiskModelForDateRange",
          signature(object       = "VirtualRiskModelBuilder",
                    date_start   = "Date",
                    date_end     = "Date"),
          function(object, date_start, date_end){

            risk_model <- getRiskModelObject(object)


            dates <-  seq(from= date_start, to = date_end, by = '1 month')
            dates <-  c(dates, seq(from= date_start, to = date_end, by = '1 month') %m+% months(1) %m-% days(1))
            dates <- sort(dates)
            copy_history <-  FALSE
            force <-  TRUE
            first  <-  TRUE

            for (date in dates) {

              update_risk_model_on_date(risk_model, as_date(date), force, copy_history)
              force <- !force
              if (first) {
                first <- FALSE
              } else {
                copy_history <- !copy_history
              }

            }
            return(object)
          }
)
