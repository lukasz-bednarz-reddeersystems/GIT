#' @include key_expander.r
NULL

####################################
#
# VirtualTradeIDKeyExpander Class
#
####################################

#' Virtual S4 class that expands a key frame over
#' available values of TradeID
#'
#'
#' @slot available_values object of class "AvailableTraders"

setClass(
  Class    = "VirtualTradeIDKeyExpander",
  slots = c(plot_modifier="function"),
  prototype = list(available_values=new("AvailableTraders"),
                   plot_modifier=function(ggplot)ggplot),
  contains = c("VirtualKeyExpander","VIRTUAL")
)

#' Modify the ggplot if desired
#' @param object object of class "VirtualTradeIDKeyExpander"
#' @param ggplot object of class "gg"
#' @return \code{ggplot} object of class "gg"
#' @export
setGeneric("expandGGPlot", function(object,ggplot){standardGeneric("expandGGPlot")})

#' @describeIn expandGGPlot
#'
#' Modify the ggplot if desired
#'
#' @inheritParams expandGGPlot
#' @return \code{ggplot} object of class "gg"
#' @export
setMethod("expandGGPlot",
          signature(object = "VirtualTradeIDKeyExpander", ggplot = "gg"),
          function(object,ggplot){
            new_plot <- tryCatch({
                                object@plot_modifier(ggplot)
                              }, error = function(cond){
                                message(paste("Error when trying to expandGGPlot:",cond))
                                return(ggplot)
                              })
            return(new_plot)
          }
)
