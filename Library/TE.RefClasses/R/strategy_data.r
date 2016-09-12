####################################
#
# VirtualStrategyData Class
#
####################################

#' Virtual S4 class storing strategy related data
#' @export

setClass(
  Class               = "VirtualStrategyData",
  prototype           = list(
    required_colnms   = c("Strategy")
  ),

  contains = c("VirtualReferenceData", "VIRTUAL")
)


