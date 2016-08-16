####################################
#
# VirtualStrategyData Class
#
####################################

#' @export

setClass(
  Class               = "VirtualStrategyData",
  prototype           = list(
    required_colnms   = c("Strategy")
  ),

  contains = c("VirtualReferenceData", "VIRTUAL")
)


