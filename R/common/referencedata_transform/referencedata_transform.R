sourceTo("../lib/referencedata/referencedata.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../common/datastore_client/datastore_client.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

####################################
#
# VirtualTransform Class
#
####################################

setClass(
  Class      = "VirtualBaseTransformation",
  slots = c(
    computed_colnms = "character",
    is_computed = "logical"
  ),
  prototype = list(
    is_computed = FALSE
  ),
  contains = c("VirtualReferenceObject", "VIRTUAL")
)