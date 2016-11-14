#' @include referencedata_transformation.r
NULL

####################################
#
# VirtualTransformationsHandler Class
#
####################################

#' Virtual S4 class handing transformations
#'
#' implements handling of transformations
#'
#' @slot transformations          "list"
#' @slot attached_transformations "character"

setClass(
  Class = "VirtualTransformationsHandler",
  slots = c(
    attached_transformations = "character",
    transformations = "list"
  ),
  contains = c("VIRTUAL")
)

#' Get list of transformations that are stored to be attached to the Object
#'
#' Returns list of names of classes of transformations that
#' have been attached to object
#'
#' @param object object of class 'VirtualTransformationHandler'.
#' @return \code{transformations} list of "VirtualReferenceDataTransformation" objects attached.

setGeneric("getTransformations",function(object){standardGeneric("getTransformations")})
setMethod("getTransformations",
          signature(object = "VirtualTransformationsHandler"),
          function(object){
            return(object@transformations)
          }
)


#' Get list of attached transformations to the Object
#'
#' Returns list of names of classes of transformations that
#' have been attached to object
#'
#' @param object object of class 'VirtualTransformationHandler'.
#' @return \code{transformations} list of "VirtualReferenceDataTransformation" objects attached.

setGeneric("getAttachedTransformations",function(object){standardGeneric("getAttachedTransformations")})
setMethod("getAttachedTransformations",
          signature(object = "VirtualTransformationsHandler"),
          function(object){
            return(object@attached_transformations)
          }
)

#' Add transformation to the object
#'
#' Private method to add transformation to list of attached transformations
#'
#' @rdname private_addTransformation
#' @param object object of class 'VirtualTransformationHandler'.
#' @param transformation object of class "VirtualReferenceDataTransformation"
#' @return \code{object} object of class 'VirtualTransformationHandler'.

setGeneric(".addTransformation",function(object, transformation){standardGeneric(".addTransformation")})
setMethod(".addTransformation",
          signature(object = "VirtualTransformationsHandler",
                    transformation = "VirtualReferenceDataTransformation"),
          function(object, transformation){
            transf.name <- class(transformation)[[1]]
            transf.present <- getAttachedTransformations(object)
            object <- .setAttachedTransformations(object, union(transf.present, transf.name))
            return(object)
          }
)


#' Set transformation list
#'
#' Private method to set transformation list of attached transformations
#'
#' @rdname private_setTransformations
#' @param object object of class 'VirtualTransformationHandler'.
#' @param transformations list of objects of class "VirtualReferenceDataTransformation"
#' @return \code{object} object of class 'VirtualTransformationHandler'.

setGeneric(".setTransformations",function(object, transformations){standardGeneric(".setTransformations")})
setMethod(".setTransformations",
          signature(object = "VirtualTransformationsHandler", transformations = "list"),
          function(object, transformations){
            object@transformations <- transformations
            return(object)
          }
)


#' Set transformation list
#'
#' Private method to set transformation list of attached transformations
#'
#' @rdname private_setAttachedTransformations
#' @param object object of class 'VirtualTransformationHandler'.
#' @param transformations character vector of list of attached transformations
#' @return \code{object} object of class 'VirtualTransformationHandler'.

setGeneric(".setAttachedTransformations",function(object, transformations){standardGeneric(".setAttachedTransformations")})
setMethod(".setAttachedTransformations",
          signature(object = "VirtualTransformationsHandler", transformations = "character"),
          function(object, transformations){
            object@attached_transformations <- transformations
            return(object)
          }
)


#' Check if given transformation is attached
#'
#' Returns logical value indicating if given transformation
#' is attached to the object
#'
#' @param object object of class 'VirtualTransformationHandler'.
#' @param transformation object of class "VirtualReferenceDataTransformation"
#' @return \code{is_attached} logical.
#' @export

setGeneric("isTransformationAttached", function(object, transformation){standardGeneric("isTransformationAttached")})

#' @describeIn isTransformationAttached
#' Check if given transformation is attached
#'
#' Returns logical value indicating if given transformation
#' is attached to the object
#'
#' @inheritParams VirtualTransformationsHandler
#' @return \code{is_attached} logical.
#' @export
setMethod("isTransformationAttached",
          signature(object = "VirtualTransformationsHandler", transformation = "VirtualReferenceDataTransformation"),
          function(object, transformation){
            nme <- class(transformation)[[1]]
            if(nme %in% getAttachedTransformations(object)){
              return(TRUE)
            }
            else{
              return(FALSE)
            }
          }
)


#' Attach transformation to the object
#'
#' Attaches new columns with new transformations
#' (data computed from existing VirtualTransformationHandler data)
#'
#' @param object object of class 'VirtualTransformationHandler'.
#' @param transformation objects of clas "VirtualReferenceDataTransformation"
#' @param replace "logical" replace transformation if already attached.
#' @return \code{object} object of class 'VirtualTransformationHandler'.
#' @export

setGeneric("attachTransformation",function(object,transformation, replace = TRUE){standardGeneric("attachTransformation")})

#' @describeIn attachTransformation
#' Attach transformation to the object
#'
#' Attaches new columns with new transformations
#' (data computed from existing VirtualTransformationHandler data)
#'
#' @inheritParams attachTransformation
#' @return \code{object} object of class 'VirtualTransformationHandler'.
#' @export
setMethod("attachTransformation",
          signature(object = "VirtualTransformationsHandler", transformation = "VirtualReferenceDataTransformation"),
          function(object,transformation,replace=TRUE){

            if (isTransformationAttached(object, transformation) || !replace) {
              return(object)
            } else {

              transformation <- tryCatch({
                setReferenceDataObject(transformation, object)
              }, error = function(cond){
                message(paste("Failed to attach", class(transformation)[[1]], "to", class(object)[[1]]))
                message(paste("In call to setVirtualTransformationHandler()"))
                stop(paste("Failed to attach", class(transformation)[[1]], "to", class(object)[[1]]))
              }
              )

              transformation <- tryCatch({
                triggerComputation(transformation)
              }, error = function(cond){
                message(paste("Failed to attach", class(transformation)[[1]], "to", class(object)[[1]]))
                message(paste("In call to triggerComputation()"))
                stop(paste("Failed to attach", class(transformation)[[1]], "to", class(object)[[1]]))
              }
              )

              if (isComputed(transformation)){
                object <- appendVariables(object,
                                          getComputationOutput(transformation),
                                          getComputedVariablesNames(transformation))

                object <- .addTransformation(object,transformation)
              } else {
                message(paste("Failed to attach", class(transformation)[[1]], "to", class(object)[[1]]))
                message(paste("Computation has not returned isComputed() status equal to TRUE."))
                stop(paste("Failed to attach", class(transformation)[[1]], "to", class(object)[[1]]))
              }

              return(object)

            }
          }
)



#' Attach transformations to the object
#'
#' Attaches new columns with new transformations
#' (data computed from existing VirtualTransformationHandler data)
#'
#' @param object object of class 'VirtualTransformationHandler'.
#' @param transformations list of objects of class "VirtualReferenceDataTransformation"
#' @param replace "logical" replace transformation(s) if already attached.
#' @return \code{object} object of class 'VirtualTransformationHandler'.
#' @export

setGeneric("attachTransformations",function(object,transformations, replace = TRUE){standardGeneric("attachTransformations")})

#' @describeIn attachTransformations
#' Attach transformations to the object
#'
#' Attaches new columns with new transformations
#' (data computed from existing VirtualTransformationHandler data)
#'
#' @inheritParams VirtualTransformationsHandler
#' @return \code{object} object of class 'VirtualTransformationHandler'.
#' @export
setMethod("attachTransformations",
          signature(object = "VirtualTransformationsHandler", transformations = "list"),
          function(object,transformations,replace=TRUE){
            for(transf in transformations){
              object <- attachTransformation(object,transf, replace)
            }
            return(object)
          }
)




