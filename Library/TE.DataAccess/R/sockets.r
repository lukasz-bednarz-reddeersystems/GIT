#' @include global_configs.r

setClassUnion("NullableSocket",c("NULL","socket"))

#' An S4 class handling connections to R process socket.
#'
#' @slot remote_host "character"
#' @slot port        "numeric"
#' @slot server      "logical"
#' @slot socket      "NullableSocket"
#' @slot data        "character"
#' @slot read_limit  "numeric"
#' @export

setClass(
  Class          = "ProcessSocket",
  representation = representation(
    remote_host  = "character",
    port         = "numeric",
    server       = "logical",
    socket       = "NullableSocket",
    data         = "character",
    read_limit   = "numeric"
  ),
  prototype(
    remote_host  = socket_defaults@remote_host,
    port         = socket_defaults@port,
    server       = socket_defaults@server,
    read_limit   = socket_defaults@read_lim
  )
)

#' open socket connection
#'
#' @param object object of class 'ProcessSocket'.
#' @return \code{object} object of class 'ProcessSocket'.
#' @examples
#' openConnection(object)

setGeneric("openConnection",function(object){standardGeneric("openConnection")})
setMethod("openConnection","ProcessSocket",
          function(object){
            if(object@server){
              message("Waiting for connections ...")
              object@socket <- tryCatch({
                make.socket(port = object@port,server = object@server)
              }, error = function(cond){
                message(paste("Error when opening server-side socket:",cond))
                stop()
              })
              message("Client connected.")
            }
            else{
              message("Opening client side socket ...")
              object@socket <-tryCatch({
                make.socket(port = object@port,host = object@remote_host,server = object@server)
              }, error = function(cond){
                message(paste("Error when opening client-side socket: ",cond))
                stop()
              })
              message("Socket ready.")
            }
           return(object)
          }
)

#' Read data from socket
#'
#' @param object object of class 'ProcessSocket'.
#' @return \code{object} object of class 'ProcessSocket'.
#' @examples
#' readConnection(object)
#' @export

setGeneric("readConnection",function(object){standardGeneric("readConnection")})
setMethod("readConnection","ProcessSocket",
          function(object){
            if(object@server){
              message("Server reading socket ...")
            }
            else{
              message("Client reading socket ...")
            }
            object@data <- tryCatch({
              read.socket(object@socket,maxlen=object@read_limit)
            }, error = function(cond){
              message(paste("Error reading socket:",cond))
            })
            return(object)
          }
)

#' Write data to socket
#'
#' @param object object of class 'ProcessSocket'.
#' @param data character, data to write to socket.
#' @return \code{object} object of class 'ProcessSocket'.
#' @examples
#' writeToConnection(object,data)
#' @export

setGeneric("writeToConnection",function(object,data){standardGeneric("writeToConnection")})
setMethod("writeToConnection","ProcessSocket",
          function(object,data){
            object@data <- data
            if(nchar(data)>object@read_limit)stop("Size of the data has exceed the socket readlimit.")
            if(object@server){
              message("Server writing to socket ...")
            }
            else{
              message("Client reading from socket ...")
            }
            tryCatch({
              write.socket(object@socket,data)
            },error = function(cond){
              message(paste("Error when writing to socket:",cond))
            })
            return(object)
          }
)

if (!R.methodsS3::isGenericS4("getData")) {
  setGeneric("getData",function(object){standardGeneric("getData")})
}

#' Get data read from socket
#'
#' Retrieve data previously read from socket using
#' \code{readConnection(object)}
#'
#' @param object object of class 'ProcessSocket'.
#' @return \code{data} character, data read from socket..
#' @examples
#' getData(object)
#' @export

setMethod("getData","ProcessSocket",
          function(object){
            return(object@data)
          }
)

#' Close socket connection
#'
#' @param object object of class 'ProcessSocket'.
#' @return \code{object} object of class 'ProcessSocket'.
#' @examples
#' closeConnection(object)
#' @export

setGeneric("closeConnection",function(object){standardGeneric("closeConnection")})
setMethod("closeConnection","ProcessSocket",
          function(object){
            if(object@server){
              message("Server closing socket ...")
            }
            else{
              message("Client closing socket ...")
            }
            object@socket <- tryCatch({
              close.socket(object@socket)
              NULL
            },error = function(cond){
              message(paste("Error when closing socket:",cond))
            })
            return(object)
          }
)
