sourceTo("../common/global_configs.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

setClassUnion("NullableSocket",c("NULL","socket"))
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

setGeneric("getDataFromSocket",function(object){standardGeneric("getDataFromSocket")})
setMethod("getDataFromSocket","ProcessSocket",
          function(object){
            return(object@data)           
          }
)

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
