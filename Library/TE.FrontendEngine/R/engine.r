#' @include client_library.r
#' @include shiny_app_factories.r
NULL


.__R_DEFAULT_TE_APP_ROOT__ <- gsub("\\\\",
                                   "/",
                                   paste0(Sys.getenv("R_TE_DEVEL_ROOT", "C:/Development/TradingEnhancementEngine"),
                                          "/R/te_module_builder")
                                   )

#' Class inmplementing Frontend Engine Command Interpreter
#'
#' Implements Server opening port and accepting commands
#' from client via socket
#'
#' @slot snd_respnse "logical"
#' @slot snd_data "logical"
#' @slot run_app "logical"
#' @slot callback "function"
#' @slot split_char "character"
#' @slot cmmd_lst "character"

setClass(
	Class          = "EngineCommandInterpreter",
	representation = representation(
		snd_respnse= "logical",
		snd_data   = "logical",
		run_app    = "logical",
		callback   = "function",
		split_char = "character",
		cmmd_lst   = "character"
	),
	prototype      = prototype(
		split_char = "|"
	)
)

setGeneric("setEngineSlotCallback",function(object,slot_name,value_getter,snd_response=TRUE,snd_data=FALSE,run_app=FALSE){standardGeneric("setEngineSlotCallback")})
setMethod("setEngineSlotCallback","EngineCommandInterpreter",
		  function(object,slot_name,value_getter,snd_response=TRUE,snd_data=FALSE,run_app=FALSE){
		  	object@callback     <- function(engine,get_value=value_getter){slot(engine,slot_name) <- get_value(engine)
		  						                                           return(engine)}
		  	object@snd_respnse  <- snd_response
		  	object@snd_data     <- snd_data
		  	object@run_app      <- run_app
		  	return(object)
		  }
)

setGeneric("testCallback",function(object){standardGeneric("testCallback")})
setMethod("testCallback","EngineCommandInterpreter",
		  function(object){
		  	object <- setEngineSlotCallback(object,'response',function(engine)paste(unlist(version),collapse=', '))
		  	return(object)
		  }
)

setGeneric("setLookbackCallback",function(object){standardGeneric("setLookbackCallback")})
setMethod("setLookbackCallback","EngineCommandInterpreter",
	      function(object){
	      	object <- setEngineSlotCallback(object,'lookback',function(engine)engine@interpreter@cmmd_lst[2])
	      	return(object)
	      }
)

setGeneric("setModuleCallback",function(object){standardGeneric("setModuleCallback")})
setMethod("setModuleCallback","EngineCommandInterpreter",
	      function(object){
	      	object <- setEngineSlotCallback(object,'module_name',function(engine)engine@interpreter@cmmd_lst[2])
	      	return(object)
	      }
)

setGeneric("setAppCallback",function(object){standardGeneric("setAppCallback")})
setMethod("setAppCallback","EngineCommandInterpreter",
          function(object){
            object <- setEngineSlotCallback(object,'app_path',function(engine)getAppPath(engine@app_store,engine@interpreter@cmmd_lst[2]))
            return(object)
          }
)

setGeneric("setAppPortCallback",function(object){standardGeneric("setAppPortCallback")})
setMethod("setAppPortCallback","EngineCommandInterpreter",
          function(object){
            object <- setEngineSlotCallback(object,'app_port',function(engine)engine@interpreter@cmmd_lst[2])
            return(object)
          }
)

setGeneric("setAppHostCallback",function(object){standardGeneric("setAppHostCallback")})
setMethod("setAppHostCallback","EngineCommandInterpreter",
          function(object){
            object <- setEngineSlotCallback(object,'app_host',function(engine)ifelse(engine@interpreter@cmmd_lst[2]=='localhost','127.0.0.1',engine@interpreter@cmmd_lst[2]))
            return(object)
          }
)

setGeneric("setTraderCallback",function(object){standardGeneric("setTraderCallback")})
setMethod("setTraderCallback","EngineCommandInterpreter",
	      function(object){
	      	object <- setEngineSlotCallback(object,'trader',function(engine){ifelse(grepl("^[0-9]+$", engine@interpreter@cmmd_lst[2]),
	      	                                                                       as.numeric(engine@interpreter@cmmd_lst[2]),
	      	                                                                       as.character(engine@interpreter@cmmd_lst[2]))})
	      	return(object)
	      }
)

setGeneric("setDateCallback",function(object){standardGeneric("setDateCallback")})
setMethod("setDateCallback","EngineCommandInterpreter",
	      function(object){
	      	object <- setEngineSlotCallback(object,'module_date',function(engine)as.character(engine@interpreter@cmmd_lst[2]))
	      	return(object)
	      }
)

setGeneric("setShutdownCallback",function(object){standardGeneric("setShutdownCallback")})
setMethod("setShutdownCallback","EngineCommandInterpreter",
	      function(object){
	      	object@callback <- function(engine)stop("Engine shutdown requested by client.")
	      	return(object)
	      }
)

setGeneric("getModuleDataCallback",function(object){standardGeneric("getModuleDataCallback")})
setMethod("getModuleDataCallback","EngineCommandInterpreter",
		  function(object){
			fn <- function(engine){list(queryAnalysisStore(engine@analys_str,data.frame(key_hash=engine@key_hash,analysis_module=engine@module_name)))}
		  	object <- setEngineSlotCallback(object,'module',fn,TRUE,TRUE)
		  	return(object)
		  }
)

setGeneric("runAppCallback",function(object){standardGeneric("runAppCallback")})
setMethod("runAppCallback","EngineCommandInterpreter",
      function(object){
        fn <- function(engine){function(engine){
        										setwd(engine@app_path)
                            library(TE.FrontendEngine)
                            analysis_ggplot <- engine@analysis_ggplot
        										analysis_data <- engine@analysis_data
        										ui_options <- engine@ui_options
        										factory <- new("ShinyFactory")
												factory <- tryCatch({shinyUIFactory(factory,analysis_ggplot,analysis_data, ui_options)},error=function(cond)stop(paste("UI factory failed:",cond)))
												factory <- tryCatch({shinyServerFactory(factory,analysis_ggplot,analysis_data)},error=function(cond)stop(paste("Server factory failed:",cond)))
        										  shiny::runApp(list(ui = getUI(factory), server = getServer(factory)),host=engine@app_host,port=as.numeric(engine@app_port),launch.browser=FALSE)}
        										}
        object <- setEngineSlotCallback(object,'app',fn,TRUE,FALSE,TRUE)
        return(object)
      }
)

setGeneric("parseCommandString",function(object,string){standardGeneric("parseCommandString")})
setMethod("parseCommandString","EngineCommandInterpreter",
		  function(object,string){
		  	if(nchar(string)==0)stop("Request string was empty.")
		  	splt_lst <- unlist(strsplit(string,paste("[",object@split_char,"]",sep="")))
		  	if(length(splt_lst)==0)
		  		object@cmmd_lst <- c(string)
		  	else{
		  		object@cmmd_lst <- splt_lst
		  	}
		  	return(object)
		  }
)

setGeneric("interpretCommand",function(object,cmmnd){standardGeneric("interpretCommand")})
setMethod("interpretCommand","EngineCommandInterpreter",
		  function(object,cmmnd){
		  	object <- parseCommandString(object,cmmnd)
		  	icmmnd <- switch(object@cmmd_lst[1],
		  						TEST = testCallback(object),
		  						SETLOOKBACK  = setLookbackCallback(object),
		  						SETMODULE  = setModuleCallback(object),
		  						SETAPP = setAppCallback(object),
		  						SETAPPPORT = setAppPortCallback(object),
		  						SETAPPHOST = setAppHostCallback(object),
		  						SETTRADER  = setTraderCallback(object),
		  						SETDATE = setDateCallback(object),
		  						GETANALYSISDATA = getModuleDataCallback(object),
		  						RUNAPP = runAppCallback(object),
		  						STOP = setShutdownCallback(object))
		  	if(length(icmmnd)==0){
		  		message("Interpreter could not parse request.")
		  		object <- setEngineSlotCallback(object,'response',function(engine)"ERROR",TRUE,FALSE)
		  	}
		  	else{
		  		object <- icmmnd
		  	}
		  	return(object)
		  }
)

setClass(
  Class          = "BasicAppClient",
  representation = representation(
    app_store    = "list"
  ),
  prototype      = prototype(
    app_store    = list(builder=.__R_DEFAULT_TE_APP_ROOT__)
  )
)

setGeneric("getAppPath",function(object,app_name){standardGeneric("getAppPath")})
setMethod("getAppPath","BasicAppClient",
      function(object,app_name){
        if(length(app_name)==0)stop("App client called on null app name.")
        rval <- object@app_store[[app_name]]
        if(length(rval)==0)stop(paste(app_name," module name not found."))
        return(rval)
      }
)



setClassUnion("NumericOrCharacter", c("numeric", "integer", "character"))


#' Class inmplementing Frontend Engine Server
#'
#' Implements Server opening port and accepting commands
#' from client via socket
#'
#' @slot interpreter "EngineCommandInterpreter",
#' @slot socket      "ProcessSocket",
#' @slot store_name  "character",
#' @slot analys_str  "AnalysisObjectStore",
#' @slot app_store   "BasicAppClient",
#' @slot trader      "NumericOrCharacter",
#' @slot module_date "character",
#' @slot lookback    "character",
#' @slot key_hash    "character",
#' @slot module_name "character",
#' @slot app_path    "character",
#' @slot app_port    "character",
#' @slot app_host    "character",
#' @slot response    "character",
#' @slot module      "list",
#' @slot app         "function",
#' @slot analysis_ggplot  "ANY",
#' @slot analysis_data    "data.frame"
#' @slot ui_options  "list"
#'
#' @export

setClass(
	Class          = "Engine",
	representation = representation(
		interpreter= "EngineCommandInterpreter",
		socket     = "ProcessSocket",
		store_name = "character",
		analys_str = "AnalysisObjectStore",
		app_store  = "BasicAppClient",
		trader     = "NumericOrCharacter",
		module_date= "character",
		lookback   = "character",
		key_hash   = "character",
		module_name= "character",
		app_path   = "character",
		app_port   = "character",
		app_host   = "character",
		response   = "character",
		module     = "list",
		app        = "function",
		analysis_ggplot = "ANY",
		analysis_data   = "data.frame",
		ui_options = "list"
	),
	prototype    = prototype(
		interpreter= new("EngineCommandInterpreter"),
		socket     = new("ProcessSocket"),
		store_name = TE.DataAccess:::engine_defaults@store_name_prefix,
		ui_options = list(omit = "Value", "PL")
	)
)

setGeneric("initialiseEngine",function(object){standardGeneric("initialiseEngine")})
setMethod("initialiseEngine","Engine",
		  function(object){

		    ready <- (length(object@trader) > 0 &&
		                length(object@lookback) > 0 &&
		                  length(object@module_date) > 0 &&
		                    length(object@module_name) > 0)


		  	if( ready ){
		  		#This depends in the implementation of how analysis data is located.
		  		#Currently based on a key function, may also prove to be useful to
		  		#use a date.
		  		message("Setting analysis module data store ...")
		  		trader <- ifelse(is.numeric(object@trader), object@trader, paste0( '"',object@trader, '"'))
		  	  kv 	   <- eval(parse(text=paste(object@lookback,"(",trader,",'",object@module_date,"')",sep="")))

		  	  object@key_hash <- as.character(murmur3.32(as.character(kv)))
		  	}
		    object@app_store <- new("BasicAppClient")
		  	return(object)
		  }
)


#' Start Frontend Engine Process
#'
#' @param object object of class "Engine"
#'
#' @export
setGeneric("startEngine",function(object){standardGeneric("startEngine")})

#' @describeIn startEngine
#' Start Frontend Engine
#'
#' @inheritParams startEngine
#'
#' @export
setMethod("startEngine","Engine",
		  function(object){
		  	message("Starting trading enhancement engine ...")
		  	message(paste("Attempting to establish socket on port",object@socket@port))
		  	object@socket <- openConnection(object@socket)
		  	request <- ""
		  	while(request != "STOP"){
		  		object <- initialiseEngine(object)
		  		object@response <- 'GOT' #Default reponse, sent if callback specifies to send response
		  								 #and does not reset the response
		  		skct <- tryCatch({
		  					readConnection(object@socket)
		  				}, error = function(cond){
		  					message(paste("Engine error on socket read:",cond))
		  				})
		  		if(length(skct)==0){
		  			message("Got zero length on read.")
		  			break
		  		}
		  		else{
		  			object@socket <- skct
		  			request <- getDataFromSocket(object@socket)
		  			message(paste("Got:",request))
		  			intr <- tryCatch({
		  						interpretCommand(object@interpreter,request)
		  					},error=function(cond){
		  						message(paste("Error when interpreting client request:",cond))

		  					})
		  			if(length(intr)==0){
		  				message("Got zero length interpreter object.")
						break
					}
					else{
						object@interpreter <- intr
						message(paste("Callback generated."))
		  				message("Executing...")
		  				obj <- tryCatch({
		  								#The interpreter generated a callback that is executed here
		  								#and causes the requested change to the engine state.
		  								object@interpreter@callback(object)
		  						  }, error = function(cond){
		  							   message("Engine error on command execution:",cond)
		  						  })
		  				if(length(obj)==0){
		  					message("Got zero length from callback execution.")
		  					break
		  				}
		  				else{
		  					object <- obj
		  					if(object@interpreter@snd_respnse){
		  						tryCatch({
		  							  sendResponse(object)
		  						},error=function(cond){
		  							  message("Engine error on sending response:",cond)
		  							  object@response <- "ERROR"
		  							  sendResponse(object)
		  						})
		  					}
		  					else{
		  						message("No response sent.")
		  					}
		  					if(object@interpreter@snd_data){
		  					  #If engine state was set to send data over the socket,
		  					  #this happens now.
		  						tryCatch({
		  							  sendModuleData(object)
		  						},error=function(cond){
		  							  message("Engine error on sending data:",cond)
		  							  object@response <- "ERROR"
		  							  sendResponse(object)
		  						})
		  					}
		  					else{
		  						message("No data sent.")
		  					}
		  					if(object@interpreter@run_app){
		  					  #If engine state set to run an app, the engine
		  					  #attempts to execute the callback to launch the app
		  					  #which will block engine exceution and make an HTTP service
		  					  #available.
		  					  object <- importAppData(object)
		  					  tryCatch({
		  					    object@app(object)
		  					  },error=function(cond){
		  					    message("Engine error on running application:",cond)
		  					    object@response <- "ERROR"
		  					    sendResponse(object)
		  					  })
		  					}
		  					else{
		  					  message("No application to run.")
		  					}
		  				}
					}
		  	 }
		  	}
		  	message("Engine exiting.")
		  	shutdownEngine(object)
		  }
)

setGeneric("importAppData",function(object,scramble=TRUE){standardGeneric("importAppData")})
setMethod("importAppData","Engine",
	function(object,scramble=TRUE){
	        block_client   <- tryCatch({new(paste(object@module_name,"Client",sep=""))},error=function(cond)stop(paste("Failed to set module name:",object@module_name,cond)))
			key_function   <- tryCatch({get(object@lookback)},error=function(cond)stop(paste("Failed to set lookback, exiting:",cond)))
			key_values     <- tryCatch({key_function(object@trader, object@module_date)},error=function(cond)stop(paste("Failed to set key values on date",object@module_date,"for trader",object@trader,":",cond)))
			block_client   <- tryCatch({dataRequest(block_client, key_values)},error=function(cond)stop(paste("Analysis data request failed:",cond)))
			block          <- tryCatch({getAnalysisBlock(block_client)},error=function(cond)stop(paste("Failed to set analysis block:",cond)))
			object@analysis_data  <- getOutputGGPlotData(block)
			object@analysis_ggplot<- getOutputGGPlot(block)
			omit <- getOutputFrontendData(block)

			if (length(omit$omit) > 0) {
			  object@ui_options[['omit']] <- unique(c(object@ui_options[['omit']], as.character(omit$omit)))
			}
			if(scramble){
			  object@analysis_data  <-scrambleData(object)
			  object@analysis_ggplot$data <- object@analysis_data
			}
			return(object)
	      }
)
#Should create a new class to do this
setGeneric("scrambleData",function(object){standardGeneric("scrambleData")})
setMethod("scrambleData","Engine",
	function(object){
	  data <- object@analysis_data
		data_cols <- colnames(object@analysis_data)
		if('Trader' %in% data_cols){
			data <- sub_column(data,'Trader',generic="Trader")
		}
		if('TraderID' %in% data_cols){
		  data <- sub_column(data,'TraderID',generic="Trader")
		}
		if('Strategy' %in% data_cols){
		  data <- sub_column(data,'Strategy',generic="Strategy")
		}
		return(data)
	}
)
sub_column <- function(data,col_name,generic=NULL){
	type <- class(data[[col_name]])[[1]]
	vals <- data[[col_name]]
	if(type=='character'||type=='factor'){
		remapper <- sample(letters,length(unique(vals)),replace=FALSE)
		names(remapper) <- as.character(unique(vals))
		if(length(generic)>0){
			new_vals <- paste(generic,remapper[vals])
		} else {
			new_vals <- remapper[vals]
		}
		data[[col_name]] <- new_vals
	} else if (type=='numeric'){
	  remapper <- 1:length(unique(vals))
	  names(remapper) <- unique(vals)
	  if(length(generic)>0){
	    new_vals <- paste(generic,remapper[vals])
	  } else {
	    new_vals <- remapper[vals]
	  }
	  data[[col_name]] <- new_vals
	}
	return(data)
}

setGeneric("shutdownEngine",function(object){standardGeneric("shutdownEngine")})
setMethod("shutdownEngine","Engine",
	      function(object){
			tryCatch({object@socket <- closeConnection(object@socket)},error=function(cond){})
	      }
)

setGeneric("sendResponse",function(object){standardGeneric("sendResponse")})
setMethod("sendResponse","Engine",
		  function(object){
		  	message(paste("Sending engine response ..."))
		  	writeToConnection(object@socket,object@response)
		  }
)

setGeneric("sendModuleData",function(object){standardGeneric("sendModuleData")})
setMethod("sendModuleData","Engine",
		  function(object){
		  	message(paste("Sending data from",object@module_name,"..."))
		  	module <- object@module[[1]]
		  	xml_string <- ""
		  	#change to use AnalysisModule getPanelData method
		  	for(panel in module@panels){
		  		for(vsn in panel@visualns){
		  			name <- paste(vsn@title,"-",vsn@y_label,"-",vsn@x_label,sep="")
		  			xml_string <- tryCatch({
		  					paste(xml_string,"#XMLDATAFRAME#",FrameToXML(vsn@plot_data,name),sep="")
		  				},error=function(cond){
		  					stop(paste("Error while converting data.frame to Xml:",cond))
		  				})
		  		}
		  		if(length(module@comp_result)>0){
		  			for(i in 1:length(module@comp_result)){
		  				xml_string <- tryCatch({
		  					paste(xml_string,"#XMLDATAFRAME#",FrameToXML(module@comp_result[[i]],names(module@comp_result)[[i]]),sep="")
		  				},error=function(cond){
		  					stop(paste("Error while converting data.frame to Xml: ",cond))
		  				})
		  			}
		  		}
		  	}
		  	writeToConnection(object@socket,xml_string)
		  	object@socket <- readConnection(object@socket)
		  	response <- getDataFromSocket(object@socket)
		  	while(response=='GETANALYSISDATA'){
		  		message(paste("Awaiting client data collection."))
		  		object@socket <- readConnection(object@socket)
		  		response <- getDataFromSocket(object@socket)
		  	}
		  	if(response!="GOT"){
		  		message(paste("WARNING: Client response not expected:",response))
		  	}
		  }
)



