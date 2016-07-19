sourceTo("../common/global_configs.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../lib/sockets.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../lib/frame_to_xml.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_module.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
sourceTo("../analysis_modules/analysis_module_library.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

setClass(
	Class          = "EngineCommandInterpreter",
	representation = representation(
		snd_respnse= "logical",
		snd_data   = "logical",
		callback   = "function",
		split_char = "character",
		cmmd_lst   = "character"
	),
	prototype      = prototype(
		split_char = "|"
	)
)

setGeneric("setEngineSlotCallback",function(object,slot_name,value_getter,snd_response=TRUE,snd_data=FALSE){standardGeneric("setEngineSlotCallback")})
setMethod("setEngineSlotCallback","EngineCommandInterpreter",
		  function(object,slot_name,value_getter,snd_response=TRUE,snd_data=FALSE){
		  	object@callback     <- function(engine,get_value=value_getter){slot(engine,slot_name) <- get_value(engine)
		  						   return(engine)}
		  	object@snd_respnse  <- snd_response
		  	object@snd_data     <- snd_data
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

setGeneric("setTraderCallback",function(object){standardGeneric("setTraderCallback")})
setMethod("setTraderCallback","EngineCommandInterpreter",
	      function(object){
	      	object <- setEngineSlotCallback(object,'trader',function(engine)as.numeric(engine@interpreter@cmmd_lst[2]))
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
		  						SETTRADER  = setTraderCallback(object),
		  						SETDATE = setDateCallback(object),
		  						GETANALYSISDATA = getModuleDataCallback(object),
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
	Class          = "Engine",
	representation = representation(
		interpreter= "EngineCommandInterpreter",
		socket     = "ProcessSocket",
		store_name = "character",
		analys_str = "AnalysisObjectStore",
		trader     = "numeric",
		module_date= "character",
		lookback   = "character",
		key_hash   = "character",
		module_name= "character",
		response   = "character",
		module     = "list"
	),
	prototype      = prototype(
		interpreter= new("EngineCommandInterpreter"),
		socket     = new("ProcessSocket"),
		store_name = engine_defaults@store_name_prefix
	)
)

setGeneric("initialiseEngine",function(object){standardGeneric("initialiseEngine")})
setMethod("initialiseEngine","Engine",
		  function(object){
		  	if(length(object@trader)>0 && length(object@lookback)>0 && length(object@module_date)){
		  		#This depends in the implementation of how analysis data is located. 
		  		#Currently based on a key function, may also prove to be useful to 
		  		#use a date.
		  		message("Setting analysis module data store ...")
		  		kv 	   <- eval(parse(text=paste(object@lookback,"(",object@trader,",'",object@module_date,"')",sep="")))
		  		hrname <- paste(kv[[1,1]],"_",as.character(min(kv[['start']])),"_",as.character(max(kv[['end']])),sep="")
		  		object@key_hash <- as.character(murmur3.32(as.character(kv)))
		  		tryCatch({
		  				object@analys_str <- analysis_store_request(paste(object@store_name,hrname,sep=""))		
		  			}, error = function(cond){
		  				stop("Fatal error, could not setup the analysis data source.")
		  			})
		  	}
		  	return(object)
		  }
)

setGeneric("startEngine",function(object){standardGeneric("startEngine")})
setMethod("startEngine","Engine",
		  function(object){
		  	message("Starting trading enhancement engine ...")
		  	message(paste("Attempting to establish socket on port",object@socket@port))
		  	object@socket <- openConnection(object@socket)
		  	request <- "";
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
		  			request <- getData(object@socket)
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
		  								#The interpreter generates a callback that when executed here
		  								#causes the requested change to the engine state.
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
		  				}
					}
		  		}
		  	}	
		  	message("Engine exiting.")
		  	shutdownEngine(object)
		  }
)

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
		  	response <- getData(object@socket)	
		  	while(response=='GETANALYSISDATA'){
		  		message(paste("Awaiting client data collection."))
		  		object@socket <- readConnection(object@socket)
		  		response <- getData(object@socket)	
		  	}
		  	if(response!="GOT"){
		  		message(paste("WARNING: Client response not expected:",response))
		  	}
		  }
)



