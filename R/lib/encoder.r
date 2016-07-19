sourceTo("../lib/referencedata.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)
#Encoders can be thought of as referance data classes with the ability 
#to encode and decode based upon the internal data.

setClass(
  Class          = "Encoder",
  slots = c(
    input_symbol_cols  = "character",
    output_classes_cols= "character",
    has_dictionary     = "boolean"
  ),
  prototype      = list(
    input_symbol_cols  = character(),
    output_classes_cols= character(),
    has_dictionary     = FALSE
  ),
  contains = c("VirtualReferenceData")
)

setGeneric("encodeWord",function(object,data_frame_values){standardGeneric("encodeWord")})
setMethod("encodeWord", "Encoder",
	function(object,data_frame_values){
		if(length(intersect(colnames(data_frame_values),object@input_symbol_cols))!=length(object@input_symbol_cols))stop("Invalid symbol columns for encoding.")
		return(merge(getReferenceData(object),data_frame_values,by=object@symbol_cols))
	}
)

setGeneric("decodeClass", function(object,data_frame_values){standardGeneric("decodeClass")})
setMethod("decodeClass", "Encoder",
	function(object,data_frame_values){
		if(length(intersect(colnames(data_frame_values),object@output_classes_cols))!=length(object@output_classes_cols))stop("Invalid symbol columns for decoding.")
		return(merge(getReferenceData(object),data_frame_values,by=object@output_classes_cols))	
	}
)

setGeneric("setDictionary",function(object,dictionary){standardGeneric("setDictionary")})
setMethod("setDictionary","Encoder",
	function(object,dictionary){
		all_cols <- c(object@input_symbol_cols,object@output_classes_cols)
		if(length(intersect(all_cols,colnames(dictionary)))!=length(all_cols))stop("Attempt to set invalid encoder dictionary")
		object   <- setReferanceData(object,dictionary)
		object@has_dictionary <- TRUE
		return(object)
	}
)

setGeneric("growDictionary",function(object,new_mapping){standardGeneric("growDictionary")})
setMethod("growDictionary","Encoder",
	function(object,new_mapping){
		if(object@has_dictionary){
			all_cols <- c(object@input_symbol_cols,object@output_classes_cols)
			if(length(intersect(all_cols,colnames(new_mapping)))!=length(all_cols))stop("Attempt to grow encoder dictionary with invalid mappings")
			object <- appendVariables(object,new_mapping,all_cols)	
		} else{
			message("Encoder dictionary must be initialised.")
		}
		return(object)
	}
)

setGeneric("setOrGrowDictionary",function(object,dictionary){standardGeneric("setOrGrowDictionary")})
setMethod("setOrGrowDictionary","Encoder",
	function(object,dictionary){
		if(object@has_dictionary){
			object <- growDictionary(object,dictionary)
		} else{
			object <- setDictionary(object,dictionary)
		}
	    return(object)
	}
)