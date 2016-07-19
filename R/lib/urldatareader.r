#Classes designed to read XML descriptions of RAID data elements from
#a URL.

library(XML)
sourceTo("../lib/urldatareader_functions.r", modifiedOnly = getOption("modifiedOnlySource"), local = FALSE)

setClass(
  Class            = "URLQuery",
  representation   = representation(
    fields         = "character",
    values         = "character",
    root_url       = "character",
    url            = "character"
  )
)

setGeneric("buildURL",function(object){standardGeneric("buildURL")})
setMethod("buildURL","URLQuery",
          function(object){
            if(length(object@fields)!=length(object@values))stop("Unequal number of URL query fields and values.")
            object@url <- paste(object@root_url,"?",sep="")
            amp=""
            for(i in 1:length(object@fields)){
              object@url <- paste(object@url,amp,object@fields[i],"=",object@values[i],sep="")
              amp="&"
            }
            return(object)
          }
)

setClassUnion("NullableDoc",c("XMLDocument","NULL"))
setClass(
    Class          = "URLDataReader", 
    representation = representation(
      data         = "NullableDoc"
    )
)

setGeneric("readURL",function(object,url){standardGeneric("readURL")})
setMethod("readURL","URLDataReader",
          function(object,url){
            message(paste("Reading from",url,"..."))
            doc <- tryCatch(
              {
                xmlTreeParse(url)
              },
              error=function(cond)
              {
                message("Error while reading URL, skipping:")
                message(cond)
              })
            if(length(doc)>0)object@data<-doc
            return (object)
          }
)

setGeneric("clearData",function(object){standardGeneric("clearData")})
setMethod("clearData","URLDataReader",
          function(object){
            object@data <- NULL
            return(object)
          }
)

#Default schema based on output of C# DataTable serialisation with CreateXML method
suppressWarnings(setClassUnion("NullableNode",c("XMLNode","NULL")))
setClass(
  Class            = "XMLObject",
  representation   = representation(
    xml_schema     = "NullableNode",
    xml_body       = "NullableNode",
    schema_tag     = "character",
    schema_element = "character",
    body_tag       = "character"
  ), 
  prototype        = prototype(
    schema_tag     = "schema",
    schema_element = "element",
    body_tag       = "DocumentElement"
  )
)

setClassUnion("RObject",c("data.frame","matrix"))
setClass(
  Class            = "XMLToR",
  representation   = representation(
    data_obj       = "RObject",
    row_name       = "character",
    columns        = "character",
    types          = "character",
    converter      = "function"
  ), 
  contains = c("XMLObject")
)

setGeneric("autoSchema",function(object,obj_xml_document){standardGeneric("autoSchema")})
setMethod("autoSchema","XMLToR",
          function(object,obj_xml_document){
            xml <- xmlElementsByTagName(xmlRoot(obj_xml_document), object@schema_tag, recursive = TRUE)
            if(length(xml)>1) message("Warning: More than one schema tag level")
            if(length(xml)==0)stop(paste("No schema tag",object@schema_tag,"found."))
            object@xml_schema <- xml[[1]]
            elements <- xmlElementsByTagName(object@xml_schema,object@schema_element, recursive = TRUE)
            row_name <- xmlAttrs(elements[[2]])
            object@row_name <- row_name[c('name')][[1]]
            for(cln in 3:length(elements)){
              object@columns <- c(object@columns,xmlAttrs(elements[[cln]])[c('name')][[1]])
              object@types <- c(object@types,xmlAttrs(elements[[cln]])[c('type')][[1]])
            }
            object@xml_schema <- NULL
            return (object)
          }
)

setGeneric("convertXML",function(object,obj_xml_document,auto){standardGeneric("convertXML")})
setMethod("convertXML","XMLToR",
          function(object,obj_xml_document,auto){
            message("Converting XML to R object ...")
            if(auto == TRUE){
              message("Attempting automatic schema recovery ...")
              object <- autoSchema(object,obj_xml_document)
            }
            xml <- xmlElementsByTagName(xmlRoot(obj_xml_document),object@body_tag,TRUE)
            if(length(xml)>1) message("Warning: More than one document body tag level")
            if(length(xml)==0){
              message(paste("No body tag",object@body_tag,"found."))
            }
            else{
              object@xml_body <- xml[[1]]  
              message("Attempting type conversion ... ")
              object@data_obj <- object@converter(object@columns,object@types,object@xml_body)
              message("Done.")
              object@xml_body <- NULL
            }
            return (object)
          }
)

setClass(
  Class       = "XMLToMatrix",
  prototype   = prototype(
    converter = convert2Matrix
  ), 
  contains = c("XMLToR")
)

setClass(
  Class       = "XMLToFrame",
  prototype   = prototype(
    converter = convert2Frame
  ), 
  contains = c("XMLToR")
)

#All XMLConverter types must implement a method named
#convertXML outputing the relevant R object having slot
#named 'data object' containing the R data item
setClassUnion("XMLConverter",c("XMLToMatrix","XMLToFrame"))
setClass(
    Class          = "XMLParser",
    representation = representation(
      auto         = "logical",
      converter    = "XMLConverter"
    ), 
    prototype      = prototype(
      auto         = TRUE
    )
)

setGeneric("parseXML",function(object,xml_document){standardGeneric("parseXML")})
setMethod("parseXML","XMLParser",
          function(object,xml_document){
            object@converter <- convertXML(object@converter,xml_document,object@auto)
            return(object)
          }
)

setGeneric("getRobject",function(object){standardGeneric("getRobject")})
setMethod("getRobject","XMLParser",
          function(object){
            return(object@converter@data_obj)
          }
)

setClass(
  Class            = "URLParser",
  representation   = representation(
    reader_instance= "URLDataReader",
    class_instance = "XMLParser",
    urls           = "character",
    objects        = "environment",
    parser_type    = "character",
    cstm_schm_tag  = "character",
    cstm_schm_elem = "character",
    cstm_body_tag  = "character"
  ), prototype     = prototype(
    objects   = new.env()
  )
)

setGeneric("setUp", function(object){standardGeneric("setUp")})
setMethod("setUp", "URLParser",
          function(object){
            object@reader_instance<- new("URLDataReader")
            object <- newParserInstance(object)
            object <- setParserCustomTags(object)
            return (object)
          }
)

setGeneric("newParserInstance", function(object){standardGeneric("newParserInstance")})
setMethod("newParserInstance", "URLParser",
          function(object){
            object@class_instance <- new("XMLParser")    
            object@class_instance@converter <- new(object@parser_type)
            return (object)
          }
)

setGeneric("setParserCustomTags", function(object){standardGeneric("setParserCustomTags")})
setMethod("setParserCustomTags", "URLParser",
          function(object){
            if(length(object@cstm_schm_tag)>0)object@class_instance@converter@schema_tag <- object@cstm_schm_tag
            if(length(object@cstm_schm_elem)>0)object@class_instance@converter@schema_element <- object@cstm_schm_elem
            if(length(object@cstm_body_tag)>0)object@class_instance@converter@body_tag <- object@cstm_body_tag
            return(object)
          }
)

setGeneric("runURLs", function(object,urls){standardGeneric("runURLs")})
setMethod("runURLs", "URLParser",
          function(object,urls){
            message("Initialising...")
            object@urls <- urls
            object <- setUp(object)
            for(url in object@urls){
              message(paste("Fetching source:",url))
              object@reader_instance <- readURL(object@reader_instance,url)
              if(length(object@reader_instance@data)>0){
                parser <- tryCatch(
                  {
                    parseXML(object@class_instance,object@reader_instance@data)
                  },
                  error = function(cond)
                  {
                    message(paste("Failed to parse url:",cond))
                  }
                )
                object@objects[[url]] <- parser
              } 
              else 
              {
                message("No data in the URL reader, skipping URL")
              }
              object@reader_instance <- clearData(object@reader_instance)
            }
            return(object)
          }
)

setGeneric("getURL",function(object,test_index){standardGeneric("getURL")})
setMethod("getURL","URLParser",
          function(object,test_index){
            url <- object@urls[test_index]
            rval <- tryCatch({
                                object@objects[[url]]
                             },error=function(cond){})
            return(rval)
          }
)

setGeneric("getURLData",function(object,test_index){standardGeneric("getURLData")})
setMethod("getURLData","URLParser",
          function(object,test_index){
            obj <- getURL(object,test_index)
            if(length(obj)>0){
              rval <- obj@converter@data_obj
            }
            else
            {
              rval <- NULL
            }
            return(rval)
          }
)

setGeneric("getColnames",function(object,index){standardGeneric("getColnames")})
setMethod("getColnames","URLParser",
          function(object,index){
            obj <- getURL(object,index)
            rval <- tryCatch({
              obj@converter@columns
            },error=function(cond){
              message("XML Parser: Attempt to retrive data schema, but none defined.")
            })
            return(rval)
          }
)
