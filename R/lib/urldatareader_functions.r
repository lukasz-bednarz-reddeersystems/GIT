library(XML)
library(plyr)

#Conversion of XMLDocument to a matrix given specified and column names
#assumes that data is not ragged, is 2d and is uniformly numeric.
convert2Matrix <- function(columns,types,obj_xml_document_body){
  rval <- xmlSApply(obj_xml_document_body, function(x) xmlSApply(x, xmlValue))
  if(class(rval)!="matrix")stop("Error during conversion: Suspect ragged data dimensions. Try converting to frame.")
  if(ncol(rval) != length(columns)){
    rval <- t(rval)
  }
  rval <- matrix(as.numeric(rval),ncol=length(columns))
  colnames(rval) <- columns
  return(rval)
}

#Conversion of XMLDocument to a data.frame given specified column names 
#and types. Assumes that data is 2d.
convert2Frame <- function(columns,types,obj_xml_document_body){
  values <- list()
  cntr <- 1
  for(chld in xmlChildren(obj_xml_document_body)){
    values[[cntr]]<-xmlSApply(chld, xmlValue)
    cntr <- cntr + 1
  }
  rval <- make_empty_frame(columns,types)
  for(i in 1:length(values)){
    rval <- rbind.fill(rval,as.data.frame(t(as.matrix(unlist(values[[i]])))))
  }
  
  for(i in 1:ncol(rval)){
    ty = types[i]
    if(ty=="xs:int" || ty=="xs:decimal" || ty == "xs:float" || ty == "xs:long" || ty == "xs:unsignedByte" || ty == "xs:double"){
      rval[,i] <- as.numeric(as.character(rval[,i]))
    } 
    else if(ty=="xs:string"){
      rval[,i] <- as.character(rval[,i])
    }
    else if(ty=="xs:dateTime"){
      rval[,i] <- as.Date(substr(rval[,i],1,10))
    }
    else if(ty=="xs:boolean"){
      rval[,i] <- as.logical(rval[,i])
    }
    else{
      stop(paste("Encountered an unknown type specficiation",ty,"while converting to data.frame."))
    }
  }
  nams <- rownames(rval)
  rownames(rval) <- make.names(nams, unique=TRUE)
  colnames(rval) <- columns
  return(rval)
}

make_empty_frame <- function(columns,types){
  evalstat <- "data.frame("
  for(i in 1:length(columns)){
    evalstat <- paste(evalstat,columns[i],"=character(),",sep="")
  }
  evalstat <- paste(evalstat,"stringsAsFactors=FALSE)",sep="")
  return (eval(parse(text=evalstat)))
}
