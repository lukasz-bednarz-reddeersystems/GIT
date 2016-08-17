factor_transform <- function(event_data,keys,data_col){
  key_values <- unique(event_data[keys])
  not_na <- rep(TRUE,nrow(key_values))
  for(key in keys){
    not_na <- not_na&!is.na(key_values[key])
  }
  key_values <- key_values[not_na,]
  types <- unique(event_data[data_col])
  types <- types[!is.na(types)]
  initialise <- TRUE
  for(ty in types){
    subdata <- event_data
    subdata <- subdata[!is.na(subdata[data_col]),]
    subdata <- subdata[subdata[data_col]==ty,]
    subdata <- unique(subdata)
    colnames(subdata)[colnames(subdata)==data_col]<-ty
    if(initialise){
      rval <- merge(key_values,subdata,by=keys,all.x=TRUE)
      initialise <- FALSE
    }
    else{
      rval <- merge(rval,subdata,by=keys,all.x=TRUE)
    }
  }
  rval <- cbind(rval[keys],data.frame(Map(function(x)!is.na(x),rval[types])))
  return(rval)
}