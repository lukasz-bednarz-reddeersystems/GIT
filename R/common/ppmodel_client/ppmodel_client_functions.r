warehouse_name_from_key <- function(key){
  return(paste(c(as.character(key['id'][[1]]),as.character(key['start'][[1]]),as.character(key['end'][[1]])),collapse="_"))
}