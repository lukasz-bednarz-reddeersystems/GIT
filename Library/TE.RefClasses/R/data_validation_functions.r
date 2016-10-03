########################################################################
#
# basic_unique_data_validator
#
########################################################################
basic_unique_data_validator <- function(data,unique_on){
  return(data[!duplicated(data[unique_on]),])
}
