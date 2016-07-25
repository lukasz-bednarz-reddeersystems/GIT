#' Generates XML from data.frame
#'
#' @param df data.frame to convert
#' @param character name
#' @param tree_name name of the xml tree, default is 'Root'
#' @return \code{xml} character xml string
#' @examples
#' rowCoalesceOn(object,by=NULL,fn=first)
#' @export
FrameToXML <- function(df,name,tree_name="Root")
{
  name <- gsub(' ','_',name)
  name <- gsub('/','_vs_',name)
  name <- gsub('\\(','_lc_',name)
  name <- gsub('\\)','_rc_',name)
  raw_names <- colnames(df)
  names_types <- parse_column_names(raw_names)
  nmes <- names_types[[1]]
  types<- names_types[[1]]

  if(substring(name,1,1)=='-')name <- substring(name,2,nchar(name))
  xml <- xmlTree(tree_name)
  xml$addNode(name,close=FALSE)
  xml$addNode("RAIDRESULTS",close=FALSE,attrs=c("raidnspaces"="RAIDNSPACES"))
  for (i in 1:nrow(df)) {
  	   xml$addNode("RAIDROW",close=FALSE)
       for (j in 1:length(nmes)) {
           xml$addNode("RAIDCELL",close=FALSE,attrs=c("title"=nmes[[j]]))
           raid_attrs <- generate_raid_attribs(types[[j]],df[i,j])
           raid_value <- format_value(types[[j]],df[i,j])
           xml$addNode("td",raid_value,attrs=raid_attrs)
           xml$closeTag()
       }
   xml$closeTag()
  }
xml$closeTag()
return(gsub('\n','',saveXML(xml$value())))
}

#Examples:
#<td class="dataTableCell positive" actualvalue="129759.986280" style="text-align:right;">$129,760</td>
generate_raid_attribs <- function(type,value){
  attribs <- switch(type,
                      STRING=c("class"="dataTableCell","actualvalue"=as.character(value),"style"="text-align:right;"),
                      INT=c("class"="dataTableCell","actualvalue"=as.character(value),"style"="text-align:right;"),
                      PERCENT_1DP=c("class"="dataTableCell","actualvalue"=as.character(value),"style"="text-align:right;"),
                      QUANTITY_1D=c("class"="dataTableCell","actualvalue"=as.character(value),"style"="text-align:right;")
                    )
  if(length(attribs)==0)stop("Failed to parse dataframe column name to XML type.")
  return(attribs)
}

format_na <- function(value){
  rval <- value
  if(is.na(value))rval <- ""
  return(r)
}

format_value <- function(type,value){
  if(type=='STRING'){
    if(value=='<NA>')value    <- ""
    if(length(value)==0)value <- ""
  }
  else{
    if(length(value)==0)value <- NA
    if(is.na(value))value     <- NA
    if(is.nan(value))value    <- NA
  }
  raid_val <- switch(type,
                      STRING=value,
                      INT = sprintf("%d",round(value)),
                      PERCENT_1DP = sprintf("%.1f%%",value*100),
                      QUANTITY_1DP = sprintf("%.1f",value),
                      MONEY = sprintf("$%d",round(value))
                    )
  return(raid_val)
}

parse_column_names <- function(raw_names){
  types<- list()
  nmes <- list()
  for(i in 1:length(raw_names)){
    splt <- strsplit(raw_names[i],'\\|')[[1]]
    if(length(splt)>2){
      stop("Found multiple type delimeters in data.frame column names when converting to XML.")
    }
    else if(length(splt)==2){
      types[[i]] <-splt[1]
      nmes[[i]] <- splt[2]
    }
    else{
      types[[i]] <-'STRING'
      nmes[[i]] <- splt[1]
    }
  }
  return(list(nmes,types))
}
