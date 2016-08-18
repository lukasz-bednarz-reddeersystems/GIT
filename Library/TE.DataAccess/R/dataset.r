#' An S4 class to represent generic data set.
#'
#' @slot key_cols      "character"
#' @slot data_cols     "character"
#' @slot data          "data.frame"
#' @slot indexed       "logical"
#' @slot last_result   "data.frame"
#' @slot pad           "data.frame"
#' @slot unique_rows   "logical"
#' @export

setClass(
  Class          = "DataSet",
  representation = representation(
    key_cols     = "character",
    data_cols    = "character",
    data         = "data.frame",
    indexed      = "logical",
    last_result  = "data.frame",
    pad          = "data.frame",
    unique_rows  = "logical"
  ),
  prototype      = prototype(
    indexed      = FALSE,
    unique_rows  = FALSE
  )
)

#' Check if new incoming data has unique row
#'
#' @param object object of class 'DataSet'.
#' @param data  data.frame.
#' @return \code{object} object of class 'DataSet'.


setGeneric("checkUnique", function(object,data){standardGeneric("checkUnique")})
setMethod("checkUnique", "DataSet",
          function(object,data){
            unique_rows <- tryCatch({
                  object@unique_rows
              },error=function(cond){
                  return(FALSE)
              })
            if(unique_rows){
              ud <- unique(data)
              if(nrow(ud)!=nrow(data)){
                message("Removed duplicate rows.")
                #should implent key level uniqueness as
                #as option with the below code
                #if(nrow(unique(ud[object@key_cols]))!=nrow(ud)){
                #  message("WARNING: key degeneracy found, some different rows for duplicate key.")
                #  message("Duplicates removed arbitrarily.")
                #  ud <- subset(ud, !duplicated(ud[object@key_cols]))
                #}
              }
              data <- ud
            }
            if(nrow(object@data)>0){
              data <- data[apply(!is.na(data[object@key_cols]),1,function(z)Reduce(function(x,y)x&&y,z)),]
            }
            return(data)
          }
)

#' Return data stored in the DataSet
#'
#' @param object object of class 'DataSet'.
#' @return \code{data} data.frame with raw data.
#' @export

setGeneric("getData", function(object){standardGeneric("getData")})

#' @describeIn getData Sets new data in the DataSet
#'
#' @inheritParams getData
#' @return \code{data} data.frame with raw data.
#' @export

setMethod("getData", "DataSet",
          function(object){
            return(object@data)
          }
)

#' Sets new data in the DataSet
#'
#' @param object object of class 'DataSet'.
#' @param data  data.frame with data to be set.
#' @param override_full_index  Should the index be reset? Defaults to FALSE
#' @return \code{object} object of class 'DataSet'.
#' @export

setGeneric("setData", function(object,data,override_full_index=FALSE){standardGeneric("setData")})

#' @describeIn setData Sets new data in the DataSet
#'
#' @inheritParams setData
#' @return \code{object} object of class 'DataSet'.
#' @export
setMethod("setData", "DataSet",
          function(object,data,override_full_index=FALSE){
            message("Updating DataSet object.")
            columns <- c(object@key_cols,object@data_cols)
            ncolumns <- length(object@key_cols)+length(object@data_cols)
            if(ncolumns != length(data))
            {
              message("Error setting data in frame.")
              message(paste("Columns:",paste(colnames(data),collapse=" ")))
              stop("Incorrect number of dataframe columns for type.")
            }
            object@data <- checkUnique(object,data)
            if(object@indexed && !override_full_index)object <- createIndex(object)
            return(object)
          }
)

#' Reset internal data storage to empty dataframe in the dataset
#'
#' @param object object of class 'DataSet'.
#' @param data "data.frame" with new data
#' @return \code{object} object of class 'DataSet'.

setGeneric("resetData", function(object,data){standardGeneric("resetData")})
setMethod("resetData", "DataSet",
          function(object,data){
            message("Reseting DataSet object.")
            overlap <- length(intersect(object@key_cols,unique(colnames(data))))
            if(overlap != length(object@key_cols))
            {
              stop("Cannot reset to a frame that does not contain DataSet key columns.")
            }
            object@data_cols <- setdiff(colnames(data),object@key_cols)
            object@data <- data
            if(nrow(data)>0){
              if(object@indexed == TRUE)object <- createIndex(object)
            }
            return(object)
          }
)

#' Sets new data in the dataset and initializes it if not done
#'
#' @param object object of class 'DataSet'.
#' @param data  data.frame.
#' @return \code{object} object of class 'DataSet'.


setGeneric("initialiseOrSetData", function(object,data){standardGeneric("initialiseOrSetData")})
setMethod("initialiseOrSetData", "DataSet",
          function(object,data){
            if(length(object@data_cols)>0 && length(object@key_cols)>0){
              #Assume data set is defined but not populated
              message("Populating DataSet")
              object <- setData(object,data)
            }
            else if(length(object@key_cols)>0 && length(object@data_cols)==0){
              #DataSet key has been defined but the data has not, so populate and discover data columns
              message("Initialising DataSet.")
              all_cols <- colnames(data)
              if(length(object@key_cols) != length(intersect(object@key_cols,all_cols))){
                stop(paste("Key columns",paste(setdiff(object@key_cols,all_cols),collapse=", "),"dont exist in the dataframe."))
              }
              data_cols <- setdiff(all_cols,object@key_cols)
              object@data_cols <- data_cols
              object <- setData(object,data)
            }
            else{
              #Need a primary key to do anything, to create a dataset with a dynmamic key, use the
              #factory method
              stop("DataSet initialiseOrSetData: No key columns defined so cannot initialise data set.")
            }
            return(object)
          }
)


#' Appends new data in the dataset or initializes it if not done
#'
#' @param object object of class 'DataSet'.
#' @param data  data.frame.
#' @return \code{object} object of class 'DataSet'.

setGeneric("initialiseOrAppendData", function(object,data){standardGeneric("initialiseOrAppendData")})
setMethod("initialiseOrAppendData", "DataSet",
          function(object,data){
            if(nrow(object@data)==0){
              object <- initialiseOrSetData(object,data)
            }
            else{
              object <- appendData(object,data)
            }
            return(object)
          }
)

#' Runs aggregate on data stored in Dataset
#'
#' @param object object of class 'DataSet'.
#' @param target_cols  column names to be aggregated
#' @param group_by  column names to use for grouping
#' @param fn  function to use for aggregation
#' @param agg_unique  should the resulting data be unique?
#' @return \code{agg} object of class 'data.frame'.

setGeneric("aggregateGroup", function(object,target_cols,group_by,fn,agg_unique=FALSE){standardGeneric("aggregateGroup")})
setMethod("aggregateGroup", "DataSet",
          function(object,target_cols,group_by,fn,agg_unique=FALSE){
            all_cols <- c(object@key_cols,object@data_cols)
            agg <- aggregate_kernel(object@data,group_by,all_cols,target_cols,fn,agg_unique)
            return(agg)
          }
)


#' Runs multiple aggregate on data stored in Dataset within groups
#' defined by group_by_lst
#'
#' @param object object of class 'DataSet'.
#' @param target_cols  column names to be aggregated
#' @param group_by_lst list of  column names to use for sequential grouping
#' @param fn_lst  function to use for aggregation on each step
#' @param agg_unique  should the resulting data be unique?
#' @return \code{agg} object of class 'data.frame'.

setGeneric("aggregateGroupCompound", function(object,target_cols,group_by_lst,fn_lst,agg_unique=FALSE){standardGeneric("aggregateGroupCompound")})
setMethod("aggregateGroupCompound", "DataSet",
          function(object,target_cols,group_by_lst,fn_lst,agg_unique=FALSE){
            if(class(group_by_lst)[[1]]!='list' || class(fn_lst)[[1]]!='list')stop("Must specify list of aggregates to apply compound aggregate.")
            if(length(group_by_lst)!=length(fn_lst))stop("List of aggregates and aggregate functions must be same length to apply compound aggregate.")
            all_cols <- c(object@key_cols,object@data_cols)
            agg <- object@data
            for(group in length(group_by_lst)){
              agg <- aggregate_kernel(agg,group_by_lst[[group]],all_cols,target_cols,fn_lst[[group]],agg_unique)
            }
            return(agg)
          }
)


#' Workhorse function for multi-stage aggregation
#'
#' @param data object of class 'data.frame'.
#' @param all_cols  all columns of data.frame to be used
#' @param group_by  column names to use for grouping
#' @param target_cols  column names to be aggregated
#' @param fn  function to use for aggregation
#' @param agg_unique  should the resulting data be unique?
#' @param na value passed to aggregate(..., na.action = na)
#' @return \code{object} object of class 'DataSet'.

aggregate_kernel <- function(data,group_by,all_cols,target_cols,fn,agg_unique,na="na.omit"){
  if(length(group_by) != length(intersect(all_cols,group_by))){
    diff_cols <- setdiff(group_by,intersect(all_cols,group_by))
    diff_cols <- paste(diff_cols,collapse=", ")
    stop(paste(diff_cols,": Columns not found!",sep=""))
  }
  fstr<- paste(target_cols,collapse=",")
  fstr<- paste("cbind(",fstr,")",sep="")
  fstr<- paste(fstr,"~",paste(group_by,collapse="+"))
  frm <- as.formula(fstr)
  if(agg_unique){
    d <- unique(data[c(target_cols,group_by)])
  }
  else{
    d <- data
  }
  return(aggregate(frm,d,fn,na.action=na))
}

#' join existing data in dataset with incomming dataset
#' inner join might result in empty data
#'
#' @param object object of class 'DataSet'.
#' @param incoming_dataset object of class 'DataSet'.
#' @param on_columns  column names used for mering
#' @param aliases  incomming data.frame column aliases of existing dataset names
#' @param joinmode  join mode, default is 'inner'
#' @return \code{object} object of class 'DataSet'.

setGeneric("innerJoin", function(object,incoming_dataset,on_columns,aliases=NULL,joinmode='inner'){standardGeneric("innerJoin")})
setMethod("innerJoin", "DataSet",
          function(object,incoming_dataset,on_columns,aliases=NULL,joinmode='inner'){
            if(class(aliases)=="list")
            {
              cnames <- colnames(incoming_dataset@data)
              for(incoming_key in incoming_dataset@key_cols)
              {
                if(is.null(aliases[[incoming_key]])==FALSE){
                  on_columns <- replace(on_columns,on_columns==incoming_key,aliases[[incoming_key]])
                  incoming_dataset@key_cols <- replace(incoming_dataset@key_cols,incoming_dataset@key_cols==incoming_key,aliases[[incoming_key]])
                  cnames <- replace(cnames,cnames==incoming_key,aliases[[incoming_key]])
                }
              }
              colnames(incoming_dataset@data) <- cnames
            }

            if(length(object@data)==0)
            {
              message("Composite dataset not yet initialised, initialising with this DataSet.")
              object@key_cols <- incoming_dataset@key_cols
              object@data_cols <- incoming_dataset@data_cols
              object <- setData(object,incoming_dataset@data)
            }
            else if(length(on_columns) != length(intersect(object@key_cols,on_columns)))
            {
              message("Cannot merge data over non-key columns.")
              if(length(intersect(object@key_cols,on_columns))==0)
              {
                stop("No intersecting key columns. Define aliases?")
              } else {
                message("WARNING: Using only intersecting key columns to join.")
                on_columns <- intersect(object@key_cols,on_columns)
              }
            }
            else
            {
              coltypes_identical <- Reduce(function(x,y)x&&y,unlist(Map(function(x)class(object@data[[x]]),on_columns))==unlist(Map(function(x)class(incoming_dataset@data[[x]]),on_columns)))
              if(!coltypes_identical)message("WARNING: Joining on columns with different classes, may give unexpected result.")
              if(joinmode=='cross'){
                data <- merge(object@data,incoming_dataset@data,by=NULL)
              }
              else if(joinmode == 'left'){
                data <- merge(x=object@data,y=incoming_dataset@data,by=on_columns,all.x=TRUE)
              }
              else if(joinmode == 'right'){
               data <- merge(x=object@data,y=incoming_dataset@data,by=on_columns,all.y=TRUE)
              }
              else{
               data <- merge(object@data,incoming_dataset@data,by=on_columns)
              }
              object@key_cols <- intersect(colnames(data),c(object@key_cols,incoming_dataset@key_cols))
              object@data_cols <- c(object@data_cols,incoming_dataset@data_cols)

              if(nrow(data)>0){
                object <- resetData(object,data)
              }
              else{
                message("WARNING: Data set innerJoin produced an empty result.")
                l <- length(object@key_cols) + length(object@data_cols)
                empty_frame <- as.data.frame(setNames(replicate(l,numeric(0), simplify = F), letters[1:l]))
                colnames(empty_frame) <- c(object@key_cols,object@data_cols)
                object@data <- empty_frame
              }
            }
            return (object)
          }
)


#' Join existing data in dataset with incomming data
#' inner join might result in empty data
#'
#' @param object object of class 'DataSet'.
#' @param frame object of class 'data.frame'.
#' @param key_cols  column names used for merging
#' @param aliases  incomming data.frame column aliases of existing dataset names
#' @param joinmode  join mode, default is 'inner'
#' @return \code{object} object of class 'DataSet'.

setGeneric("innerJoinFrame", function(object,frame,key_cols,aliases=NULL,joinmode='inner'){standardGeneric("innerJoinFrame")})
setMethod("innerJoinFrame", "DataSet",
          function(object,frame,key_cols,aliases=NULL,joinmode='inner'){
            new_dataset <- dataset_factory(key_cols,frame)
            object <- innerJoin(object,new_dataset,key_cols,aliases,joinmode)
            return(object)
          }
)

#' Find set difference between stored keys and argument value
#' stores the result internaly
#'
#' @param object object of class 'DataSet'.
#' @param key_values  data.frame with columns corresponding to object key_columns and values of the keys to compare
#' @return \code{object} object of class 'DataSet'.

setGeneric("keyDiff",function(object,key_values){standardGeneric("keyDiff")})
setMethod("keyDiff","DataSet",
          function(object,key_values){
            if(length(intersect(colnames(key_values),object@key_cols))!=ncol(key_values))stop("Can only keyDiff on DataSet key columns.")
            if(object@indexed == FALSE){
              message("Indexing dataset to apply keyDiff...")
              object <- createIndex(object)
            }
            f_keys <- hashRows(object,key_values,object@key_cols,type='foreign')
            l_keys <- hashRows(object,object@data,object@key_cols,type='foreign')
            ocols <- colnames(key_values)
            key_values$f_keys <- f_keys
            object@last_result <- subset(key_values,key_values$f_keys %in% setdiff(f_keys,l_keys))
            object@last_result <- object@last_result[ocols]
            return(object)
          }
)

#' Create new index for dataset
#'
#' @param object object of class 'DataSet'.
#' @return \code{object} object of class 'DataSet'.

setGeneric("createIndex",function(object){standardGeneric("createIndex")})
setMethod("createIndex","DataSet",
          function(object){
            message("Indexing dataset...")
            if(nrow(object@data)>0)pvals <- 1:nrow(object@data)
            pad <- tryCatch({
              if(nrow(object@data)>0)cbind(object@data[object@key_cols],PadValue=pvals)
              },error=function(cond){
                stop("Failure during dataset indexing")
              })
            if(length(pad)>0)object@pad<-pad
            row_hash_values <- hashRows(object,object@data,object@key_cols)
            rownames(object@data) <- row_hash_values
            object@indexed <- TRUE
            return(object)
          }
)

#' Update index based on new incoming data
#'
#' @param object object of class 'DataSet'.
#' @param new_data  data.frame with new data to be added to dataset
#' @return \code{object} object of class 'DataSet'.

setGeneric("updateIndex",function(object,new_data){standardGeneric("updateIndex")})
setMethod("updateIndex","DataSet",
          function(object,new_data){
            if(object@indexed&&nrow(object@data >0)){
              message("Updating dataset index...")
              pvals <- 1:nrow(object@data)
              pad <- tryCatch({
                                cbind(object@data[object@key_cols],PadValue=pvals)
                              },error=function(cond){
                                stop(paste("Error during dataset indexing, key pad update failed:",cond))
                              })
              object@pad<-pad
              new_hash_values <- hashRows(object,new_data,object@key_cols)
              row_hash_values <- c(rownames(object@data[1:(nrow(object@data)-nrow(new_data)),]),new_hash_values)
              tryCatch({
                          rownames(object@data) <- row_hash_values
                       }, error=function(cond){
                          stop(paste("Error during dataset indexing, could not set hash values:",cond))

                       })
            }
            else{
              message("Cannot update index, index not yet created or dataset empty.")
            }
            return(object)
          }
)

#' Hash keys of new incomming rows
#'
#' @param object object of class 'DataSet'.
#' @param rows  data.frame with new rows to be added to dataset
#' @param keys  columnames of columns used as keys for hashing
#' @param type = 'local', c('foreign', 'local'),
#' @return \code{hashed_rows} vector of hashed key values

setGeneric("hashRows",function(object,rows,keys,type='local'){standardGeneric("hashRows")})
setMethod("hashRows","DataSet",
          function(object,rows,keys,type='local'){
            if(length(keys) != length(intersect(colnames(rows),keys)))stop("Attempt to hash local key on non-key columns")
            if(type=='foreign'){
              frame <- rows[keys]
              columns <- c(keys)
            }
            else {
              columns <- c(keys,'PadValue')
              rows <- cbind(rows,PadValue=(max(object@pad$PadValue)-nrow(rows))+1:nrow(rows))
              frame <- tryCatch({
                          unique(merge(object@pad,rows[columns],by=columns))
                       },error=function(cond){
                          stop(paste("DataSet index error:",cond))
                       })
            }
            return (apply(frame[columns],1,function(x){paste(cityhash.64(paste(gsub(" ","",as.character(x),fixed=TRUE),collapse="")),collapse="")}))
          }
)

#' Check if new data is unique values in reference to stored data
#'
#' @param object object of class 'DataSet'.
#' @param new_data  data.frame with new data
#' @return \code{new_data} data.frame with incoming_data without duplicated values

setGeneric("compareUnique", function(object,new_data){standardGeneric("compareUnique")})
setMethod("compareUnique", "DataSet",
          function(object,new_data){
            unique_rows <- tryCatch({
                  object@unique_rows
              },error=function(cond){
                  return(FALSE)
              })
            if(unique_rows){
              data <- rbind(object@data,new_data)
              data <- subset(data,!duplicated(data))
              ud <- data[(nrow(object@data)+1):nrow(data),]
              if(nrow(ud)!=nrow(new_data)){
                message("Removed duplicate rows when appending data.")
              }
              new_data <- ud
            }
            return(new_data)
          }
)


#' Append new data to dataset
#'
#' @param object object of class 'DataSet'.
#' @param new_data  data.frame with new data
#' @return \code{object} object of class 'DataSet'

setGeneric("appendData",function(object,new_data){standardGeneric("appendData")})
setMethod("appendData","DataSet",
          function(object,new_data){
            message("Appending to DataSet object.")
            new_data <- compareUnique(object,new_data)
            data <- object@data
            data <- tryCatch({
                 rbind(data,new_data)
              },error=function(cond){
                 message(paste("Could not append data to DataSet:",cond))
                 stop("Try to aggregate to DataSet instead?")
              })
            object <- attemptUpdate(object,data,new_data)
            return(object)
          }
)

#' Aggregates new data with new columns to dataset
#'
#' @param object object of class 'DataSet'.
#' @param new_data  data.frame with new data
#' @return \code{object} object of class 'DataSet'

setGeneric("aggregateData", function(object,new_data){standardGeneric("aggregateData")})
setMethod("aggregateData", "DataSet",
          function(object,new_data){
            message("Aggregating to DataSet object.")
            new_data <- compareUnique(object,new_data)
            columns <- unique(c(object@key_cols,object@data_cols,colnames(new_data)))
            data <- rbind.fill(object@data,new_data)
            object@data_cols <- setdiff(columns,object@key_cols)
            object <- attemptUpdate(object,data,new_data)
            return(object)
          }
)

#' Safely attempts to update dataset with new data
#'
#' @param object object of class 'DataSet'.
#' @param data  data.frame with new dataset data to be set combined with existing data
#' @param new_data  data.frame with original new data
#' @return \code{object} object of class 'DataSet'

setGeneric("attemptUpdate", function(object,data,new_data){standardGeneric("attemptUpdate")})
setMethod("attemptUpdate", "DataSet",
          function(object,data,new_data){
            unique_rows <- tryCatch({
              object@unique_rows
            },error=function(cond){
              return(FALSE)
            })
            if(nrow(new_data)>0){
              tryCatch({
                object <- setData(object,data,override_full_index=TRUE)
                if(object@indexed)object <- updateIndex(object,new_data)
              },error=function(cond){
                message(paste("Dataset update failed:",cond))
                if(unique_rows){
                  message("Attempting to rebuild dataset index...")
                  tryCatch({
                    object <- setData(object,data)
                  },error=function(cond){
                    stop(paste("Dataset index build failed:",cond))
                  })
                }
                else{
                  stop("Dataset not enforcing unique rows, try setting unique_rows flag.")
                }
              })
            }
            return(object)
          }
)

first <- function(x){
  rval <- NA
  vals <- x[!is.na(x)]
  if(length(vals)==1){
    rval <- vals
  }
  else if(length(vals)>1){
    rval <- vals[[1]]
  }
  return(rval)
}

#' aggregates underlying data on specified columns and using specified function
#'
#' @param object object of class 'DataSet'.
#' @param by  character vector of column names on which to aggregate on, default are key columns
#' @param fn  function used for aggregation, default is first()
#' @return \code{object} object of class 'DataSet'
setGeneric("rowCoalesceOn", function(object,by=NULL,fn=first){standardGeneric("rowCoalesceOn")})

setMethod("rowCoalesceOn", "DataSet",
          function(object,by=NULL,fn=first){
            if(length(by)==0)by <- object@key_cols
            data_cols <- setdiff(colnames(object@data),by)
            types <- unlist(Map(function(x)class(x)[[1]],object@data))
            types[types=='factor'] <- 'character'
            skip <- names(types)[types=='character']
            if(length(skip)>0)message("WARNING: rowCoalesceOn stripped character columns from the frame.")
            data_cols <- setdiff(data_cols,skip)
            output <- aggregate_kernel(object@data,by,c(by,data_cols),data_cols,fn,FALSE,na="na.pass")
            object <- resetData(object,output)
            return(object)
          }
)


#' Builds a dataset from a dataframe
#'
#' @param key_cols character vector ofs column names to be used as identifying keys.
#' @param data data.frame to generate dataset.from
#' @return \code{object} object of class 'DataSet'
#' @export

dataset_factory <- function(key_cols,data){
  all_cols <- colnames(data)
  if(length(key_cols) != length(intersect(key_cols,all_cols)))stop("Some key columns dont exist in data frame")
  data_cols <- setdiff(all_cols,key_cols)
  message("Creating DataSet.")
  dataset <- new("DataSet")
  dataset@key_cols <- key_cols
  dataset@data_cols <- data_cols
  dataset <- setData(dataset,data)
  return(dataset)
}
