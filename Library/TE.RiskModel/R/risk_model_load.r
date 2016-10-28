#' @include risk_model_functions.r
NULL


##################################################################################################################
#
# ModelFactors
#
##################################################################################################################

get_model_type_factors <-function(model_name, lookback){

  ret <- query_model_type_factors(model_name, lookback)

  factor_info <- get_factors()[c("lFactorID", "sFactorName")]

  ret <- merge(factor_info, ret)

  if (nrow(ret) ==  0) {
    ret <- NULL
  }

  return(ret[,"sFactorName"])
}


query_model_type_factors <- function(model_name, lookback){

  model_type_id <- get_model_type_id(model_name, lookback)

  cn <- odbcConnect(.__DEFAULT_RISK_MODEL_DB__,uid=.__DEFAULT_RAID_DB_USER__ )
  on.exit(odbcClose(cn))
  SQL <- paste0("SELECT * FROM [Research].[dbo].[tMultiFactorRiskModelFactors] WHERE ",
                "[lModelTypeID] = ", model_type_id)
  ret <- sqlQuery(cn, SQL, as.is = FALSE)
  return(ret)
}


insert_model_type_factors <- function(model_name, lookback, factors){

  model_type_id <- get_model_type_id(model_name, lookback)

  model_factors <- get_model_type_factors(model_name, lookback)

  if (!is.null(model_factors) && (length(model_factors) > 0)) {
    message(paste("Some or all factors are already inserted inserting only missing ones."))
    print(cbind(model_type_id, data.frame(model_name, lookback)))

    factors <- setdiff(factors, model_factors)
  }

  if (length(factors) > 0) {
    factor_info <- get_factors()[c("lFactorID", "sFactorName")]

    factor_data <- merge(factor_info, data.frame(sFactorName = factors))

    data <- cbind(data.frame(lModelTypeID = model_type_id), factor_data[c("lFactorID")])

    cn <- odbcConnect(.__DEFAULT_RISK_MODEL_DB__,uid=.__DEFAULT_RAID_DB_USER__ )
    on.exit(odbcClose(cn))

    sqlSave(cn, data, tablename = "tMultiFactorRiskModelFactors", append = TRUE, rownames = FALSE)
  }

  return(query_model_type_factors(model_name, lookback))
}

##################################################################################################################
#
# ModelType
#
##################################################################################################################

get_model_type_id <-function(model_name, lookback){

  ret <- query_model_type_id(model_name, lookback)

  if (nrow(ret) ==  0) {
    ret <- insert_model_type(model_name, lookback)
  }

  return(ret[1,"lModelTypeID"])
}


query_model_type_id <- function(model_name, lookback){
  cn <- odbcConnect(.__DEFAULT_RISK_MODEL_DB__,uid=.__DEFAULT_RAID_DB_USER__ )
  on.exit(odbcClose(cn))
  SQL <- paste0("SELECT [lModelTypeID] FROM [Research].[dbo].[tMultiFactorRiskModelTypes] WHERE",
                "[sName] = '", model_name,"'",
                "AND [lLookback] = ", lookback)
  ret <- sqlQuery(cn, SQL, as.is = FALSE)
  return(ret)
}

insert_model_type <- function(model_name, lookback){

  model_type_id <- query_model_type_id(model_name, lookback)

  if (nrow(model_type_id) > 0) {
    message(paste("Model Type entry already present in database [Research].[dbo].[tMultiFactorRiskModels] for given parameters"))
    print(cbind(model_type_id, data.frame(model_name, lookback)))
    return(model_type_id)
  } else {

    cn <- odbcConnect(.__DEFAULT_RISK_MODEL_DB__,uid=.__DEFAULT_RAID_DB_USER__ )
    on.exit(odbcClose(cn))
    SQL <- paste0("INSERT INTO [Research].[dbo].[tMultiFactorRiskModelTypes] (",
                  "[lLookback], ",
                  "[sName]",
                  ") VALUES (", lookback,", '",
                                 model_name, "')"
    )

    sqlQuery(cn, SQL)
    return(query_model_type_id(model_name, lookback))
  }
}

##################################################################################################################
#
# Model
#
##################################################################################################################

insert_model_definition <- function(model_date,computation_date,lookback,model_name){

  model_type <- get_model_type_id(model_name, lookback)

  model_id <- query_model_id(model_type, model_date, computation_date)

  if (nrow(model_id) > 0) {
    message(paste("Model entry already present in database for given parameters"))
    print(cbind(model_id, data.frame(model_type, model_date, computation_date)))
    return(model_id)
  } else {

    cn <- odbcConnect(.__DEFAULT_RISK_MODEL_DB__,uid=.__DEFAULT_RAID_DB_USER__ )
    on.exit(odbcClose(cn))
    SQL <- paste0("INSERT INTO [Research].[dbo].[tMultiFactorRiskModels] (",  "[lModelTypeID], ",
                                                                              "[dtModelDate], ",
                                                                              "[dtComputationDate] ",
                                                                              ") VALUES (", model_type,", '",
                                                                                            model_date,"', '",
                                                                                            computation_date,"')"
                  )

    sqlQuery(cn, SQL)
    return(query_model_id(model_type, model_date, computation_date))
  }
}



query_model_id <- function(model_type,model_date,computation_date = NULL){

  cn <- odbcConnect(.__DEFAULT_RISK_MODEL_DB__,uid=.__DEFAULT_RAID_DB_USER__ )
  on.exit(odbcClose(cn))

  if (is.null(computation_date)) {
    SQL <- paste0("EXEC [dbo].[prMultiFactorRisk_ModelID_SelectByModelTypeModelDateComputationDate]",
                  "@lModelTypeID = ", model_type,
                  ", @dtModelDate = '", model_date, "'")
  }
  else {
    SQL <- paste0("EXEC [dbo].[prMultiFactorRisk_ModelID_SelectByModelTypeModelDateComputationDate]",
  		           "@lModelTypeID = ", model_type,
                 ", @dtModelDate = '", model_date, "'",
                 ", @dtComputationDate = '", computation_date, "'")
  }
  ret <- sqlQuery(cn, SQL, as.is = FALSE)
  return(ret)
}


get_model_id <-function(model_date,computation_date,lookback,model_name){

  model_type <- get_model_type_id(model_name, lookback)

  ret <- query_model_id(model_type,model_date,computation_date)

  if (nrow(ret) ==  0) {
    return(NULL)
  } else {
    return(ret[1,"lModelID"])
  }
}

get_model_info <-function(model_id){

  cn <- odbcConnect(.__DEFAULT_RISK_MODEL_DB__,uid=.__DEFAULT_RAID_DB_USER__ )
  on.exit(odbcClose(cn))
  SQL <- paste0("EXEC	[dbo].[prMultiFactorRisk_ModelInfo_SelectByModelID] @lModelID = ", model_id)
  ret <- sqlQuery(cn, SQL, as.is = FALSE)
  return(ret)
}

##################################################################################################################
#
# FactorTypes
#
##################################################################################################################

query_factor_type_id <- function(factor_type){

  cn <- odbcConnect(.__DEFAULT_RISK_MODEL_DB__,uid=.__DEFAULT_RAID_DB_USER__ )
  on.exit(odbcClose(cn))
  SQL <- paste0("SELECT [lFactorTypeID] FROM [Research].[dbo].[tMultiFactorRiskFactorTypes] WHERE",
                "[sFactorTypeName] = '", factor_type, "'")
  ret <- sqlQuery(cn, SQL, as.is = FALSE)
  return(ret)
}


insert_factor_type <- function(factor_type){

  factor_type_id <- query_factor_type_id(factor_type)

  if (nrow(factor_type_id) > 0) {
    message(paste("Factor", factor_type, "already present in database."))
    print(factor_type_id)
    return(factor_type_id)
  } else {

    cn <- odbcConnect(.__DEFAULT_RISK_MODEL_DB__,uid=.__DEFAULT_RAID_DB_USER__ )
    on.exit(odbcClose(cn))
    SQL <- paste0("INSERT INTO [Research].[dbo].[tMultiFactorRiskFactorTypes] (",  "[sFactorTypeName]",
                  ") VALUES ('", factor_type, "')"
    )

    sqlQuery(cn, SQL)
    return(query_factor_type_id(factor_type))
  }
}

##################################################################################################################
#
# Factor
#
##################################################################################################################

insert_factor <- function(factor_name){

  factor_id <- query_factor_id(factor_name)

  if (nrow(factor_id) > 0) {
    message(paste("Factor", factor_name, "already present in database."))
    print(factor_id)
    return(factor_id)
  } else {

    cn <- odbcConnect(.__DEFAULT_RISK_MODEL_DB__,uid=.__DEFAULT_RAID_DB_USER__ )
    on.exit(odbcClose(cn))
    SQL <- paste0("INSERT INTO [Research].[dbo].[tMultiFactorRiskFactors] (",  "[sFactorName]",
                  ") VALUES ('", factor_name, "')"
    )

    sqlQuery(cn, SQL)
    return(query_factor_id(factor_name))
  }
}


query_factor_id <- function(factor_name){

  cn <- odbcConnect(.__DEFAULT_RISK_MODEL_DB__,uid=.__DEFAULT_RAID_DB_USER__ )
  on.exit(odbcClose(cn))
  SQL <- paste0("SELECT [lFactorID] FROM [Research].[dbo].[tMultiFactorRiskFactors] WHERE",
                "[sFactorName] = '", factor_name, "'")
  ret <- sqlQuery(cn, SQL, as.is = FALSE)
  return(ret)
}

get_factor_name<- function(factor_id){

  cn <- odbcConnect(.__DEFAULT_RISK_MODEL_DB__,uid=.__DEFAULT_RAID_DB_USER__ )
  on.exit(odbcClose(cn))
  SQL <- paste0("SELECT [sFactorName] FROM [Research].[dbo].[tMultiFactorRiskFactors] WHERE",
                "[lFactorID] = ", factor_id)
  ret <- sqlQuery(cn, SQL, as.is = FALSE)
  return(unlist(ret["sFactorName"][1]))
}

get_factor_id <-function(factor_name){

  ret <- query_factor_id(factor_name)

  return(unlist(ret[1,"lFactorID"]))
}


get_factors <- function(){

  cn <- odbcConnect(.__DEFAULT_RISK_MODEL_DB__,uid=.__DEFAULT_RAID_DB_USER__ )
  on.exit(odbcClose(cn))
  SQL <- paste0("EXEC	[dbo].[prMultiFactorRisk_Factors_InfoSelectAll]")
  ret <- sqlQuery(cn, SQL, as.is = FALSE)
  return(ret)
}


##################################################################################################################
#
# FactorCouples
#
##################################################################################################################

query_factor_couple_id <- function(factor1_name, factor2_name){

  cn <- odbcConnect(.__DEFAULT_RISK_MODEL_DB__,uid=.__DEFAULT_RAID_DB_USER__ )
  on.exit(odbcClose(cn))

  factor1_id <- get_factor_id(factor1_name)
  factor2_id <- get_factor_id(factor2_name)

  factor_ids <- sort(c(factor1_id, factor2_id))

  SQL <- paste0("SELECT [lFactorCoupleID] FROM [Research].[dbo].[tMultiFactorRiskFactorCouples] WHERE",
                "[lFactor1ID] = ", factor_ids[[1]],
                " AND [lFactor2ID] = ", factor_ids[[2]])

  ret <- sqlQuery(cn, SQL, as.is = FALSE)
  return(ret)
}

insert_factor_couple <- function(factor1_id, factor2_id){


  factor_ids <- sort(c(factor1_id, factor2_id))

  factor1_name <- get_factor_name(factor_ids[[1]])
  factor2_name <- get_factor_name(factor_ids[[2]])

  factor_couple_id <- query_factor_couple_id(factor1_name, factor2_name)

  if (nrow(factor_couple_id) > 0) {
    message(paste("Factor couple :",factor1_name ,factor2_name , "already present in database."))
    print(factor_couple_id)
    return(factor_couple_id)
  } else {

    cn <- odbcConnect(.__DEFAULT_RISK_MODEL_DB__,uid=.__DEFAULT_RAID_DB_USER__ )
    on.exit(odbcClose(cn))
    SQL <- paste0("INSERT INTO [Research].[dbo].[tMultiFactorRiskFactorCouples] ( [lFactor1ID], [lFactor2ID]",
                  ") VALUES (", factor_ids[1], "," , factor_ids[2], ")"
    )

    sqlQuery(cn, SQL)
    return(query_factor_couple_id(factor1_name, factor2_name))
  }
}

get_factor_couple_id <- function(factor1_name, factor2_name){

  ret <- query_factor_couple_id(factor1_name, factor2_name)

  return(unlist(ret[1,"lFactorCoupleID"]))
}


get_factor_couples <- function(){

  cn <- odbcConnect(.__DEFAULT_RISK_MODEL_DB__,uid=.__DEFAULT_RAID_DB_USER__ )
  on.exit(odbcClose(cn))
  SQL <- paste0("EXEC	[dbo].[prMultiFactorRisk_FactorCouples_InfoSelectAll]")
  ret <- sqlQuery(cn, SQL, as.is = FALSE)
  return(ret)
}


##################################################################################################################
#
# Generic Functions
#
##################################################################################################################

write_data_to_file <- function(data_to_write,filename){

  if (file.exists(filename)) {
    file.remove(filename)
  }
  options(scipen=0, digits = 22)
  sblock <- 1
  eblock <- min(c(50000,nrow(data_to_write)))
  end <- FALSE
  while(!end){
    first <- TRUE
    cnt <- 1
    if(eblock>=nrow(data_to_write))end <- TRUE
    data <- data_to_write[sblock:eblock,]
    message(paste("Writing:",round(100*(sblock-1)/nrow(data_to_write))))
    # fd <- format(data, scientific = FALSE, justify = "right", nsmall = 15, digits = 15, na.encode = FALSE)
    fd <- data
    cwd <- getwd()
    write.table(fd,filename,col.names=FALSE,row.names=FALSE,sep='|',append=TRUE, quote = FALSE, na = "")
    setwd(cwd)
    sblock <- eblock+1
    eblock <- min(c(eblock+50000,nrow(data_to_write)))
  }
  options(scipen=0)
}


query_existing_data <- function(table_name, unique_keys){

  cn <- odbcConnect(.__DEFAULT_RISK_MODEL_DB__,uid=.__DEFAULT_RAID_DB_USER__ )
  on.exit(odbcClose(cn))
  SQL <- paste0("SELECT DISTINCT ",
                paste(colnames(unique_keys), collapse = ", "),
                " FROM ", table_name, " WHERE ")

  next_statement <- FALSE
  for (col in colnames(unique_keys)) {
    val <- unique_keys[1, col]
    if (is.Date(val)) {
      val <- as_date(val)
    }

    if (next_statement) {
      SQL <- paste(SQL, "AND ")
    } else {
      next_statement <- TRUE
    }

    SQL <- paste0(SQL, "[", col, "] = '", unique_keys[1, col], "'")
  }

  print(SQL)
  ret <- sqlQuery(cn, SQL, as.is = FALSE)
  return(ret)
}

format_datetime <- function(df) {

  for (col in colnames(df)) {
    if ( grepl("dt", col)){
      df[col] <- ymd(df[,col])
    }
  }

  return(df)

}


bulk_load_data <- function(table_name, data, unique_keys, index_key_names) {

  cn <- odbcConnect(.__DEFAULT_RISK_MODEL_DB__,uid=.__DEFAULT_RAID_DB_USER__ )
  on.exit(odbcClose(cn))

  old_data <- query_existing_data(table_name, unique_keys)
  # cure date time issue with date comming from database
  old_data <- format_datetime(old_data)

  old_data <- merge(data, old_data)

  insert_data <- rbind(old_data, data)
  insert_data <- insert_data[! duplicated(insert_data[index_key_names]) & seq(nrow(insert_data)) > nrow(old_data), ]

  if (nrow(insert_data) == 0) return()

  col_names <- as.character(sqlColumns(cn, table_name)[4L][,1L])

  insert_data <- insert_data[col_names]

  #unc_dirname <- paste0("\\\\", RAIDSTAGEDB, "\\c$\\Temp\\risk_data\\")
  unc_dirname <- "\\\\raidlivedb\\upload\\"
  #srv_dirname <- "c:\\Temp\\risk_Data\\"
  srv_dirname <- "j:\\upload\\"
  filename <- paste(table_name)
  next_statement <- FALSE
  for (col in colnames(unique_keys)) {
    val <- unique_keys[col][1]
    if (is.Date(val)) {
      val <- as_date(val)
    }

    filename <- paste(filename, col , val[1,1], sep = "_")
  }

  filename <- paste0( filename, ".txt")
  write_data_to_file(insert_data,paste0(unc_dirname, filename))

  SQL <- paste("BULK INSERT ",
                table_name,
                paste0("FROM '", srv_dirname, filename, "'"),
                "WITH",
                "(",
                "FIELDTERMINATOR = '|',",
                "ROWTERMINATOR = '\\n',",
                "MAXERRORS = 1,",
                "KEEPNULLS",
                ")",
                sep = " ")


  ret <- tryCatch({
    sqlQuery(cn, SQL)
  }, error = function(cond) {
    message(paste("Error when BULK INSERTing data from file", paste0(unc_dirname,  filename), "into table", table_name))
    stop(cond)
    }
  )

  #file.remove(file.path(unc_dirname, filename))
  return(ret)
}

##################################################################################################################
#
# InstrumentBetas
#
##################################################################################################################

bulk_load_factor_betas <- function(betas, rm_type){

  cn <- odbcConnect(.__DEFAULT_RISK_MODEL_DB__,uid=.__DEFAULT_RAID_DB_USER__ )
  on.exit(odbcClose(cn))

  table_name <- "dbo.tMultiFactorRiskBetas"
  col_names <- as.character(sqlColumns(cn, table_name)[4L][,1L])

  for (date in unique(betas$Date)) {
    first <-  TRUE
    for (factor in setdiff(colnames(betas), c("Date", "Instrument"))) {

      row <- betas[betas$Date == date,c("Date", "Instrument", factor)]
      colnames(row) <- c("dtDate","lInstrumentID", "dblBeta")

      factor_id <- get_factor_id(factor)

      row <- cbind(data.frame(lModelTypeID = rm_type), row, data.frame(lFactorID = factor_id))

      if (first) {
        insert_data <- row
        first <- FALSE
      } else {
        insert_data <- rbind(insert_data, row)
      }

    }

    insert_data <- insert_data[,col_names]
    index_key_names <- c("lModelTypeID", "dtDate", "lInstrumentID", "lFactorID" )

    bulk_load_data(table_name, insert_data, data.frame(lModelTypeID = rm_type, dtDate = as_date(date)), index_key_names )

  }

}

##################################################################################################################
#
# FactorReturns
#
##################################################################################################################


bulk_load_implied_factor_returns <- function(returns, rm_type){

  cn <- odbcConnect(.__DEFAULT_RISK_MODEL_DB__,uid=.__DEFAULT_RAID_DB_USER__ )
  on.exit(odbcClose(cn))

  table_name <- "dbo.tMultiFactorRiskImpliedReturns"
  col_names <- as.character(sqlColumns(cn, table_name)[4L][,1L])


  for (date in unique(returns$Date)) {
    first <-  TRUE
    for (factor in setdiff(colnames(returns), c("Date", "Instrument"))) {

      row <- returns[returns$Date  == date ,c("Date", factor)]
      colnames(row) <- c("dtDate","dblLogReturn")

      factor_id <- get_factor_id(factor)

      row <- cbind(data.frame(lModelTypeID = rm_type), row, data.frame(lFactorID = factor_id))

      row <- row[,col_names]

      if (first) {
        data <- row
        first = FALSE
      } else {
        data <- rbind(data, row)
      }

    }

    index_key_names <- c("lModelTypeID", "dtDate", "lFactorID" )
    bulk_load_data(table_name, data, data.frame(dtDate = as_date(date)), index_key_names )
  }
}

##################################################################################################################
#
# FactorVariance
#
##################################################################################################################

bulk_load_factor_variances <- function(variance, rm_id){

  cn <- odbcConnect(.__DEFAULT_RISK_MODEL_DB__,uid=.__DEFAULT_RAID_DB_USER__ )
  on.exit(odbcClose(cn))

  table_name <- "dbo.tMultiFactorRiskFactorVariance"
  col_names <- as.character(sqlColumns(cn, table_name)[4L][,1L])

  model_info <- get_model_info(rm_id)

  if (length(unique(variance$Date)) != 1) {
    message("Invalid Variance data frame passed to bulk_load_factor_variances")
    message("Variance data.frame contains values for more than one date.")
    message(paste("Date range :", range(variance$Date)))
    stop("Invalid Variance data frame passed to bulk_load_factor_variances")
  } else if (ymd(model_info$dtModelDate[1]) != unique(variance$Date)) {
    message(paste("Specified model_id :", model_info$dtModelDate, "is different than date"))
    message(paste("of the variance data :", unique(variance$Date)))
    stop("Invalid Variance data frame passed to bulk_load_factor_variance")
  }

  first <- TRUE
  for (factor in setdiff(colnames(variance), c("Date"))) {

    row <- variance[c(factor)]
    colnames(row) <- c("dblVariance")

    factor_id <- get_factor_id(factor)

    row <- cbind(data.frame(lModelID = rm_id), row, data.frame(lFactorID = factor_id))

    row <- row[,col_names]

    if (first) {
      data <- row
      first <- FALSE
    } else {
      data <- rbind(data, row)
    }
  }

  index_key_names <- c("lModelID", "lFactorID" )
  bulk_load_data(table_name, data, data.frame(lModelID = rm_id), index_key_names )

}

##################################################################################################################
#
# FactorCorrelation
#
##################################################################################################################

bulk_load_factor_correlations <- function(correlation, rm_id){

  cn <- odbcConnect(.__DEFAULT_RISK_MODEL_DB__,uid=.__DEFAULT_RAID_DB_USER__ )
  on.exit(odbcClose(cn))

  table_name <- "dbo.tMultiFactorRiskFactorCorrelation"
  col_names <- as.character(sqlColumns(cn, table_name)[4L][,1L])

  model_info <- get_model_info(rm_id)

  if (length(unique(correlation$Date)) != 1) {
    message("Invalid Correlation data frame passed to bulk_load_factor_correlations")
    message("Correlation data.frame contains values for more than one date.")
    message(paste("Date range :", range(correlation$Date)))
    stop("Invalid correlation data frame passed to bulk_load_factor_correlations")
  } else if (ymd(model_info$dtModelDate[1]) != unique(correlation$Date)) {
    message(paste("Specified model_id :", model_info$dtModelDate, "is different than date"))
    message(paste("of the variance data :", unique(correlation$Date)))
    stop("Invalid Variance data frame passed to bulk_load_factor_correlation")
  }


  factor_info <- get_factors()[c("lFactorID", "sFactorName")]
  rownames(factor_info) <- factor_info$sFactorName

  fc <- correlation[,-1]

  if (nrow(fc) != ncol(fc)) {
    message("Invalid Correlation data frame passed to bulk_load_factor_correlations")
    message("Number of rows is not equal to number of columns")
    stop("Invalid correlation data frame passed to bulk_load_factor_correlations")
  }

  factor_names <- colnames(fc)
  rownames(fc) <- factor_names
  factor_ids <- factor_info[factor_names,]
  factor_ids <- factor_ids[order(factor_ids$lFactorID),]

  # sort matrix row/columns by factor_ID's
  fc <- fc[,as.character(factor_ids$sFactorName)]
  fc <- fc[as.character(factor_ids$sFactorName),]

  factor_ids$matIndex <- seq(nrow(factor_ids))

  # select lower triangular part of the Correlation matrix and vectorize,
  fc_matrix <- as.matrix(fc)
  fc_lower_tri <- lower.tri(fc_matrix, diag = TRUE)
  rownames(fc_lower_tri) <- rownames(fc_matrix)
  colnames(fc_lower_tri) <- colnames(fc_matrix)
  fc_couples <- which(fc_lower_tri, arr.ind = TRUE, useNames = TRUE)
  fc_couples_df <- data.frame(row = fc_couples[,1],col = fc_couples[,2])
  fc_couples_df$dblCorrelation <- fc[fc_lower_tri]

  # creating new factor id info per each factor
  factor1_ids <- factor_ids[c("lFactorID", "sFactorName", "matIndex")]
  colnames(factor1_ids) <- c("lFactor1ID", "sFactor1Name", "matIndex")
  fc_couples_df <- merge(fc_couples_df,factor1_ids, by.x = c("col"), by.y = c("matIndex"))

  factor2_ids <- factor_ids[c("lFactorID", "sFactorName", "matIndex")]
  colnames(factor2_ids) <- c("lFactor2ID", "sFactor2Name", "matIndex")
  fc_couples_df <- merge(fc_couples_df,factor2_ids, by.x = c("row"), by.y = c("matIndex"))


  # get couples info from db
  fc_couples_info <- get_factor_couples()

  insert_data <- merge(fc_couples_df, fc_couples_info, by=c("sFactor1Name","sFactor2Name","lFactor1ID", "lFactor2ID"))

  insert_data <- cbind(insert_data, data.frame(lModelID = rm_id))

  if (nrow(fc_couples_df) != nrow(insert_data)) {
    message("Problem occured during merge of factor correlations with factor couple ids")
    message("Operation outcome has reduced number of rows")
    stop("Invalid merge of factor_couple_ids with correlation vector in bulk_load_factor_correlations()")
  }

  index_key_names <- c("lModelID", "lFactorCoupleID" )
  insert_data <- insert_data[,col_names]
  bulk_load_data(table_name, insert_data, data.frame(lModelID = rm_id), index_key_names )

}

query_factor_correlations_on_model_id <- function(rm_id) {
  cn <- odbcConnect(.__DEFAULT_RISK_MODEL_DB__,uid=.__DEFAULT_RAID_DB_USER__ )
  on.exit(odbcClose(cn))

  SQL <- paste0("EXEC	[dbo].[prMultiFactorRisk_FactorCorrelation_SelectByModelID] @lModelID = ", rm_id)
  ret <- sqlQuery(cn, SQL, as.is = FALSE)
  return(ret)

}

get_factor_correlations_matrix_on_model_id <- function(rm_id) {

  model_info <- get_model_info(rm_id)
  if (nrow(model_info) == 0) {
    message(paste("Query for factor model_info for model", rm_id, "returned empty data frame." ))
    message("Please verify if valid model id have been passed")
    stop(paste("Query for factor model_info for model", rm_id, "returned empty data frame." ))
  }

  fc_couples_df <- query_factor_correlations_on_model_id(rm_id)
  if (nrow(fc_couples_df) == 0) {
    message(paste("Query for factor correlations for model ID", rm_id, "returned empty data frame." ))
    message("Please verify if data for model have been loaded")
    stop(paste("Query for factor correlations for model ID", rm_id, "returned empty data frame." ))
  }

  if (!setequal(unique(fc_couples_df$sFactor1Name),unique(fc_couples_df$sFactor1Name))) {
    message(paste("Returned data frame doesn't contain all necessary correlation matrix elements." ))
    message("Please verify if data for model have been properly loaded")
    stop(paste("Returned data frame doesn't contain all necessary correlation matrix elements." ))
  }

  # sort couples data.frame
  fc_couples_df <- fc_couples_df[with(fc_couples_df, order(fc_couples_df$lFactor1ID, fc_couples_df$lFactor2ID)),]

  fc_names <- unique(fc_couples_df$sFactor1Name)

  fc_matrix <- matrix(data = 0, nrow = length(fc_names), ncol = length(fc_names))

  fc_matrix[lower.tri(fc_matrix, diag = TRUE)] <- fc_couples_df$dblCorrelation

  upper_tmp <- fc_matrix

  diag(upper_tmp) <- 0

  fc_matrix <- fc_matrix + t(upper_tmp)

  if (!is.positive.definite(fc_matrix)) {
    message(paste("Generated factor correlation matrix is not positive definite ." ))
    message("Please verify if data for model have been properly loaded and matrix is properly constructed")
    stop(paste("Generated factor correlation matrix is not positive definite ." ))
  }

  colnames(fc_matrix) <- fc_names
  rownames(fc_matrix) <- fc_names

  return(as.data.frame(fc_matrix))

}

##################################################################################################################
#
# MarketStyle
#
##################################################################################################################


bulk_load_market_style <- function(ms, rm_id) {

  cn <- odbcConnect(.__DEFAULT_RISK_MODEL_DB__,uid=.__DEFAULT_RAID_DB_USER__ )
  on.exit(odbcClose(cn))

  table_name <- "dbo.tMultiFactorRiskMarketStyle"
  col_names <- as.character(sqlColumns(cn, table_name)[4L][,1L])

  model_info <- get_model_info(rm_id)

  if (length(unique(ms$Date)) != 1) {
    message("Invalid Market Style data frame passed to bulk_load_market_style")
    message("Market Style data.frame contains values for more than one date.")
    message(paste("Date range :", range(ms$Date)))
    stop("Invalid ms data frame passed to bulk_load_factor_mss")
  } else if (ymd(model_info$dtModelDate[1]) != unique(ms$Date)) {
    message(paste("Specified model_id :", model_info$dtModelDate, "is different than date"))
    message(paste("of the variance data :", unique(ms$Date)))
    stop("Invalid Variance data frame passed to bulk_load_factor_ms")
  }


  first <- TRUE
  for (factor in setdiff(colnames(ms), c("Date"))) {

    row <- ms[c(factor)]
    colnames(row) <- c("dblZScore")

    factor_id <- get_factor_id(factor)

    row <- cbind(data.frame(lModelID = rm_id), row, data.frame(lFactorID = factor_id))

    row <- row[,col_names]

    if (first) {
      data <- row
      first <- FALSE
    } else {
      data <- rbind(data, row)
    }
  }

  index_key_names <- c("lModelID", "lFactorID" )
  bulk_load_data(table_name, data, data.frame(lModelID = rm_id), index_key_names )

}

query_market_style_on_model_id <- function(rm_id) {
  cn <- odbcConnect(.__DEFAULT_RISK_MODEL_DB__,uid=.__DEFAULT_RAID_DB_USER__ )
  on.exit(odbcClose(cn))

  SQL <- paste0("EXEC	[dbo].[prMultiFactorRisk_MarketStyle_SelectByModelID] @lModelID = ", rm_id)
  ret <- sqlQuery(cn, SQL, as.is = FALSE)
  return(ret)

}


##################################################################################################################
#
# MarketStyle
#
##################################################################################################################

bulk_load_residual_returns <- function(returns, rm_id){

  cn <- odbcConnect(.__DEFAULT_RISK_MODEL_DB__,uid=.__DEFAULT_RAID_DB_USER__ )
  on.exit(odbcClose(cn))

  table_name <- "dbo.tMultiFactorRiskResidualReturns"
  col_names <- as.character(sqlColumns(cn, table_name)[4L][,1L])

  model_info <- get_model_info(rm_id)
  lookback <- model_info$lLookback[1]
  model_date <- ymd(model_info$dtModelDate[1])

  if (diff(range(unique(returns$Date))) > lookback) {
    message("Invalid Market Style data frame passed to bulk_load_residual_returns")
    message("Residual Returns data.frame contains dates that span over lookback period indicated by model.")
    message(paste("Date range :", range(returns$Date)))
    stop("Invalid returns data frame passed to bulk_load_residual_returns")
  } else if (model_date < max(returns$Date)) {
    message(paste("Specified model_id date:", model_info$dtModelDate, "is smaller than maximum date"))
    message(paste("of the residual returns data :", max(returns$Date)))
    stop("Invalid residual returns data frame passed to bulk_load_residual_returns")
  } else if (model_date - lookback > min(returns$Date)) {
    message(paste("Specified model_id start date:", model_date + 1 - lookback, "is smaller than minimum date"))
    message(paste("of the residual returns data :", min(returns$Date)))
    stop("Invalid residual returns data frame passed to bulk_load_residual_returns")
  }

  data <- returns[c("Date" ,"Instrument", "Return")]
  colnames(data) <- c("dtDate" ,"lInstrumentID","dblLogReturn")

  data <- cbind(data.frame(lModelID = rm_id), data)

  data <- data[,col_names]
  index_key_names <- c("dtDate", "lModelID",  "lInstrumentID" )

  bulk_load_data(table_name, data, data.frame(lModelID = rm_id), index_key_names )
}

query_residual_returns_on_model_id <- function(rm_id) {
  cn <- odbcConnect(.__DEFAULT_RISK_MODEL_DB__,uid=.__DEFAULT_RAID_DB_USER__ )
  on.exit(odbcClose(cn))

  SQL <- paste0("EXEC	[dbo].[prMultiFactorRisk_ResidualReturns_SelectByModelID] @lModelID =", rm_id)
  ret <- sqlQuery(cn, SQL, as.is = FALSE)
  return(ret)

}
