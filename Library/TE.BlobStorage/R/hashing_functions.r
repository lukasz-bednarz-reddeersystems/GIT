#' @include TE.BlobStorage.r
NULL



serialize_data_frame <- function(df){

  df <- df[sort(colnames(df))]

  # sort on all columns
  df <- df[do.call(order, df),]

  # xml creates warning when adding node to empty document
  xml <- suppressWarnings(xmlTree("root"))

  for (j in names(df)) {
    xml$addNode(j, close=FALSE)

    for (i in 1:nrow(df)) {
    xml$addNode("value", as.character(df[i, j]))
    }
    xml$closeTag()
  }
  xml$closeTag()
  ret <- gsub("\n", "", saveXML(xml$value(), indent = FALSE, prefix = NULL))
  return(ret)
}


#' hash data frame
#'
#' Hashes
#'
#' @param df "data.frame" to be hashed
#' @param algo "character" name of algorithm to be used as per "digest"
#' default is "sha512
#' @return \code{ret} "character" hash of the data.frame
#' @export
hash_data_frame <- function(df, algo  = "sha512"){

  txt_ret <- serialize_data_frame(df)

  hashed_df<- digest(txt_ret, algo = algo)

  hashed_df <- toupper(hashed_df)

  return(hashed_df)
}
