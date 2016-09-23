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

hash_data_frame <- function(df){

  txt_ret <- serialize_data_frame(df)

  hashed_df<- digest(txt_ret, algo = "sha512")

  hashed_df <- toupper(hashed_df)

  return(hashed_df)
}
