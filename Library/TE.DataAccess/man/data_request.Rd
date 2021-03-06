% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/common_dataplex.r
\name{data_request}
\alias{data_request}
\title{Request data from datastore}
\usage{
data_request(store, keys, variables)
}
\arguments{
\item{store}{character, store name.}

\item{keys}{data.frame store query keys}

\item{variables}{character vector of variables to pull from store}
}
\value{
\code{rval} data.frame, result of query
}
\description{
Global data access function
}

