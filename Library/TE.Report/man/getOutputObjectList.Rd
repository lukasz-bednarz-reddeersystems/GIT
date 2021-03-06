% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report_analysis_block.r
\docType{methods}
\name{getOutputObjectList}
\alias{getOutputObjectList}
\alias{getOutputObjectList,VirtualReportAnalysisBlock-method}
\title{Returns generated Output Reference Data objects list}
\usage{
getOutputObjectList(object)

\S4method{getOutputObjectList}{VirtualReportAnalysisBlock}(object)
}
\arguments{
\item{object}{object of class "VirtualReportAnalysisBlock"}
}
\value{
\code{output_list} list of objects of class derived from "VirtualReferenceData"
}
\description{
Returns Reference Data Objects list that possibly was
computed by analysis objects. If embeded Analysis modules
don't compute any output this slot will be NULL
}
\section{Methods (by class)}{
\itemize{
\item \code{VirtualReportAnalysisBlock}: Returns generated output object

Returns Reference Data Objects list that possibly was
computed by analysis objects. If embeded Analysis modules
don't compute any output this slot will be NULL
}}

