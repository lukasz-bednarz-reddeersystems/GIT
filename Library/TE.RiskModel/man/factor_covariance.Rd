% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/risk_model_functions.r
\name{factor_covariance}
\alias{factor_covariance}
\title{Factor covariance from correlation}
\usage{
factor_covariance(correlation, factor_stdev)
}
\arguments{
\item{correlation}{"data.frame" with factor correlation}

\item{factor_stdev}{"data.frame" with factor's standard deviation}
}
\value{
\code{correlation} "matrix" with factor correlation
}
\description{
Compute the factor covariance matrix by scaling the correlaiton matrix by the factor
returns standard deviation
}

