% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/common_daily_riskmodel_objectstore.r
\docType{methods}
\name{pushRiskModelComponent}
\alias{pushRiskModelComponent}
\alias{pushRiskModelComponent,DailyRiskModelObjectStore-method}
\title{Push component into risk model objectsotre}
\usage{
pushRiskModelComponent(object, data, name, lookback, component, force = FALSE)

\S4method{pushRiskModelComponent}{DailyRiskModelObjectStore}(object, data, name,
  lookback, component)
}
\arguments{
\item{object}{object of class "DailyRiskModelObjectStore"}

\item{data}{"data.frame" with data that will be appended to existing data}

\item{name}{"character" name of objectstore}

\item{lookback}{"integer" lookback value for model}

\item{component}{"character" name of the component of the risk model, possible values are:
c('ImpliedFactorReturns', 'ResidualReturns', 'Betas',
  'FactorCorrelation', 'FactorVariance', 'MarketStyle')}

\item{force}{"logical" if TRUE the existing values will be replaced if the same}
}
\value{
\code{rm} "DataSet" object with component if present otherwise NULL
}
\description{
Push component into risk model objectsotre
}
\section{Methods (by class)}{
\itemize{
\item \code{DailyRiskModelObjectStore}: Push component into risk model objectsotre
}}

