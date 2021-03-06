% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/keymap.r
\name{direct_keygen}
\alias{direct_keygen}
\title{Generate keys based on incoming data frame}
\usage{
direct_keygen(x, cols, fields)
}
\arguments{
\item{x}{data.frame with columns based on which the keys will be generated}

\item{cols, }{column names used to create keys}

\item{fields}{name of the columns}
}
\value{
\code{key_values} data.frame with generated keys
}
\description{
Directly translates a key value frame to a frame used by the
middleware by simply renaming the key value frame columns
to the supplied field name
}

