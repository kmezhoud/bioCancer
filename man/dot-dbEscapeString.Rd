% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AnnotationFuncs.R
\name{.dbEscapeString}
\alias{.dbEscapeString}
\title{Private Escape string}
\usage{
.dbEscapeString(str, raise.error = TRUE)
}
\arguments{
\item{str}{String to test}

\item{raise.error}{Logical, whether to raise an error or not.}
}
\value{
Invisible logical
}
\description{
Does not escape strings, but raises an error if any character expect normal letters and underscores are found in the string.
}
