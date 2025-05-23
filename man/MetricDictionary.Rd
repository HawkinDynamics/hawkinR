% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MetricDictionary.R
\docType{data}
\name{MetricDictionary}
\alias{MetricDictionary}
\title{Metric Dictionary}
\format{
A data frame with 484 rows and 7 variables:
\describe{
\item{canonicalTestTypeId}{The unique id of the canonical test type (character).}
\item{testTypeName}{The given / common name of the test type (character).}
\item{id}{PascalCase formatted name of the metric (character).}
\item{label}{The metric label found in app and cloud UI (character).}
\item{label_unit}{The metric name returned from the API (character).}
\item{header}{header of data frame output (character).}
\item{units}{Metric unit of measure (character).}
\item{description}{A verbose description of the metric (character).}
}
}
\source{
Generated for demonstration purposes.
}
\usage{
MetricDictionary
}
\description{
This data frame contains metrics from all tests.
}
\keyword{datasets}
