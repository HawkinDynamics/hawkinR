% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_groups.R
\name{get_groups}
\alias{get_groups}
\title{Get Groups}
\usage{
get_groups()
}
\value{
Response will be a data frame containing the groups that are in the organization.
Each group has the following variables:

\strong{id}   \emph{chr}   group's unique ID

\strong{name}   \emph{chr}   group's given name
}
\description{
Get the group names and ids for all the groups in the org.
}
\examples{
\dontrun{
# This is an example of how the function would be called.

df_groups <- get_groups()

}

}
