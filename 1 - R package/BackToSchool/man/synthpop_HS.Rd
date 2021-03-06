% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/abm6.R
\docType{data}
\name{synthpop_HS}
\alias{synthpop_HS}
\title{Synthetic Maryland high school population}
\format{
A data frame with
\describe{
\item{HH_id}{household ID}
\item{age}{age}
\item{flag_mult}{true if more than one child in the household, not used}
\item{id}{individual id #}
}
}
\source{
Wheaton, W.D., U.S. Synthetic Population 2010 Version 1.0 Quick Start Guide, RTI International, May 2014.
(\href{https://fred.publichealth.pitt.edu/syn_pops}{website}).  Created with script demographic_data2.R.
}
\usage{
data(synthMaryland_HS)
}
\description{
A data frame containing a synthetic population of children
ages 14-17, representative of the state of Maryland.
This is used by make_class() to sort children into classes.
}
\keyword{datasets}
