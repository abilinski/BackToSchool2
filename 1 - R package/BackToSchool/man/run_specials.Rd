% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/abm6.R
\name{run_specials}
\alias{run_specials}
\title{Set specials transmission}
\usage{
run_specials(a, df, specials)
}
\arguments{
\item{a}{id of infected individual}

\item{df}{school data frame from make_school()}

\item{specials}{classroom and teacher ids of specials at time t}
}
\value{
infs id of infected individuals
}
\description{
Determine who is infected at a timestep
from specials
}
