% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{cpp_compute_occ_exp2}
\alias{cpp_compute_occ_exp2}
\title{cpp_compute_occ_exp}
\usage{
cpp_compute_occ_exp2(lambda, alpha, alpha_offset, tau, thresh = NA_real_)
}
\arguments{
\item{lambda}{a matrix of lifelines whose rows are units of observation (individuals),
and whose columns are (in order): start time, end time, event time}

\item{alpha}{a matrix whose rows are age groups and whose columns are
(in order): start time, end time (both starting from 0)}

\item{alpha_offset}{a vector with the birthdate of each unit of observation or,
more generally, the offset to use for the age groups in alpha}

\item{tau}{a matrix of time periods whose rows are units of observation (individuals),
and whose columns are (in order): start time, end time}

\item{thresh}{a threshold for discretizing exposure: when the amount of exposure meets or
exceeds this threshold, resulting exp is set to 1; if the amount of exposure is less than the threshold,
the resulting exp is set to 0}
}
\value{
a list containing two matrices, 'occ' and 'exp. each
matrix has one row for each unit of observation (individua) whose
columns are (for exp, in order): age group 1 exposure, ...,
last age group exposure; and (for occ, in order)
age group 1 number of events, ..., last age group number of events
}
\description{
compute occurrences and exposures from lifelines,
age groups, and time intervals
}
\details{
TODO - should write a more detailed description
}
