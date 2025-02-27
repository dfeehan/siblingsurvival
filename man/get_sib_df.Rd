% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep_dhs_sib_histories.R
\name{get_sib_df}
\alias{get_sib_df}
\title{helper to prep the sib dataset}
\usage{
get_sib_df(ego.dat, sib.attrib, verbose = FALSE)
}
\arguments{
\item{ego.dat}{the prepped ego dataset}

\item{sib.attrib}{vector with sibling attribute columns (see \link{prep_dhs_sib_histories})}

\item{verbose}{see \link{prep_dhs_sib_histories}}
}
\value{
a prepped sibling dataset, used in \link{prep_dhs_sib_histories}
}
\description{
helper to prep the sib dataset
}
