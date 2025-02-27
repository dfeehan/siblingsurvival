% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_sibship_visibility.R
\name{get_visibility}
\alias{get_visibility}
\title{calculate visibility for each sibship and ego}
\usage{
get_visibility(
  ego.dat,
  ego.id,
  sib.dat,
  sib.frame.indicator,
  weight = "wwgt",
  age = "age.cat"
)
}
\arguments{
\item{ego.dat}{The ego dataset (likely produced by \link{prep_dhs_sib_histories})}

\item{ego.id}{String with the name of the column in \code{sib.dat} containing the survey respondent ID}

\item{sib.dat}{The long-form sibling dataset (likely produced by \link{prep_dhs_sib_histories})}

\item{sib.frame.indicator}{String with the name of the column in \code{sib.dat} containing a 0/1 coded variable indicating whether or not each sib is in the frame population}

\item{weight}{string with the name of the column in \code{ego.dat} and \code{sib.dat} containing the sampling weight. Defaults to \code{wwgt}}

\item{age}{string with the name of the column in \code{ego.dat} containing the age group. Defaults to \code{age.cat}}
}
\value{
A list with two entries:
\itemize{
\item \code{ego_vis_agg} - a tibble with summarized adjustment factors
\item \code{ego_vis} - a tibble with one row per ego and the ego-specific visibilities
tibble with a row for each survey respondent (each unique value of \code{ego.id}),
and the number of sibs the respondent reported on the frame, including and not including herself
}
}
\description{
calculate visibility for each sibship and ego
}
\examples{
  # TODO write example code
}
