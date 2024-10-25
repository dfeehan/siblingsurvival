##' calculate summary statistics for siblings in a given time window
##'
##' @param sib.dat The long-form sibling dataset (likely produced by \link{\code{prep_dhs_sib_histories}})
##' @param ego.id  String with the name of the column in \code{sib.dat} containing the survey respondent ID
##' @param sib.id  String with the name of the column of \code{sib.dat} that has the sibling ID
##' @param sib.frame.indicator String with the name of the column in \code{sib.dat} containing a 0/1 coded variable indicating whether or not each sib is in the frame population
##' @return A one-row tibble with two columns: \code{num_sibs} has the number of sibs who contribute exposure in the time window; and \code{num_deaths}, the number of reported sibling deaths in the time window
sibling_summ <- function(sib.dat,
                         # the name of the id of the ego in the sibling histories
                         ego.id,
                         # the name of the id of the sib in the sibling histories
                         sib.id,
                         # the name of the indicator for whether or not each sib is on the
                         # frame
                         sib.frame.indicator,
                         cell.config) {

  sib.dat <- sib.dat %>%
    dplyr::mutate(.ego.id     = !!sym(ego.id),
                  .sib.id     = !!sym(sib.id))

  esc_dat <- get_esc_reports(sib.dat = sib.dat,
                             ego.id='.ego.id',
                             sib.id='.sib.id',
                             cell.config=cell.config)

  sib_summ <- esc_dat %>%
    filter(sib.exp > 0 | sib.occ > 0)

  ## may eventually summarize more features...
  return(tibble(num_sibs = length(unique(sib_summ$.sib.id)),
                num_deaths = sum(sib_summ$sib.occ)))

}
