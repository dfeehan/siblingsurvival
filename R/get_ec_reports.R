##' get ego X cell reports
##'
##' Produces a dataframe that has a row for each respondent X cell containing respondent's reported deaths and exposure among siblings in the cell.
##'
##' @param esc.dat Dataset with a row for each respondent X sibling X cell, likely produced by \code{\link{get_esc_reports}}
##' @param ego.id  String with the name of the column in \code{esc.dat} containing the survey respondent's id
##' @param sib.dat Dataset with a row for each reported sibling, likely produced by TODO
##' @param sib.frame.indicator String with the name of the column in \code{sib.dat} containing a 0/1 coded variable indicating whether or not each sib is in the frame population
##' @param cell.vars a vector with Strings containing the names of columns in \code{esc.dat} identifying the cells to group reports by (typically age group, sex, time period)
##' @param weights String with the name of the column in \code{esc.dat} that has the survey weights
##' @param discretize.exp Boolean for whether or not expsoure should be discretized. Not yet implemented.
##' @return A dataframe that has a row for each respondent X cell containing respondent's reported deaths and exposure among siblings in the cell.
##' @examples
##'   # TODO - add example
get_ec_reports <- function(esc.dat,
                           ego.id,
                           sib.dat,
                           sib.frame.indicator,
                           cell.vars,
                           # the survey weight for each respondent
                           weights,
                           discretize.exp = FALSE) {

  esc.dat <- esc.dat %>% dplyr::rename(.ego.id = !!sym(ego.id),
                                       .sib.in.F = !!sym(sib.frame.indicator))

  sib.dat <- sib.dat %>% dplyr::rename(.ego.weight = !!sym(weights),
                                .ego.id = !!sym(ego.id))

  if(discretize.exp) {
    stop("discretize.exp not yet implemented...")
  }

  # calculate y.F, which is closely related to the visibility of each sibship
  vdat <- get_sibship_visibility(sib.dat,
                                 ego.id='.ego.id',
                                 sib.frame.indicator='.sib.in.F')

  # add individual visibility weights to the esc data
  # (these are sibling individual visibility weights)
  esc.dat.with.indviswgt <- esc.dat %>%
    left_join(vdat %>% select(.ego.id, y.F),
              by='.ego.id') %>%
    # individual visibility weight depends on whether the sib is on the frame
    # if yes, then individual vis weight is y.F.
    # if no (including if sib is dead), it is y.F + 1
    mutate(ind_vis_weight = case_when(.sib.in.F == 1 ~ 1 / y.F,
                                      .sib.in.F == 0 ~ 1 / (y.F + 1)))

  if (any(is.na(esc.dat.with.indviswgt$ind_vis_weight))) {
    stop("esc data has rows for which we have no individual visibility weight. something must be wrong. is there any missingness in the indicator variable for sibling frame membership?")
  }

  ## ego reports about cells
  ##
  ## TODO - eventually add yprime quantities
  res <- esc.dat.with.indviswgt %>%
    group_by_at(c('.ego.id', cell.vars)) %>%
    summarize(# for aggregate vis
              y.Dcell = sum(sib.occ),
              y.Ncell = sum(sib.exp),
              y.NandFcell = sum(.sib.in.F * sib.exp),
              # for individual vis (use individual vis weights)
              y.Dcell.ind = sum(sib.occ*ind_vis_weight),
              y.Ncell.ind = sum(sib.exp*ind_vis_weight)
              ) %>%
    mutate(y.NandnotFcell = y.Ncell - y.NandFcell)

  ## res now has a row for each ego X cell
  ## we want to add sampling weight info to this dataset

  # join sampling weights onto sibship visibilities
  vdat <- vdat %>% left_join(sib.dat %>%
                               group_by(.ego.id) %>%
                               slice(1) %>%
                               select(.ego.id, .ego.weight), by='.ego.id')

  ## TODO - LEFT OFF HERE
  ##   ... figuring this out; needed for IC checks...
  ## TODO - not yet calculating y.Fcell and y.Fnotcell
  ##        think this through once we get the other stuff working
  res <- res %>%
    left_join(vdat, by='.ego.id')
  #%>%
  #mutate(#y.Fnotcell = y.F - y.Fcell,
  #       y.NandnotFcell = y.Ncell - y.NandFcell)

  res <- res %>% rename(!!ego.id := .ego.id,
                        !!weights := .ego.weight)

  return(res)
}
