##' get ego X cell reports
##'
##' Produces a dataframe that has a row for each respondent X cell containing respondent's reported deaths and exposure among siblings in the cell.
##'
##' @param esc.dat Dataset with a row for each respondent X sibling X cell, likely produced by \code{\link{get_esc_reports}}
##' @param ego.id  String with the name of the column in \code{esc.dat} containing the survey respondent's id
##' @param sib.dat Dataset with a row for each reported sibling, likely produced by TODO
##' @param sib.frame.indicator String with the name of the column in \code{sib.dat} containing a 0/1 coded variable indicating whether or not each sib is in the frame population
##' @param cell.vars TODO
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

  esc.dat <- esc.dat %>% rename(.ego.id = !!sym(ego.id),
                                .sib.in.F = !!sym(sib.frame.indicator))

  sib.dat <- sib.dat %>% rename(.ego.weight = !!sym(weights),
                                .ego.id = !!sym(ego.id))

  if(discretize.exp) {
    stop("discretize.exp not yet implemented...")
  }

  ## ego reports about cells
  ##
  ## TODO - eventually add yprime quantities
  res <- esc.dat %>%
    group_by_at(c('.ego.id', cell.vars)) %>%
    summarize(y.Dcell = sum(sib.occ),
              y.Ncell = sum(sib.exp),
              y.NandFcell = sum(.sib.in.F * sib.exp)) %>%
    mutate(y.NandnotFcell = y.Ncell - y.NandFcell)

  # calculate y.F, which is closely related to the visibility of each sibship
  vdat <- get_sibship_visibility(sib.dat,
                                 ego.id='.ego.id',
                                 sib.frame.indicator='.sib.in.F')

  #vdat <- res %>%
  #  group_by(.ego.id) %>%
  #  summarize(y.F = sum(y.Fcell),
  #            y.NandF = sum(y.NandFcell))

  # join sampling weights onto sibship visibilities
  vdat <- vdat %>% left_join(sib.dat %>%
                               group_by(.ego.id) %>%
                               slice(1) %>%
                               select(.ego.id, .ego.weight), by='.ego.id')

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
