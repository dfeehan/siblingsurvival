##' Estimate death rates from sibling history data
##'
##' @param sib.dat The long-form sibling history dataset (likely produced by TODO)
##' @param ego.id  String with the name of the column of \code{sib.dat} that has the ID of the survey respondent
##' @param sib.id  String with the name of the column of \code{sib.dat} that has the sibling ID
##' @param sib.frame.indicator String with the name of the column in \code{sib.dat} containing a 0/1 coded variable indicating whether or not each sib is in the frame population
##' @param sib.sex String with the name fo the column of \code{sib.dat} that has the sibling's sex
##' @param cell.config An object containing the configuration of cells; see TODO for more information
##' @param discretize.exp Boolean for whether or not expsoure should be discretized. Not yet implemented.
##' @param method Which method to use - not yet implemented (currently we just return ind and agg, both without the respondent)
##' @return a list with two entries: \code{asdr.ind}, individual visibility asdr estimates; and \code{asdr.agg}, aggregate visibility asdr estimates
##'
##' @export
sibling_estimator <- function(sib.dat,
                              # the name of the id of the ego in the sibling histories
                              ego.id,
                              # the name of the id of the sib in the sibling histories
                              sib.id,
                              # the name of the indicator for whether or not each sib is on the
                              # frame
                              sib.frame.indicator,
                              # variable for sibling's sex
                              sib.sex = 'sex',
                              cell.config,
                              # by default, we report continuous exposure (ie, number of months of exposure)
                              # but the formal results are based on exposed/not exposed; use this setting to
                              # discretize exposure
                              discretize.exp=FALSE,
                              # either 'individual' or 'aggregate'
                              method='individual') {

  if (method != 'individual') {
    stop('Only the individual estimator is implemented at this point.')
  }

  sib.dat <- sib.dat %>%
    mutate(.ego.id = !!sym(ego.id),
           .sib.id = !!sym(sib.id),
           .sib.in.F = !!sym(sib.frame.indicator),
           .sib.sex = !!sym(sib.sex),
           .ego.weight = !!sym(weights))

  # get ego X sib X cell reports
  esc.dat <- get_esc_reports(sib.dat=sib.dat,
                             ego.id='.ego.id',
                             sib.id='.sib.id',
                             cell.config)

  # add covariates for the siblings
  esc.dat <- esc.dat %>%
    left_join(sib.dat %>% select(.sib.id,
                                 .ego.weight,
                                 .sib.in.F,
                                 .sib.sex), by='.sib.id')

  cell.vars <- c('time.period', '.sib.sex', 'agelabel')

  ec.dat <- get_ec_reports(esc.dat,
                           ego.id='.ego.id',
                           sib.dat=sib.dat,
                           sib.frame.indicator='.sib.in.F',
                           # TODO - eventually, perhaps these should be
                           # parameters and not hard-coded
                           cell.vars=cell.vars,
                           weights='.ego.weight')

  asdr.ind.dat <- ec.dat %>%
    mutate(ind.num.ego   = ifelse(y.F > 0,
                                  y.Dcell / (y.F + 1),
                                  0),
           ind.denom.ego = ifelse(y.F > 0,
                                  (y.NandFcell / y.F) + (y.NandnotFcell / (y.F + 1)),
                                  0)) %>%
    group_by_at(cell.vars) %>%
    summarize(num.hat = sum(.ego.weight * ind.num.ego),
              denom.hat = sum(.ego.weight * ind.denom.ego),
              ind.y.F = sum(.ego.weight * y.F),
              n = n(),
              wgt.sum = sum(.ego.weight)) %>%
    mutate(asdr.hat = num.hat / denom.hat,
           estimator='sib_ind')

  asdr.agg.dat <- ec.dat %>%
    group_by_at(cell.vars) %>%
    summarize(num.hat = sum(.ego.weight * y.Dcell),
              denom.hat = sum(.ego.weight * y.Ncell),
              n = n(),
              wgt.sum = sum(.ego.weight),
              asdr.hat = num.hat / denom.hat,
              estimator='sib_agg')

  asdr.ind.dat <- asdr.ind.dat %>%
    rename(!!sib.sex := .sib.sex,
           sib.age = agelabel)

  asdr.agg.dat <- asdr.agg.dat %>%
    rename(!!sib.sex := .sib.sex,
           sib.age = agelabel)

  return(list(asdr.ind=asdr.ind.dat,
              asdr.agg=asdr.agg.dat,
              ec.dat=ec.dat,
              esc.dat=esc.dat))
}
