##' Estimate death rates from sibling history data
##'
##' @param sib.dat The long-form sibling history dataset (likely produced by TODO)
##' @param ego.id  String with the name of the column of \code{sib.dat} that has the ID of the survey respondent
##' @param sib.id  String with the name of the column of \code{sib.dat} that has the sibling ID
##' @param sib.frame.indicator String with the name of the column in \code{sib.dat} containing a 0/1 coded variable indicating whether or not each sib is in the frame population
##' @param sib.sex String with the name fo the column of \code{sib.dat} that has the sibling's sex
##' @param cell.config An object containing the configuration of cells; see TODO for more information
##' @param weights String with the name of the column of \code{sib.dat} that has the sampling weight
##' @param boot.weights Optional dataframe with bootstrap resampled weights. See Details for more info.
##' @param return.boot If TRUE, and if \code{boot.weights} is specified, then return each bootstrap estimate
##' @param discretize.exp Boolean for whether or not expsoure should be discretized. Not yet implemented.
##' @return a list with two entries: \code{asdr.ind}, individual visibility asdr estimates; and \code{asdr.agg}, aggregate visibility asdr estimates
##'
##' @section Details:
##' If you want estimated sampling variances, you can pass in a data frame \code{boot.weights}.
##' \code{boot.weights} is assumed to have a column that is named whatever the \code{ego.id} is,
##' and then a series of columns named \code{boot_weight_1}, ..., \code{boot_weight_M}.
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
                              weights,
                              boot.weights = NULL,
                              return.boot = FALSE,
                              # by default, we report continuous exposure (ie, number of months of exposure)
                              # but the formal results are based on exposed/not exposed; use this setting to
                              # discretize exposure
                              discretize.exp=FALSE) {

  sib.dat <- sib.dat %>%
    dplyr::mutate(.ego.id     = !!sym(ego.id),
                  .sib.id     = !!sym(sib.id),
                  .sib.in.F   = !!sym(sib.frame.indicator),
                  .sib.sex    = !!sym(sib.sex),
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

  asdr.ind.dat <- get_ind_est_from_ec(ec.dat, '.ego.weight', cell.vars)
  asdr.agg.dat <- get_agg_est_from_ec(ec.dat, '.ego.weight', cell.vars)

  ## if we want sampling variances...
  if (! is.null(boot.weights)) {
    M <- ncol(boot.weights) - 1

    ec.boot.dat <- ec.dat %>% left_join(boot.weights %>%
                                          dplyr::mutate(.ego.id = !!sym(ego.id)),
                                        by='.ego.id')

    # for each bootstrap rep,
    # calculate individual visibility estimate
    # and aggregate visibility estimate
    boot.ests <- map(1:M,
                     function(idx) {

                       cur.wgt <- paste0('boot_weight_', idx)

                       cur.ind.est <- get_ind_est_from_ec(ec.boot.dat, cur.wgt, cell.vars)
                       cur.ind.est <- cur.ind.est %>% dplyr::mutate(boot_idx = idx)

                       cur.agg.est <- get_agg_est_from_ec(ec.boot.dat, cur.wgt, cell.vars)
                       cur.agg.est <- cur.agg.est %>% dplyr::mutate(boot_idx = idx)

                       return(list(agg=cur.agg.est, ind=cur.ind.est))

                     })

    boot.ind.ests <- bind_rows(map(boot.ests, ~.x$ind))
    boot.agg.ests <- bind_rows(map(boot.ests, ~.x$agg))

    # get estimated sampling uncertainty for the
    # individual and aggregate visibility estimates
    boot.ind.varest <- boot.ind.ests %>%
      ungroup() %>%
      group_by_at(cell.vars) %>%
      summarise(asdr.hat.ci.low = quantile(asdr.hat, .025),
                asdr.hat.ci.high = quantile(asdr.hat, 0.975),
                asdr.hat.median = quantile(asdr.hat, 0.5),
                asdr.hat.se = sd(asdr.hat))

    boot.agg.varest <- boot.agg.ests %>%
      ungroup() %>%
      group_by_at(cell.vars) %>%
      summarise(asdr.hat.ci.low = quantile(asdr.hat, .025),
                asdr.hat.ci.high = quantile(asdr.hat, 0.975),
                asdr.hat.median = quantile(asdr.hat, 0.5),
                asdr.hat.se = sd(asdr.hat))

    # and join the estimated sampling uncertainty onto the returned asdrs
    asdr.ind.dat <- asdr.ind.dat %>%
      left_join(boot.ind.varest, by=cell.vars)

    asdr.agg.dat <- asdr.agg.dat %>%
      left_join(boot.agg.varest, by=cell.vars)

  }



  asdr.ind.dat <- asdr.ind.dat %>%
    rename(!!sib.sex := .sib.sex,
           sib.age = agelabel)

  asdr.agg.dat <- asdr.agg.dat %>%
    rename(!!sib.sex := .sib.sex,
           sib.age = agelabel)

  res <- list(asdr.ind=asdr.ind.dat,
              asdr.agg=asdr.agg.dat,
              ec.dat=ec.dat,
              esc.dat=esc.dat)

  # if the user wants us to return all of the bootstrap estimates
  # (instead of just the summaries), add them to the results list
  if (! is.null(boot.weights)) {
    if(return.boot) {
      res$boot.asdr.ind <- boot.ind.ests
      res$boot.asdr.agg <- boot.agg.ests
    }

  }

  return(res)
}

##' helper function for calculating individual visibility estimate from ego X cell data
##'
##' @param ec_dat the ego X cell data
##' @param wgt_var string with the name of the column that has sampling weights
##' @param cell_vars vector of strings with the names of variables to group by (the cells)
##' @return a tibble with the individual visibility ASDR estimates (not including the respondents' exposures)
get_ind_est_from_ec <- function(ec_dat, wgt_var, cell_vars) {
  res <- ec_dat %>%
    dplyr::mutate(.cur.weight = !!sym(wgt_var)) %>%
    dplyr::mutate(ind.num.ego   = ifelse(y.F > 0,
                                  y.Dcell / (y.F + 1),
                                  0),
           ind.denom.ego = ifelse(y.F > 0,
                                  (y.NandFcell / y.F) + (y.NandnotFcell / (y.F + 1)),
                                  0)) %>%
    group_by_at(cell_vars) %>%
    summarize(num.hat = sum(.cur.weight * ind.num.ego),
              denom.hat = sum(.cur.weight * ind.denom.ego),
              ind.y.F = sum(.cur.weight * y.F),
              n = n(),
              wgt.sum = sum(.cur.weight)) %>%
    dplyr::mutate(asdr.hat = num.hat / denom.hat,
           estimator='sib_ind')

  return(res)
}

##' helper function for calculating aggregate visibility estimate from ego X cell data
##'
##' @param ec_dat the ego X cell data
##' @param wgt_var string with the name of the column that has sampling weights
##' @param cell_vars vector of strings with the names of variables to group by (the cells)
##' @return a tibble with the individual visibility ASDR estimates (not including the respondents' exposures)
get_agg_est_from_ec <- function(ec_dat, wgt_var, cell_vars) {
  res <- ec_dat %>%
    dplyr::mutate(.cur.weight = !!sym(wgt_var)) %>%
    group_by_at(cell_vars) %>%
    summarize(num.hat = sum(.cur.weight * y.Dcell),
              denom.hat = sum(.cur.weight * y.Ncell),
              n = n(),
              wgt.sum = sum(.cur.weight),
              asdr.hat = num.hat / denom.hat,
              estimator='sib_agg')

  return(res)
}
