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

    boot.weights <- boot.weights %>%
      dplyr::rename(.ego.id = !!sym(ego.id))


    ec.boot.dat <- ec.dat %>% left_join(boot.weights,
                                        by='.ego.id')

    boot.cols <- stringr::str_subset(colnames(boot.weights), ego.id, negate=TRUE)

    boot.ind.ests <- get_ind_est_from_ec(ec.boot.dat, boot.cols, cell.vars)
    boot.agg.ests <- get_agg_est_from_ec(ec.boot.dat, boot.cols, cell.vars)

    if (any(is.na(boot.ind.ests$asdr.hat))) {
      n.na <- sum(is.na(boot.ind.ests$asdr.hat))
      n.all <- length(boot.ind.ests$asdr.hat)
      warning(glue::glue("Individual estimates have {n.na} out of {n.all} values missing. These have been removed in the summary statistics. Beware!\n"))
    }

    # get estimated sampling uncertainty for the
    # individual and aggregate visibility estimates
    boot.ind.varest <- boot.ind.ests %>%
      ungroup() %>%
      group_by_at(cell.vars) %>%
      summarise(asdr.hat.ci.low = quantile(asdr.hat, .025, na.rm=TRUE),
                asdr.hat.ci.high = quantile(asdr.hat, 0.975, na.rm=TRUE),
                asdr.hat.median = quantile(asdr.hat, 0.5, na.rm=TRUE),
                asdr.hat.se = sd(asdr.hat, na.rm=TRUE))

    if (any(is.na(boot.agg.ests$asdr.hat))) {
      n.na <- sum(is.na(boot.agg.ests$asdr.hat))
      n.all <- length(boot.agg.ests$asdr.hat)
      warning(glue::glue("Aggregate estimates have {n.na} out of {n.all} values missing. These have been removed in the summary statistics. Beware!\n"))
    }


    boot.agg.varest <- boot.agg.ests %>%
      ungroup() %>%
      group_by_at(cell.vars) %>%
      summarise(asdr.hat.ci.low = quantile(asdr.hat, .025, na.rm=TRUE),
                asdr.hat.ci.high = quantile(asdr.hat, 0.975, na.rm=TRUE),
                asdr.hat.median = quantile(asdr.hat, 0.5, na.rm=TRUE),
                asdr.hat.se = sd(asdr.hat, na.rm=TRUE))

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

  ec.dat <- ec.dat %>%
    rename(!!sib.sex := .sib.sex,
           !!ego.id := .ego.id,
           sib.age = agelabel,
           !!weights := .ego.weight)

  esc.dat <- esc.dat %>%
    rename(!!sib.sex := .sib.sex,
           !!ego.id := .ego.id,
           !!sib.id := .sib.id,
           sib.age = agelabel,
           !!sib.frame.indicator := .sib.in.F,
           !!weights := .ego.weight)

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
##' @param wgt_var either a string with the name of the column that has sampling weights or a vector with the names of columns with bootstrap weights
##' @param cell_vars vector of strings with the names of variables to group by (the cells)
##' @return a tibble with the individual visibility ASDR estimates (not including the respondents' exposures)
get_ind_est_from_ec <- function(ec_dat, wgt_var, cell_vars) {

  res <- ec_dat %>%
    dplyr::mutate(ind.num.ego   = ifelse(y.F > 0,
                                         y.Dcell / (y.F + 1),
                                         0),
                  ind.denom.ego = ifelse(y.F > 0,
                                         (y.NandFcell / y.F) + (y.NandnotFcell / (y.F + 1)),
                                         0))

  weighted_sum <- function(x, w) { return(sum(x*w)) }

  res2 <- res %>%
    group_by_at(cell_vars) %>%
    summarize_at(.vars=wgt_var,
                 .funs=list(num.hat   = ~weighted_sum(x=.data[['ind.num.ego']], w=.),
                            denom.hat = ~weighted_sum(x=.data[['ind.denom.ego']], w=.),
                            ind.y.F   = ~weighted_sum(x=.data[['y.F']], w=.),
                            n         = ~n(),
                            wgt.sum   = ~weighted_sum(x=1, w=.)),
                 )

  ## if we have bootstrap weights, reshape and clean things up
  if(length(wgt_var) > 1) {
    res3 <- res2 %>%
      gather(starts_with('boot_weight'),
             key='rawqty',
             value='value') %>%
      mutate(qty = stringr::str_remove(rawqty, 'boot_weight_\\d+_'),
             boot_idx = as.integer(stringr::str_remove_all(rawqty, '[^\\d]'))) %>%
      select(-rawqty) %>%
      spread(qty, value)
  } else {
    res3 <- res2
  }

  res4 <- res3 %>%
    dplyr::mutate(asdr.hat = num.hat / denom.hat,
                  estimator='sib_ind')

  return(res4)
}

##' helper function for calculating aggregate visibility estimate from ego X cell data
##'
##' @param ec_dat the ego X cell data
##' @param wgt_var either a string with the name of the column that has sampling weights or a vector with the names of columns with bootstrap weights
##' @param cell_vars vector of strings with the names of variables to group by (the cells)
##' @return a tibble with the individual visibility ASDR estimates (not including the respondents' exposures)
get_agg_est_from_ec <- function(ec_dat, wgt_var, cell_vars) {

  weighted_sum <- function(x, w) { return(sum(x*w)) }

  res <- ec_dat %>%
    #dplyr::mutate(.cur.weight = !!sym(wgt_var)) %>%
    group_by_at(cell_vars) %>%
    summarize_at(.vars=wgt_var,
                 .funs=list(num.hat   = ~weighted_sum(x=.data[['y.Dcell']], w=.),
                            denom.hat = ~weighted_sum(x=.data[['y.Ncell']], w=.),
                            n         = ~n(),
                            wgt.sum   = ~weighted_sum(x=1, w=.)))

  ## if we have bootstrap weights, reshape and clean things up
  if(length(wgt_var) > 1) {
    res2 <- res %>%
      gather(starts_with('boot_weight'),
             key='rawqty',
             value='value') %>%
      mutate(qty = stringr::str_remove(rawqty, 'boot_weight_\\d+_'),
             boot_idx = as.integer(stringr::str_remove_all(rawqty, '[^\\d]'))) %>%
      select(-rawqty) %>%
      spread(qty, value)
  } else {
    res2 <- res
  }

  res3 <- res2 %>%
    mutate(asdr.hat = num.hat / denom.hat,
           estimator='sib_agg')

  return(res3)
}

## DEPRECATED - WILL EVENTUALLY BE CULLED

##' helper function for calculating individual visibility estimate from ego X cell data
##'
##' @param ec_dat the ego X cell data
##' @param wgt_var string with the name of the column that has sampling weights
##' @param cell_vars vector of strings with the names of variables to group by (the cells)
##' @return a tibble with the individual visibility ASDR estimates (not including the respondents' exposures)
get_ind_est_from_ec_OLD <- function(ec_dat, wgt_var, cell_vars) {
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
get_agg_est_from_ec_OLD <- function(ec_dat, wgt_var, cell_vars) {
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




