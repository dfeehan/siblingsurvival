##' Estimate death rates from sibling history data
##'
##' @param sib.dat The long-form sibling history dataset (likely produced by \link{\code{prep_dhs_sib_histories}})
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
    left_join(sib.dat %>% select(.ego.id,
                                 .sib.id,
                                 .ego.weight,
                                 .sib.in.F,
                                 .sib.sex),
              by=c('.ego.id', '.sib.id'))

  cell.vars <- c('time.period', '.sib.sex', 'agelabel', cell.config$covars)

  # add individual visibility weights for the siblings
  esc.dat <- esc.dat %>%
    add_esc_ind_vis(ego.id='.ego.id',
                    sib.dat,
                    sib.frame.indicator='.sib.in.F',
                    # column name for individual visibility
                    varname='ind_vis')

  ## TODO - I think this line sometimes causes a warning
  ## "Column `.ego.id` has different attributes on LHS and RHS of join"
  ec.dat <- get_ec_reports(esc.dat,
                           ego.id='.ego.id',
                           sib.dat=sib.dat,
                           sib.frame.indicator='.sib.in.F',
                           # TODO - eventually, perhaps these should be
                           # parameters and not hard-coded
                           cell.vars=cell.vars,
                           weights='.ego.weight',
                           ind.vis.var='ind_vis')

  asdr.ind.dat <- get_ind_est_from_ec(ec.dat, '.ego.weight', cell.vars)
  asdr.agg.dat <- get_agg_est_from_ec(ec.dat, '.ego.weight', cell.vars)

  ## if we want sampling variances...
  if (! is.null(boot.weights)) {
    M <- ncol(boot.weights) - 1

    boot.weights <- boot.weights %>%
      dplyr::rename(.ego.id = !!sym(ego.id))

    boot.ind.ests <- get_boot_ests_matrix(ec.dat, boot.weights, '.ego.id', cell.vars, 'ind')
    boot.agg.ests <- get_boot_ests_matrix(ec.dat, boot.weights, '.ego.id', cell.vars, 'agg')

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

  if(! is.null(cell.config$event.name)) {
    asdr.ind.dat$event.name <- cell.config$event.name
    asdr.agg.dat$event.name <- cell.config$event.name
    ec.dat$event.name <- cell.config$event.name
    esc.dat$event.name <- cell.config$event.name
  }

  res <- list(asdr.ind=asdr.ind.dat,
              asdr.agg=asdr.agg.dat,
              ec.dat=ec.dat,
              esc.dat=esc.dat)

  # if the user wants us to return all of the bootstrap estimates
  # (instead of just the summaries), add them to the results list
  if (! is.null(boot.weights)) {
    if(return.boot) {

      boot.ind.ests <- boot.ind.ests %>%
        rename(!!sib.sex := .sib.sex,
               sib.age = agelabel)

      boot.agg.ests <- boot.agg.ests %>%
        rename(!!sib.sex := .sib.sex,
               sib.age = agelabel)

      if(! is.null(cell.config$event.name)) {
        boot.ind.ests$event.name <- cell.config$event.name
        boot.agg.ests$event.name <- cell.config$event.name
      }

      res$boot.asdr.ind <- boot.ind.ests
      res$boot.asdr.agg <- boot.agg.ests

    }

  }

  return(res)
}

##' Fast bootstrap estimation using matrix multiplication
##'
##' Replaces the wide-dataframe summarize_at + gather/spread approach with direct
##' matrix multiplication. For each cell, computes weighted sums across all M bootstrap
##' replicates simultaneously using BLAS routines, avoiding the creation of 10k-column
##' intermediate dataframes.
##'
##' @param ec_dat ego X cell data from get_ec_reports()
##' @param boot_weights_df dataframe with .ego.id column and boot_weight_1..M columns
##' @param ego_id_col name of the ego id column in ec_dat and boot_weights_df
##' @param cell_vars vector of column names defining cells (age, sex, time period, etc)
##' @param estimator_type either 'ind' (individual visibility) or 'agg' (aggregate visibility)
##' @return long-form data frame with one row per cell per bootstrap replicate
get_boot_ests_matrix <- function(ec_dat, boot_weights_df, ego_id_col, cell_vars, estimator_type) {

  # Build boot weight matrix: rows = respondents, cols = bootstrap replicates
  boot_col_names <- stringr::str_subset(colnames(boot_weights_df), 'ego.id', negate = TRUE)
  boot_mat <- as.matrix(boot_weights_df[, boot_col_names, drop = FALSE])
  boot_ego_ids <- boot_weights_df[[ego_id_col]]
  M <- ncol(boot_mat)

  # Split ec_dat by cell for vectorized operations within each cell
  cell_groups <- ec_dat %>% dplyr::group_by_at(cell_vars) %>% dplyr::group_split()
  cell_keys   <- ec_dat %>% dplyr::group_by_at(cell_vars) %>% dplyr::group_keys()

  purrr::map2_dfr(cell_groups, seq_len(nrow(cell_keys)), function(grp, i) {
    # Match respondents in this cell to rows in the boot weight matrix
    row_idx <- match(grp[[ego_id_col]], boot_ego_ids)

    # Rows from boot_mat corresponding to respondents in this cell
    W <- boot_mat[row_idx, , drop = FALSE]  # N_cell x M

    # Select numerator and denominator vectors based on estimator type
    if (estimator_type == 'ind') {
      num_vec   <- grp$y.Dcell.ind
      denom_vec <- grp$y.Ncell.ind
    } else {
      num_vec   <- grp$y.Dcell
      denom_vec <- grp$y.Ncell
    }

    # Matrix multiply: length-N_cell vector %*% N_cell x M matrix = length-M vector
    # This uses BLAS and runs in milliseconds even for large M
    num.hat   <- as.vector(num_vec   %*% W)
    denom.hat <- as.vector(denom_vec %*% W)

    estimator_label <- if (estimator_type == 'ind') 'sib_ind' else 'sib_agg'

    data.frame(
      cell_keys[rep(i, M), , drop = FALSE],
      boot_idx  = seq_len(M),
      num.hat   = num.hat,
      denom.hat = denom.hat,
      asdr.hat  = num.hat / denom.hat,
      estimator = estimator_label,
      stringsAsFactors = FALSE
    )
  })
}

##' helper function for calculating individual visibility estimate from ego X cell data
##'
##' @param ec_dat the ego X cell data
##' @param wgt_var either a string with the name of the column that has sampling weights or a vector with the names of columns with bootstrap weights
##' @param cell_vars vector of strings with the names of variables to group by (the cells)
##' @return a tibble with the individual visibility ASDR estimates (not including the respondents' exposures)
get_ind_est_from_ec <- function(ec_dat, wgt_var, cell_vars) {

  res <- ec_dat %>%
    dplyr::mutate(ind.num.ego   = y.Dcell.ind,
                  ind.denom.ego = y.Ncell.ind)

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
      tidyr::pivot_longer(cols = tidyselect::starts_with('boot_weight'),
                          names_to = 'rawqty',
                          values_to = 'value') %>%
      mutate(qty = stringr::str_remove(rawqty, 'boot_weight_\\d+_'),
             boot_idx = as.integer(stringr::str_remove_all(rawqty, '[^\\d]'))) %>%
      select(-rawqty) %>%
      tidyr::pivot_wider(names_from = qty, values_from = value)
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
      tidyr::pivot_longer(cols = tidyselect::starts_with('boot_weight'),
                          names_to = 'rawqty',
                          values_to = 'value') %>%
      mutate(qty = stringr::str_remove(rawqty, 'boot_weight_\\d+_'),
             boot_idx = as.integer(stringr::str_remove_all(rawqty, '[^\\d]'))) %>%
      select(-rawqty) %>%
      tidyr::pivot_wider(names_from = qty, values_from = value)
  } else {
    res2 <- res
  }

  res3 <- res2 %>%
    mutate(asdr.hat = num.hat / denom.hat,
           estimator='sib_agg')

  return(res3)
}



