##' Estimate death rates from sibling history data
##'
##' @param sib.dat The long-form sibling history dataset (likely produced by [prep_dhs_sib_histories])
##' @param ego.id  String with the name of the column of \code{sib.dat} that has the ID of the survey respondent
##' @param sib.id  String with the name of the column of \code{sib.dat} that has the sibling ID. Defaults to \code{'sibid'}, which is the column created by \code{\link{prep_dhs_sib_histories}} and \code{\link{prep_nrsim_sib_histories}}.
##' @param sib.frame.indicator String with the name of the column in \code{sib.dat} containing a 0/1 coded variable indicating whether or not each sib is in the frame population
##' @param sib.sex String with the name fo the column of \code{sib.dat} that has the sibling's sex
##' @param cell.config An object containing the configuration of cells; see TODO for more information
##' @param weights String with the name of the column of \code{sib.dat} that has the sampling weight
##' @param boot.weights Optional dataframe with bootstrap resampled weights. See Details for more info.
##' @param return.boot If TRUE, and if \code{boot.weights} is specified, then return each bootstrap estimate
##' @param visibility A visibility rule saying how each reported sibling's
##'        visibility is derived. Defaults to
##'        [networkreporting::vis_from_clique()], the exact rule this function
##'        has always applied, so the default changes nothing. See
##'        [networkreporting::vis_from_donor()] and
##'        [networkreporting::vis_coalesce()] for the approximating rules that
##'        non-clique ties need.
##' @param tie What kind of tie the reports are about, as a
##'        [networkreporting::tie_config()]. Defaults to
##'        `tie_config("clique", name = "siblings")`, which is what siblings
##'        are, so the default changes nothing.
##'
##'        **Set this if you are using this function for a tie that is not a
##'        clique.** It is used for maternal cousins and cousins-plus-siblings
##'        in the socsim work, and cousinship is not transitive, so the default
##'        clique rule silently overstates visibility there --- by 1.55x for
##'        off-frame alters against 1.29x for on-frame ones, measured against
##'        socsim ground truth. Because a death is always off-frame while
##'        exposure is a mixture, that differential biases the rate rather than
##'        cancelling. Declaring `tie_config("group")` makes the clique rule
##'        refuse rather than mislead.
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
                              # the name of the id of the sib in the sibling histories;
                              # 'sibid' is what the prep_*_sib_histories functions create
                              sib.id = 'sibid',
                              # the name of the indicator for whether or not each sib is on the
                              # frame
                              sib.frame.indicator,
                              # variable for sibling's sex
                              sib.sex = 'sex',
                              cell.config,
                              weights,
                              boot.weights = NULL,
                              return.boot = FALSE,
                              # how each reported sibling's visibility is derived.
                              # the default is the exact clique rule, which is what
                              # this function has always used
                              visibility = networkreporting::vis_from_clique(),
                              # what kind of tie these reports are about. siblings are
                              # a clique, which is why the clique rule is exact here.
                              # override for cousins and other non-clique ties.
                              tie = networkreporting::tie_config('clique',
                                                                 name = 'siblings'),
                              # by default, we report continuous exposure (ie, number of months of exposure)
                              # but the formal results are based on exposed/not exposed; use this setting to
                              # discretize exposure
                              discretize.exp=FALSE) {

  ## check up front that the columns we were given actually exist, so that a
  ## mismatched name (eg sib.id='sib.id' when prep created 'sibid') produces a
  ## message that names the columns available rather than an opaque tidyselect error
  requested.cols <- c(ego.id=ego.id,
                      sib.id=sib.id,
                      sib.frame.indicator=sib.frame.indicator,
                      sib.sex=sib.sex,
                      weights=weights)
  missing.cols <- requested.cols[! requested.cols %in% names(sib.dat)]

  if (length(missing.cols) > 0) {
    stop(glue::glue(
      "Column(s) requested but not found in sib.dat: ",
      "{paste0(names(missing.cols), \"='\", missing.cols, \"'\", collapse=', ')}.\n",
      "sib.dat has columns: {paste0(names(sib.dat), collapse=', ')}\n"))
  }

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

  ## Apply the visibility rule. The default, vis_from_clique(), reproduces the
  ## previous hardcoded behaviour exactly -- 1/y.F on frame, 1/(y.F + 1) off it
  ## -- so nothing about existing estimates moves. Passing another rule is what
  ## makes visibility a declared modelling choice rather than an assumption
  ## buried in the estimator.
  vis.res <- networkreporting::apply_visibility_rule(
    rule            = visibility,
    esc.dat         = esc.dat,
    sib.dat         = sib.dat,
    ego.id          = '.ego.id',
    frame.indicator = '.sib.in.F',
    weights         = '.ego.weight',
    tie             = tie)

  ## esc.dat comes back with y.F attached, which get_ec_reports() reads
  esc.dat <- vis.res$data
  ## `ind_vis` is the visibility WEIGHT (the reciprocal of the count), which is
  ## what get_ec_reports() consumes
  esc.dat$ind_vis <- vis.res$values$vis_weight

  if (any(is.na(esc.dat$ind_vis))) {
    n.na <- sum(is.na(esc.dat$ind_vis))
    stop(glue::glue(
      "The visibility rule '{visibility$label}' left {n.na} of {nrow(esc.dat)} ",
      "report(s) without a visibility.\n",
      "For the clique rule this points at missingness in the frame indicator. ",
      "For an approximating rule it usually means some alters have no donor ",
      "cell; wrap the rule in vis_coalesce() with a coarser fallback tier."))
  }

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

    ## For an estimated visibility rule, the group size moves with the
    ## replicate, so it has to be refit inside the loop rather than frozen.
    ## For vis_from_clique() this is NULL and nothing changes -- which is what
    ## makes the change safe to land: the clique CIs must not move.
    vis.refit <- networkreporting::make_vis_refit(
      rule         = visibility,
      donor.dat    = vis.res$donor.dat,
      boot.weights = boot.weights,
      ec.dat       = ec.dat,
      ego.id       = '.ego.id')

    boot.ind.ests <- get_boot_ests_matrix(ec.dat, boot.weights, '.ego.id', cell.vars, 'ind',
                                          visibility = visibility, refit = vis.refit)
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
      group_by(across(all_of(cell.vars))) %>%
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
      group_by(across(all_of(cell.vars))) %>%
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

  ## Provenance travels with the estimate: which rule produced it, how many
  ## alters each tier resolved, and what share of the deaths and of the exposure
  ## were approximated. Attached rather than added as a column so that nothing
  ## downstream that indexes res by name is disturbed.
  attr(res, "vis_provenance") <- vis.res$provenance
  res$vis_provenance <- vis.res$provenance

  return(res)
}
