##' warn when a sibling sex has no matching respondents
##'
##' Called from [siblingsurvival::aggregate_maternal_estimates] when
##' `only_females = FALSE`. In most sibling history surveys only women are
##' interviewed, so there is no respondent age distribution and no visibility
##' adjustment for male siblings, and their estimates come out `NA`. That is the
##' honest answer -- you cannot estimate a visibility adjustment for a sex that
##' was never interviewed -- but it should not be silent.
##'
##' @param res the joined estimate dataframe
##' @param ego.dat the prepped ego data, used to report which sexes were interviewed
##' @return `res`, unchanged; called for the warning
##'
warn_uninterviewed_sex <- function(res, ego.dat) {

  unmatched <- res %>%
    filter(is.na(agegrp_prop) | is.na(adj.factor)) %>%
    pull(sib.sex) %>%
    unique()

  if (length(unmatched) > 0) {

    ego.sexes <- sort(unique(as.character(ego.dat$sex)))

    warning(glue::glue(
      "No respondent information for sibling sex(es): ",
      "{paste0(sort(unmatched), collapse=', ')}. ",
      "The respondents in ego.dat are: {paste0(ego.sexes, collapse=', ')}. ",
      "Estimates for those siblings will be NA -- a reference age distribution ",
      "and a visibility adjustment can only come from respondents of the same ",
      "sex, and that sex was not interviewed."))
  }

  invisible(res)
}

##' calculate total rate based on point estimates
##'
##' @param estimates the output of [siblingsurvival::sibling_estimator]
##' @param ego.dat the prepped ego data
##' @param sib.dat the prepped sibling data
##' @param only_females only keep female estimates? Defaults to TRUE; see Details, below
##' @param age_prop optional, the respondent age distribution from
##'        [siblingsurvival::get_ego_age_distn]. Computed internally when `NULL`
##'        (the default); pass it in to avoid recomputing it; see Details
##' @param vis_res optional, the visibility results from
##'        [siblingsurvival::get_visibility]. Computed internally when `NULL`
##'        (the default); pass it in to avoid recomputing it; see Details
##'
##' @returns either a tibble with the estimates aggregated across age groups
##' OR, if there are bootstrap results, then a list with three entries:
##'   * `point` - the point estimates (should be the same as running w/out bootstraps)
##'   * `boot_summ` - estimates w/ confidence intervals calculated from bootstrap reps
##'   * `boot` - results for each bootstrap rep
##'
##' @section Details:
##'
##' This function aggregates age-specific maternal mortality quantities
##' like the maternal death rate or the pregnancy-related death rate.
##' It uses the age distribution of the survey respondents as the reference
##' population.
##'
##' Note that, by default, this will only consider estimates for females aged 15-49,
##' as this is what makes sense for aggregating maternal estimates.
##' It might occasionally be useful to run this function for all-cause mortality;
##' in that case, setting the parameter `only_female=FALSE` will include
##' male estimates, too (but will still restrict to ages 15-49)
##'
##' `age_prop` and `vis_res` are computed from `ego.dat` and `sib.dat` when they
##' are not supplied, which is the usual case. They are exposed because callers
##' frequently need them for their own age-specific output, and frequently call
##' this function more than once per survey (for example, once for all-cause and
##' once for pregnancy-related mortality). Computing them once and passing them
##' in avoids repeating identical work; it does not change any result.
##'
##' @export
##' @md
aggregate_maternal_estimates <- function(estimates,
                                         ego.dat,
                                         sib.dat,
                                         only_females = TRUE,
                                         age_prop = NULL,
                                         vis_res = NULL) {


  # get age distribution of respondents
  if (is.null(age_prop)) {
    age_prop <- get_ego_age_distn(ego.dat,
                                  only_females)
  }

  if (is.null(vis_res)) {
    vis_res <- get_visibility(ego.dat,
                              ego.id='caseid',
                              sib.dat,
                              sib.frame.indicator='in.F')
  }

  ## assumption:
  ##   estimates$asdr.ind and estimates$asdr.agg
  ##   have columns called sib.sex, sib.age, time.period, and asdr.hat

  res <- estimates$asdr.ind %>%
    select(time.period, sib.sex, sib.age,
           # if we specified 'event.name' in the cell.config, keep it...
           any_of('event.name'),
           asdr.hat.ind = asdr.hat) %>%
    left_join(estimates$asdr.agg %>%
                select(time.period, sib.sex, sib.age,
                       any_of('event.name'),
                       asdr.hat.agg = asdr.hat)) %>%
    # only need ages 15-49
    filter(sib.age %in% reproductive_age_groups())

  if (only_females) {

    res <- res %>%
      # only need female estimates
      filter(sib.sex  == 'f') %>%
      left_join(age_prop,
                by=c('sib.age'='age.cat')) %>%
      ## NB: keying on sex as well as age matters when ego_vis_agg has both
      ## sexes -- joining on age alone duplicates every row once per sex.
      ## The bootstrap branch below has to use the identical key.
      left_join(vis_res$ego_vis_agg,
                by=c('sib.age'='age.cat',
                     'sib.sex'='sex')) %>%
      mutate(dummy=1) %>%
      group_by(dummy)

  } else {

    ## both joins key on sex as well as age: each sibling sex is weighted by
    ## its own respondents' age structure, and adjusted by its own respondents'
    ## visibility. Note the join consumes the `sex` column from each of the two
    ## right-hand tables, so `sib.sex` is what survives to group by.
    res <- res %>%
      left_join(age_prop,
                by=c('sib.age'='age.cat',
                     'sib.sex'='sex')) %>%
      left_join(vis_res$ego_vis_agg,
                by=c('sib.age'='age.cat',
                     'sib.sex'='sex')) %>%
      mutate(dummy=1) %>%
      group_by(dummy, sib.sex)

    warn_uninterviewed_sex(res, ego.dat)

  }

  res <- res %>%
    summarize(ind.est = sum(asdr.hat.ind*agegrp_prop),
              agg.est = sum(asdr.hat.agg*agegrp_prop),
              adj.factor = adj.factor[1],
              adj.factor.allage = adj.factor.allage[1],
              adj.factor.meanagespec = sum(adj.factor.agespec*agegrp_prop)) %>%
    mutate(ratio.agg.ind = agg.est / ind.est) %>%
    mutate(ratio.ind.agg = ind.est  / agg.est) %>%
    ## `dummy` exists only to give summarize() a single group in the
    ## only_females branch; ungroup first, or select() refuses to drop it
    ungroup() %>%
    select(-dummy)

  ## if there are bootstrap results, also calculate aggregate for those
  if ('boot.asdr.ind' %in% names(estimates)) {

    # calculate confidence intervals for the total estimates
    # (the ones across all age groups), based on the bootstrap rep
    calculate_ci_for_total <- function(total_boot_res, only_females) {

      if(only_females) {
        res <- total_boot_res %>%
          mutate(dummy=1) %>%
          group_by(dummy) %>%
          summarize(across(c(ind.est, agg.est,
                             adj.factor, adj.factor.allage, adj.factor.meanagespec,
                             ratio.agg.ind,
                             ratio.ind.agg),
                           list( .ci.low = ~ quantile(.x, .025, na.rm=TRUE),
                                 .ci.high = ~ quantile(.x, .975, na.rm=TRUE),
                                 .mean = ~ mean(.x, na.rm=TRUE)))) %>%
          ungroup() %>%
          select(-dummy) %>%
          rename_with(~ stringr::str_replace(.x, "_", ""))
      } else {
        res <- total_boot_res %>%
          mutate(dummy=1) %>%
          group_by(dummy, sib.sex) %>%
          summarize(across(c(ind.est, agg.est,
                             adj.factor, adj.factor.allage, adj.factor.meanagespec,
                             ratio.agg.ind,
                             ratio.ind.agg),
                           list( .ci.low = ~ quantile(.x, .025, na.rm=TRUE),
                                 .ci.high = ~ quantile(.x, .975, na.rm=TRUE),
                                 .mean = ~ mean(.x, na.rm=TRUE)))) %>%
          ungroup() %>%
          select(-dummy) %>%
          rename_with(~ stringr::str_replace(.x, "_", ""))

      }

      return(res)

    }

    res_boot <- estimates$boot.asdr.ind %>%
      select(time.period, sib.sex, sib.age,
             boot_idx,
             # if we specified 'event.name' in the cell.config, keep it...
             any_of('event.name'),
             asdr.hat.ind = asdr.hat) %>%
      left_join(estimates$boot.asdr.agg %>%
                  select(time.period, sib.sex, sib.age,
                         boot_idx,
                         any_of('event.name'),
                         asdr.hat.agg = asdr.hat)) %>%
      # only need ages 15-49
      filter(sib.age %in% reproductive_age_groups())

    if (only_females) {

      res_boot <- res_boot %>%
        # only need females
        filter(sib.sex  == 'f') %>%
        left_join(age_prop, by=c('sib.age'='age.cat')) %>%
        ## NB: this join has to match the one in the point-estimate branch
        ## above. Joining on age alone duplicates every row once per sex
        ## present in ego_vis_agg, which silently multiplies the bootstrap
        ## estimates (exactly 2x when respondents include both sexes).
        left_join(vis_res$ego_vis_agg, by=c('sib.age'='age.cat',
                                            'sib.sex'='sex')) %>%
        group_by(boot_idx)

    } else {

      res_boot <- res_boot %>%
        left_join(age_prop,
                  by=c('sib.age'='age.cat',
                       'sib.sex'='sex')) %>%
        left_join(vis_res$ego_vis_agg,
                  by=c('sib.age'='age.cat',
                       'sib.sex'='sex')) %>%
        group_by(boot_idx, sib.sex)
    }

    res_boot <- res_boot %>%
      summarize(ind.est = sum(asdr.hat.ind*agegrp_prop),
                agg.est = sum(asdr.hat.agg*agegrp_prop),
                adj.factor = adj.factor[1],
                adj.factor.allage = adj.factor.allage[1],
                adj.factor.meanagespec = sum(adj.factor.agespec*agegrp_prop)) %>%
      mutate(ratio.agg.ind = agg.est / ind.est) %>%
      mutate(ratio.ind.agg = ind.est  / agg.est)

    res_boot_summ <- calculate_ci_for_total(res_boot, only_females) %>%
      select(order(colnames(.)))

    res <- list(point = res,
                boot_summ = res_boot_summ,
                boot = res_boot)
  }


  return(res)


}


