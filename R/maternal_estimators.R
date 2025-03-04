##' calculate total rate based on point estimates
##'
##' @param estimates the output of [siblingsurvival::sibling_estimator]
##' @param ego.dat the prepped ego data
##' @param sib.dat the prepped sibling data
##' @param only_females only keep female estimates? Defaults to TRUE; see Details, below
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
##'
##' @export
aggregate_maternal_estimates <- function(estimates,
                                         ego.dat,
                                         sib.dat,
                                         only_females = TRUE) {


  # get age distribution of respondents
  age_prop <- get_ego_age_distn(ego.dat,
                                only_females)

  vis_res <- get_visibility(ego.dat,
                            ego.id='caseid',
                            sib.dat,
                            sib.frame.indicator='in.F')

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
    # only need ages 15-50
    filter(! sib.age %in% c("[50,55)", "[55,60)", "[60,65)"))

  if (only_females) {

    res <- res %>%
      # only need female estimates
      filter(sib.sex  == 'f') %>%
      left_join(age_prop,
                by=c('sib.age'='age.cat')) %>%
      ## TODO - test that this works even if vis_res$ego_vis_agg has males and females?
      left_join(vis_res$ego_vis_agg,
                by=c('sib.age'='age.cat',
                     'sib.sex'='sex')) %>%
      mutate(dummy=1) %>%
      group_by(dummy)

  } else {

    res <- res %>%
      ## TODO NEED TO TEST THESE TWO JOINS
      left_join(age_prop,
                by=c('sib.age'='age.cat',
                     'sib.sex'='sex')) %>%
      left_join(vis_res$ego_vis_agg,
                by=c('sib.age'='age.cat',
                     'sib.sex'='sex')) %>%
      mutate(dummy=1) %>%
      ## TODO IS THIS RIGHT?
      group_by(sex)

  }

  res <- res %>%
    summarize(ind.est = sum(asdr.hat.ind*agegrp_prop),
              agg.est = sum(asdr.hat.agg*agegrp_prop),
              adj.factor = adj.factor[1],
              adj.factor.allage = adj.factor.allage[1],
              adj.factor.meanagespec = sum(adj.factor.agespec*agegrp_prop)) %>%
    mutate(ratio.agg.ind = agg.est / ind.est) %>%
    mutate(ratio.ind.agg = ind.est  / agg.est) %>%
    select(-dummy)

  ## TODO NEED TO ADAPT BOOTSTRAP VERSION BELOW
  ## TODO LEFT OFF HERE
  ##   - check that this works w/ females and with both
  ##   - see the other TODOs above here
  ##   - then also adapt bootstrap code below...

  ## if there are bootstrap results, also calculate aggregate for those
  if ('boot.asdr.ind' %in% names(estimates)) {

    # calculate confidence intervals for the total estimates
    # (the ones across all age groups), based on the bootstrap rep
    calculate_ci_for_total <- function(total_boot_res, only_females) {

      if(only_females) {
        res <- total_boot_res %>%
          mutate(dummy=1) %>%
          group_by(dummy) %>%
          summarize_at(vars(ind.est, agg.est,
                            adj.factor, adj.factor.allage, adj.factor.meanagespec,
                            ratio.agg.ind,
                            ratio.ind.agg),
                       list( .ci.low = ~ quantile(.x, .025, na.rm=TRUE),
                             .ci.high = ~ quantile(.x, .975, na.rm=TRUE),
                             .mean = ~ mean(.x, na.rm=TRUE))) %>%
          select(-dummy) %>%
          rename_with(~ stringr::str_replace(.x, "_", ""))
      } else {
        res <- total_boot_res %>%
          mutate(dummy=1) %>%
          group_by(dummy, sex) %>%
          summarize_at(vars(ind.est, agg.est,
                            adj.factor, adj.factor.allage, adj.factor.meanagespec,
                            ratio.agg.ind,
                            ratio.ind.agg),
                       list( .ci.low = ~ quantile(.x, .025, na.rm=TRUE),
                             .ci.high = ~ quantile(.x, .975, na.rm=TRUE),
                             .mean = ~ mean(.x, na.rm=TRUE))) %>%
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
      # only need ages 15-50
      filter(! sib.age %in% c("[50,55)", "[55,60)", "[60,65)"))

    if (only_females) {

      res_boot <- res_boot %>%
        # only need females
        filter(sib.sex  == 'f') %>%
        left_join(age_prop, by=c('sib.age'='age.cat')) %>%
        left_join(vis_res$ego_vis_agg, by=c('sib.age'='age.cat')) %>%
        group_by(boot_idx)

    } else {

      res_boot <- res_boot %>%
        left_join(age_prop,
                  by=c('sib.age'='age.cat',
                       'sib.sex'='sex')) %>%
        left_join(vis_res$ego_vis_agg,
                  by=c('sib.age'='age.cat',
                       'sib.sex'='sex')) %>%
        group_by(boot_idx, sex)
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


