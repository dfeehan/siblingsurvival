##' shared tail of the sibling-history prep functions
##'
##' Summarises a prepped ego/sibling pair, optionally drops sibling reports that
##' are missing survival status or sex, and assembles the returned list. Shared
##' by [siblingsurvival::prep_dhs_sib_histories],
##' [siblingsurvival::prep_nrsim_sib_histories] and
##' [siblingsurvival::prep_mics_sib_histories] so that the three preps cannot
##' drift apart.
##'
##' @param ego.dat the prepped ego dataset
##' @param sib.dat the prepped sibling dataset
##' @param cur.survey the survey id
##' @param miss_col the list returned by `check_varmap_cols()`
##' @param resp.attrib named vector of ego variables from the varmap, used by
##'        `keep_varmap_only`
##' @param keep_missing see [siblingsurvival::prep_dhs_sib_histories]
##' @param keep_varmap_only see [siblingsurvival::prep_dhs_sib_histories]
##' @param verbose see [siblingsurvival::prep_dhs_sib_histories]
##' @return a list with entries `survey`, `ego.dat`, `sib.dat` and `summ`
##'
finalize_sib_prep <- function(ego.dat,
                              sib.dat,
                              cur.survey,
                              miss_col,
                              resp.attrib,
                              keep_missing=FALSE,
                              keep_varmap_only=FALSE,
                              verbose=TRUE) {

  n.ego <- nrow(ego.dat)
  n.sib.raw <- nrow(sib.dat)

  ## CALCULATE % of siblings with unknown survival status
  pre.n <- nrow(sib.dat)
  tmp <- sib.dat %>% filter(sib.alive %in% c(0,1))
  post.n <- nrow(tmp)
  miss.alive <- pre.n-post.n
  miss.alive.pct <- 100 * miss.alive / pre.n

  ## CALCULATE % of siblings with unknown sex
  pre.n <- nrow(sib.dat)
  tmp <- sib.dat %>% filter(sib.sex %in% c('f', 'm'))
  post.n <- nrow(tmp)
  miss.sex <- pre.n-post.n
  miss.sex.pct <- 100 * miss.sex / pre.n

  if(verbose) {

    cat(paste0(miss.alive, " out of ", n.sib.raw, " (", round(miss.alive.pct,2), "%)",
               " reports about sibs have unknown survival status.\n"))

    cat(paste0(miss.sex, " out of ", n.sib.raw, " (", round(miss.sex.pct,2), "%)",
               " reports about sibs have unknown sex.\n"))
  }

  sibs.removed.n <- 0
  sibs.removed.pct <- 0

  if(! keep_missing) {

    ## take siblings missing sex and missing survival status out of the analysis,
    if(verbose) cat("Removing reported sibs missing survival status or sex.\n")
    pre.n <- nrow(sib.dat)
    sib.dat <- sib.dat %>% filter(sib.alive %in% c(0,1)) %>% filter(sib.sex %in% c('f', 'm'))
    post.n <- nrow(sib.dat)
    sibs.removed.n <- pre.n - post.n
    sibs.removed.pct <- 100 * sibs.removed.n / n.sib.raw

    if(verbose) {
      cat("... this removes ", pre.n-post.n, " out of ", pre.n," (",
          round(100*(pre.n-post.n)/pre.n,2), "%)", " sibling reports.\n")
    }
  }

  n.sib <- nrow(sib.dat)

  summ <- tibble(survey=cur.survey,
                 n.ego = n.ego,
                 n.sib.raw = n.sib.raw,
                 n.sib = n.sib,
                 miss.alive = miss.alive,
                 miss.alive.pct = miss.alive.pct,
                 miss.sex = miss.sex,
                 miss.sex.pct = miss.sex.pct,
                 sibs.removed = sibs.removed.n,
                 sibs.removed.pct = sibs.removed.pct,
                 ego.cols.notfound = list(miss_col$ego),
                 sib.cols.notfound = list(miss_col$sib))

  if (keep_varmap_only) {
    ego.dat <- ego.dat %>%
      select(any_of(c(names(resp.attrib), 'sex', 'age.cat', 'age.cat10', 'wwgt')))
  }

  return(list(survey=cur.survey,
              ego.dat = ego.dat,
              sib.dat = sib.dat,
              summ = summ))
}
