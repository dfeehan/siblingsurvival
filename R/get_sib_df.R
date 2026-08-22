## get_sib_df(): build the sibling-level dataset, reshaping wide data when
## needed and deriving birth and death dates that were not supplied.
## Shared by all three prep_*_sib_histories() functions.

##' helper to prep the sib dataset
##'
##' Shared by [siblingsurvival::prep_dhs_sib_histories],
##' [siblingsurvival::prep_nrsim_sib_histories] and
##' [siblingsurvival::prep_mics_sib_histories].
##'
##' @param ego.dat the prepped ego dataset
##' @param sib.attrib vector with sibling attribute columns (see [siblingsurvival::prep_dhs_sib_histories])
##' @param verbose see [siblingsurvival::prep_dhs_sib_histories]
##' @param reshape is `ego.dat` wide, with one column per sibling attribute per
##'        sibling (TRUE, the DHS layout), or already one row per reported
##'        sibling (FALSE, the MICS `mm.sav` layout)?
##' @param max.plausible.age warn about siblings whose derived date of birth
##'        implies they would be older than this at the date of interview. Such
##'        rows mean the reported years-since-death and age at death are
##'        jointly inconsistent. Note this is *not* a check on whether a sibling
##'        died before the respondent was born, which is perfectly possible
##' @param death.exposure whether a sibling who died contributes the month of
##'        death as exposure. `"dhs"` (the default) counts it, matching
##'        `Chap16_AM/AM_rates.do`; `"mics"` stops the month before, matching
##'        the MICS6 tabulation syntax. The two references genuinely disagree
##'        here, so it cannot be settled by getting it "right"
##' @return a prepped sibling dataset, used by the `prep_*_sib_histories()` functions
##'
get_sib_df <- function(ego.dat, sib.attrib, verbose=FALSE, reshape=TRUE,
                       max.plausible.age=110,
                       death.exposure=c("dhs", "mics")) {

  death.exposure <- match.arg(death.exposure)

  ## these ego columns are carried onto every sibling row
  required.ego <- c('caseid', 'wwgt', 'psu', 'doi', 'sex')
  missing.ego <- required.ego[! required.ego %in% names(ego.dat)]

  if (length(missing.ego) > 0) {
    stop(glue::glue(
      "The ego dataset is missing column(s) needed to build the sibling data: ",
      "{paste0(missing.ego, collapse=', ')}.\n",
      "Note that 'doi' has to be the date of interview as a CMC (century month ",
      "code), since the sibling date derivations are arithmetic in months.\n"))
  }

  if (reshape) {

    ## wide data (one row per respondent, one column per sibling attribute
    ## per sibling), as the DHS publishes it
    sib.dat <- attributes.to.long(ego.dat,
                                  attribute.prefix=sib.attrib,
                                  ego.vars=c('caseid', 'wwgt',
                                             'psu', 'doi', 'sex'),
                                  idvar="caseid")

  } else {

    ## already one row per reported sibling, as MICS publishes it in mm.sav.
    ## the caller has renamed via the varmap and attached the ego columns,
    ## so there is nothing to reshape
    sib.dat <- ego.dat

  }

  ## Two groups of columns, and the distinction matters.
  ##
  ## These have to be supplied: nothing can reconstruct them.
  required.sib <- c('sib.sex', 'sib.alive', 'sib.age',
                    'sib.death.yrsago', 'sib.death.age')
  missing.sib <- required.sib[! required.sib %in% names(sib.dat)]

  if (length(missing.sib) > 0) {
    stop(glue::glue(
      "The sibling data is missing required column(s): ",
      "{paste0(missing.sib, collapse=', ')}.\n",
      "The varmap needs a row mapping each of them (with sibvar=1); the ",
      "sibling data has columns: {paste0(names(sib.dat), collapse=', ')}\n"))
  }

  ## These are *derived* when not supplied. The DHS reports both as CMC dates
  ## (mm4, mm8) and MICS often does too (MM17C/MM18C in MICS6, MM7C/MM8C in
  ## MICS4/5), but some surveys report only ages and years-since-death. In that
  ## case initialize them to NA and let the case_when()s below fill them in.
  derived.sib <- c('sib.dob', 'sib.death.date')
  absent.derived <- derived.sib[! derived.sib %in% names(sib.dat)]

  if (length(absent.derived) > 0) {

    if (verbose) {
      cat(glue::glue("

                      No {paste0(absent.derived, collapse=' or ')} column(s) in the data; \\
                      these will be approximated from reported ages and \\
                      years-since-death.

                      "))
    }

    for (this.col in absent.derived) {
      sib.dat[[this.col]] <- NA_real_
    }
  }

  ## Only 1 (male) and 2 (female) are meaningful. The DHS labels 8 as
  ## "don't know", and some surveys carry an unlabelled 9 -- Gabon 2000 has 163
  ## of them. `ifelse(sib.sex == 2, 'f', 'm')` silently made every one of those
  ## male, which inflated male exposure in 13 of the 43 DHS surveys examined and
  ## quietly put siblings of unknown sex into the male rates. Anything other
  ## than 1 or 2 now becomes NA, and finalize_sib_prep() drops it and reports it
  ## in summ$miss.sex -- which is what the MICS path already relied on
  ## recode_mics_sib_vars() to arrange.
  sib.dat <- sib.dat %>%
    mutate(sib.sex = dplyr::case_when(sib.sex == 1 ~ 'm',
                                      sib.sex == 2 ~ 'f',
                                      TRUE         ~ NA_character_))


  ## in some cases, there will be information about how many years ago
  ## a sibling died, and at what age - but not the date
  ## in those cases, assume that the death happened (12*x) + 6 months ago,
  ## i.e., assume the death happened halfway through the year that was x years ago
  ##
  ## eg, if cmc of intervie, doi, is 1500
  ## and a sib died 1 years ago, we'd estimate
  ## death date of 1500 - (12*1 + 6) = 1482
  approx_death_date <- function(years_ago, doi) {
    return(as.integer(doi - (12*years_ago + 6)))
  }

  ## same idea as above, but now try to figure birth date based on
  ## age at death and how many years ago death was
  approx_birth_date_from_death <- function(death_date, age_at_death) {
    return(as.integer(death_date - (12*age_at_death)))
  }

  ## same idea as above, but now try to figure birth date based on
  ## age at a living sibling
  ## assume sib is halfway through year of age
  approx_birth_date_from_age <- function(sib_age, doi) {
    return(as.integer(doi - (12*sib_age + 6)))
  }

  sib.dat <- sib.dat %>%
    mutate(sib.death.date = case_when((! is.na(sib.death.yrsago)) &
                                        (is.na(sib.death.date)) ~
                                        approx_death_date(sib.death.yrsago,
                                                          doi),
                                      TRUE ~ sib.death.date)) %>%
    # estimate birth date from a sib who is living and whose age we have,
    # but whose dob we do not have
    mutate(sib.dob = case_when((is.na(sib.dob) &
                                  (sib.alive == 1) &
                                  (! is.na(sib.age))) ~
                                 approx_birth_date_from_age(sib.age,
                                                            doi),
                               TRUE ~ sib.dob)) %>%
    # estimate birth date from a sib whose date of death and age at death
    # we have
    # NB: the guard has to be on sib.death.age, since that is what the
    #     approximation actually uses; guarding on sib.death.yrsago instead
    #     silently produced NA whenever age at death was missing
    mutate(sib.dob = case_when((is.na(sib.dob) &
                                  (! is.na(sib.death.age)) &
                                  (! is.na(sib.death.date))  ) ~
                                 approx_birth_date_from_death(sib.death.date,
                                                              sib.death.age),
                               TRUE ~ sib.dob))

  ## Sanity-check the derived dates by the implied age of the sibling at the
  ## date of interview.
  ##
  ## Note that a sibling dying *before the respondent was born* is perfectly
  ## possible, and not rare where fertility is high and sibships are long --
  ## that is not what this flags. What it flags is an implied age no human
  ## reaches, which means the reported years-since-death and age-at-death are
  ## jointly inconsistent. Eg Bhutan 2010 has a sibling reported as dying 58
  ## years ago at age 58, implying a birth 116 years before the interview.
  ##
  ## They are kept, not dropped: the exposure they contribute lands outside any
  ## reproductive age group anyway, and silently discarding reported data is
  ## worse than flagging it.
  implied.age <- (sib.dat$doi - sib.dat$sib.dob) / 12
  n.implausible <- sum(implied.age > max.plausible.age, na.rm=TRUE)

  if (n.implausible > 0) {
    warning(glue::glue(
      "{n.implausible} sibling(s) have an implied age at interview over ",
      "{max.plausible.age} years, which means the reported years-since-death ",
      "and age at death are jointly inconsistent. They are retained; inspect ",
      "them with `subset(sib.dat, (doi - sib.dob)/12 > {max.plausible.age})`."))
  }

  ## How much of the month of death counts as exposure? The two reference
  ## implementations disagree, so this is an argument rather than an assumption.
  ##
  ##  "dhs"  -- a sibling lived all the way through the month she is reported to
  ##            have died in, so that month is exposure. AM_rates.do:711 sets
  ##            `last = mm8` and then counts `mexp = last - first + 1`.
  ##  "mics" -- exposure stops the month *before* death. The MICS6 syntax sets
  ##            `higcm = MM18C - 1`.
  ##
  ## Observation windows here are half-open, [start, end), so "through the month
  ## of death" is `death + 1` and "up to the month before" is `death`.
  ##
  ## The interview is treated as taking place on the first of the month either
  ## way, which is why `doi` caps the result: both references ignore exposure and
  ## events in the month of interview.
  death.month.offset <- switch(death.exposure, dhs = 1L, mics = 0L)

  sib.dat$sib.endobs <- pmin(sib.dat$doi,
                             sib.dat$sib.death.date + death.month.offset,
                             na.rm=TRUE)

  ## siblings who haven't died get their death dates
  ## recoded to -1 so we don't lose the exposures they
  ## contribute...
  sib.dat$sib.death.date[ is.na(sib.dat$sib.death.date) ] <- -1

  ## create a unique id for each sib
  sib.dat$sibid <- 1:nrow(sib.dat)


  return(sib.dat)

}
