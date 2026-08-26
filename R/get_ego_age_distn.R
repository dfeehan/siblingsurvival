##' get_ego_age_distribution
##'
##' @param ego.dat the ego dataset (probably from [siblingsurvival::prep_dhs_sib_histories])
##' @param only_females should only females be used to calculate age distribution? (default: True)
##' @param warn.single.sex when `only_females = FALSE`, warn if `ego.dat` holds
##'        only one respondent sex, since the result then cannot standardise
##'        rates for the other. Set `FALSE` only when the caller reports the
##'        problem itself, as
##'        [siblingsurvival::aggregate_maternal_estimates] does
##'
##' @return dataframe with distribution of respondent ages by 5-year category.
##' Columns `age.cat`, `total` and `agegrp_prop`, plus `sex` when
##' `only_females = FALSE`; see Details
##' @section Details:
##' `ego_dat` is assumed to have the columns `wwgt`, `age.cat` and `sex`
##'
##' The age groups used are those returned by
##' [siblingsurvival::reproductive_age_groups].
##'
##' When `only_females = TRUE` (the default), respondents are restricted to
##' females and a single age distribution is returned, with `agegrp_prop`
##' summing to 1 across age groups.
##'
##' When `only_females = FALSE`, a **separate** age distribution is returned for
##' each respondent sex: the result gains a `sex` column, and `agegrp_prop` sums
##' to 1 *within* each sex. This is what
##' [siblingsurvival::aggregate_maternal_estimates] needs in order to weight
##' each sex's age-specific rates by its own respondents' age structure.
##'
##' ## There is no male age distribution in DHS data, and you do not need one
##'
##' `only_females = FALSE` splits whatever respondents are in `ego.dat`; it does
##' not conjure a sex that was never interviewed. DHS sibling histories come from
##' the women's file, so a DHS `ego.dat` is entirely female and
##' `only_females = FALSE` returns the female distribution with a `sex` column
##' attached, and warns.
##'
##' It would be reasonable to conclude from `Chap16_AM/AM_rates.do` that this
##' blocks reproducing a published male rate, since its `get_age_distributions`
##' takes the men's age distribution from the men's recode (`MR`) file. **In
##' practice it does not.** Published DHS reports standardise *both* sexes by
##' the age distribution of the survey respondents --- the women --- which is
##' exactly what `only_females = TRUE` returns here.
##'
##' Checked against four published tables spanning DHS phases 4 to 8:
##'
##' | Survey | male rate, this way | published |
##' |---|---|---|
##' | Malawi 2000 | 11.064 | 11.1 |
##' | Rwanda 2005 | 7.393 | 7.39 |
##' | Rwanda 2014-15 | 2.961 | 2.96 |
##' | Gambia 2019-20 | 3.133 | 3.13 |
##'
##' Standardising men by a male distribution built from the `MR` file instead
##' gives 11.162, 7.285 and 2.881 for the first three --- further from the
##' published figures in every case.
##' @export
##' @md
get_ego_age_distn <- function(ego.dat,
                              only_females = TRUE,
                              warn.single.sex = TRUE) {

  if(only_females) {
    ego.dat <- ego.dat %>% filter(sex == 'f')
  }

  ## Asking for a per-sex distribution and getting only one sex back is the
  ## setup for a silent error: the caller wants to standardise each sex by its
  ## own respondents, and for the missing sex there is nothing to standardise
  ## with. Applying the sex that *is* present would attribute one sex's age
  ## structure to the other. DHS and MICS interview only women, so this is the
  ## normal case for them, not an exotic one.
  if (!only_females && warn.single.sex) {

    ego.sexes <- sort(unique(as.character(ego.dat$sex)))

    if (length(ego.sexes) < 2) {
      warning(glue::glue(
        "only_females = FALSE asks for one age distribution per respondent sex, ",
        "but the respondents in ego.dat are all '{paste0(ego.sexes, collapse=\"', '\")}'.\n",
        "The result therefore covers that sex alone, and any estimate for ",
        "another sex built from it will be NA rather than wrong.\n",
        "If you are trying to reproduce published DHS figures, note that they ",
        "standardise *both* sexes by the age distribution of the survey ",
        "respondents -- which is what only_females = TRUE returns. Verified ",
        "against Rwanda 2005 and 2014-15, Gambia 2019-20 and Malawi 2000: the ",
        "published male rates match that standardisation, not one built from a ",
        "male age distribution."))
    }
  }

  respondent_age <- ego.dat %>%
    ## age.cat and wwgt are assumed to come with the dataset; they have
    ## ego age in 5-year groups and the women's weight
    filter(age.cat %in% reproductive_age_groups())

  ## A respondent with no sampling weight cannot contribute to a weighted
  ## distribution, and leaving her in makes `sum(wwgt)` NA -- which propagates
  ## through the denominator and turns *every* agegrp_prop into NA, silently
  ## NA-ing out any age-standardised rate computed from it. Sao Tome and
  ## Principe 2014 has exactly one such respondent. The prep functions already
  ## drop these rows from sib.dat; do the same here.
  n.badwgt <- sum(is.na(respondent_age$wwgt))
  if (n.badwgt > 0) {
    warning(glue::glue(
      "{n.badwgt} respondent(s) have no sampling weight and are dropped from ",
      "the age distribution. Left in, they would make every group proportion NA."))
    respondent_age <- respondent_age %>% filter(!is.na(wwgt))
  }

  if(only_females) {

    respondent_age <- respondent_age %>%
      group_by(age.cat) %>%
      # note that [siblingsurvival::prep_dhs_sib_histories]
      # will have already scaled these weights
      summarize(total = sum(wwgt), .groups = 'drop') %>%
      mutate(agegrp_prop = total / sum(total))

  } else {

    ## one age distribution per respondent sex, each summing to 1
    respondent_age <- respondent_age %>%
      group_by(sex, age.cat) %>%
      summarize(total = sum(wwgt), .groups = 'drop') %>%
      group_by(sex) %>%
      mutate(agegrp_prop = total / sum(total)) %>%
      ungroup()

  }

  return(respondent_age)
}
