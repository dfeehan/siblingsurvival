##' prepare a DHS dataset for analysis
##'
##' @param df the raw DHS dataset (indvidual recode)
##' @param varmap see Details
##' @param add_maternal should maternal/pregnancy-related death info be added? (Default: FALSE)
##' @param na.action only used when `add_maternal = TRUE`; defaults to
##'        `"include"` for DHS data, which is what this package has always done.
##'        See [siblingsurvival::add_maternal_deaths]
##' @param keep_missing should we keep reported sibs that are missing sex or survival status?
##' @param keep_varmap_only should we only keep ego variables mentioned in the varmap? (Default: FALSE)
##' @param weight.scale divide the women's weight by this number. Defaults to
##'        `1e6`, which is correct for the DHS; see Details
##' @param verbose report detailed summaries?
##' @return a list; see Details
##' @examples
##'   # TODO - write example code
##' @section Details:
##'
##' The DHS publishes women's weights multiplied by 1,000,000, so the default
##' `weight.scale = 1e6` recovers weights that average 1. Surveys whose weights
##' are already normalized should pass `weight.scale = 1`.
##'
##' Note that if the dataframe does not have a column called 'sex', then
##' one will be added, and we will assume respondents are all female (sex='f'). If you
##' want to avoid this, only pass in a dataframe after adding the 'sex' column.
##'
##' `varmap` should be a dataframe with columns
##' * `orig.varname` (the raw variable name)
##' * `new.varname` (the new variable name)
##' * `sibvar` (a 0/1 column, with 1 meaning this is a sibling variable and 0 meaning an ego variable)
##'
##' Each row of `varmap` describes a variable to rename from the original dataset.
##'
##' For respondents, you should be sure to include
##' * `survey` (the survey id, usually a country code plus one digit)
##' * `caseid` (the respondent id)
##' * `wwgt` (the sampling weight for women)
##' * `psu` (the primary sampling unit)
##' * `doi` (the date of the interview)
##'
##' For siblings, you should be sure to include
##' * `sib.death.date` (the date of the sibling's death)
##' * `sib.alive` (whether or not the sib is alive)
##' * `sib.sex` (the sex of the sibling, coded 'f' or 'm').
##'
##' The default varmap is `sibhist_varmap_dhs6`, which is included with the package.
##'
##' Returns a list whose entries include
##' * `ego.dat` - dataset with information about the survey respondents
##' * `sib.dat` - dataset with information about the reported siblings
##' * `summ` - a one-row tibble with a summary of the data
##'
##' @export
prep_dhs_sib_histories <- function(df,
                                   varmap=sibhist_varmap_dhs6,
                                   add_maternal=FALSE,
                                   na.action=NULL,
                                   keep_missing=FALSE,
                                   keep_varmap_only=FALSE,
                                   weight.scale=1e6,
                                   verbose=TRUE) {

  ## ego (respondent) variables to grab
  tmp <- varmap %>% filter(sibvar==0)
  resp.attrib <- tmp$orig.varname
  names(resp.attrib) <- tmp$new.varname

  ## alter (sibling) variables to grab
  tmp <- varmap %>% filter(sibvar==1)
  sib.attrib <- tmp$orig.varname
  names(sib.attrib) <- tmp$new.varname

  #########################
  # prepare ego data
  #########################

  ## in some cases, variables in the varmap may not be in the specific dataset
  ## we are preparing (for example, some surveys don't have the 'literacy'
  ## variable, v155); in those cases, we report it but proceed
  miss_col <- check_varmap_cols(df, resp.attrib, sib.attrib, verbose=verbose)

  ego.dat <- get_ego_df(df, resp.attrib, verbose, weight.scale=weight.scale)

  cur.survey <- ego.dat$survey[1]

  #########################
  # prepare sibling data
  #########################

  sib.dat <- get_sib_df(ego.dat, sib.attrib, verbose)

  #########################
  # add maternal vars, if needed
  #########################
  if (add_maternal) {
    if (verbose) cat("Adding pregnancy-related/maternal death info\n")
    sib.dat <- add_maternal_deaths(sib.dat,
                                   style='dhs',
                                   na.action=na.action,
                                   keep_missing=keep_missing,
                                   verbose=verbose)
  }

  #########################
  # summarize, filter, and assemble the result
  #########################
  return(finalize_sib_prep(ego.dat = ego.dat,
                           sib.dat = sib.dat,
                           cur.survey = cur.survey,
                           miss_col = miss_col,
                           resp.attrib = resp.attrib,
                           keep_missing = keep_missing,
                           keep_varmap_only = keep_varmap_only,
                           verbose = verbose))
}

##' prepare a dataset from nrsimulatr for sibling analysis
##'
##' @param df the raw dataset (each row is a survey response)
##' @param varmap see Details; defaults to NULL
##' @param keep_missing should we keep reported sibs that are missing sex or survival status?
##' @param keep_varmap_only should we only keep ego variables mentioned in the varmap? (Default: FALSE)
##' @param weight.scale divide the weight by this number. Defaults to `1`, since
##'        non-DHS weights are typically already normalized; see Details
##' @param verbose report detailed summaries?
##' @return a list; see Details
##' @examples
##'   # TODO - write example code
##' @section Details:
##'
##' This function is similar to [siblingsurvival::prep_dhs_sib_histories], but it
##' is not customized to work with DHS survey data.
##'
##' In particular, `weight.scale` defaults to `1` here, because weights outside
##' the DHS are typically already normalized to average 1. Pass
##' `weight.scale = 1e6` if you are preparing data that follows the DHS
##' convention of publishing weights multiplied by 1,000,000.
##'
##' Note that if the dataframe does not have a column called 'sex', then
##' one will be added, and we will assume respondents are all female (sex='f'). If you
##' want to avoid this, only pass in a dataframe after adding the 'sex' column.
##'
##' `varmap` should be a dataframe with columns
##' * `orig.varname` (the raw variable name)
##' * `new.varname` (the new variable name)
##' * `sibvar` (a 0/1 column, with 1 meaning this is a sibling variable and 0 meaning an ego variable)
##'
##' Each row of `varmap` describes a variable to rename from the original dataset.
##' Note that you MUST include `varmap`; at a minimum, it is needed to show which columns
##' are siblings...
##'
##' For respondents, you should be sure to include
##' * `survey` (the survey id, usually a country code plus one digit)
##' * `caseid` (the respondent id)
##' * `wwgt` (the sampling weight for women)
##' * `psu` (the primary sampling unit)
##' * `doi` (the date of the interview)
##'
##' For siblings, you should be sure to include
##' * `sib.death.date` (the date of the sibling's death)
##' * `sib.alive` (whether or not the sib is alive)
##' * `sib.sex` (the sex of the sibling, coded 'f' or 'm').
##'
##' Returns a list whose entries include
##' * `ego.dat` - dataset with information about the survey respondents
##' * `sib.dat` - dataset with information about the reported siblings
##' * `summ` - a one-row tibble with a summary of the data
##'
##' @export
prep_nrsim_sib_histories <- function(df,
                                     varmap,
                                     keep_missing=FALSE,
                                     keep_varmap_only=FALSE,
                                     weight.scale=1,
                                     verbose=TRUE) {

  ## ego (respondent) variables to grab
  tmp <- varmap %>% filter(sibvar==0)
  resp.attrib <- tmp$orig.varname
  names(resp.attrib) <- tmp$new.varname

  ## alter (sibling) variables to grab
  tmp <- varmap %>% filter(sibvar==1)
  sib.attrib <- tmp$orig.varname
  names(sib.attrib) <- tmp$new.varname

  #########################
  # prepare ego data
  #########################

  ## in some cases, variables in the varmap may not be in the specific dataset
  ## we are preparing (for example, some surveys don't have the 'literacy'
  ## variable, v155); in those cases, we report it but proceed
  miss_col <- check_varmap_cols(df, resp.attrib, sib.attrib, verbose=verbose)


  #########################
  # prepare ego data
  #########################

  ego.dat <- get_ego_df(df,
                        resp.attrib,
                        verbose,
                        weight.scale=weight.scale)

  cur.survey <- ego.dat$survey[1]

  #########################
  # prepare sibling data
  #########################

  sib.dat <- get_sib_df(ego.dat, sib.attrib, verbose)

  #########################
  # summarize, filter, and assemble the result
  #########################
  return(finalize_sib_prep(ego.dat = ego.dat,
                           sib.dat = sib.dat,
                           cur.survey = cur.survey,
                           miss_col = miss_col,
                           resp.attrib = resp.attrib,
                           keep_missing = keep_missing,
                           keep_varmap_only = keep_varmap_only,
                           verbose = verbose))
}

##' helper to prep the ego dataset
##'
##' @param df the survey dataset
##' @param resp.attrib vector with respondent attribute columns (see [siblingsurvival::prep_dhs_sib_histories])
##' @param verbose see [siblingsurvival::prep_dhs_sib_histories]
##' @param weight.scale divide the `wwgt` column by this number. DHS weights are
##'        published multiplied by 1,000,000, so `1e6` recovers weights that
##'        average 1; surveys whose weights are already normalized (MICS, and
##'        simulated data) should pass `1`. See
##'        [siblingsurvival::prep_dhs_sib_histories]
##' @return a prepped ego dataset, used in [siblingsurvival::prep_dhs_sib_histories]
##'
get_ego_df <- function(df, resp.attrib, verbose=FALSE, weight.scale=1e6) {

  ## TODO - to make more generic...
  ##   - customizable weight variable
  ##   - customizable age variable

  ego.dat <- df %>%
    as_tibble() %>%
    # use information from the varmap to rename ego variables
    #rename(!!!resp.attrib)
    rename(any_of(resp.attrib))

  if(!"sex" %in% names(ego.dat)) {
    if(verbose) {
      cat(paste0("\nNo information on respondent sex given; assuming all respondents are female.\n"))
    }

    ## typically, only women are asked sibling histories in DHS surveys
    ego.dat$sex <- 'f'
  }

  ## the ego dataset has to have an age (in single years) and a survey id;
  ## check for them here rather than letting the failure surface as an opaque
  ## error inside the mutate() and the survey lookup below
  required.ego <- c('age', 'survey')
  missing.ego <- required.ego[! required.ego %in% names(ego.dat)]

  if (length(missing.ego) > 0) {
    stop(glue::glue(
      "The ego dataset is missing required column(s): ",
      "{paste0(missing.ego, collapse=', ')}.\n",
      "These come from the varmap: it needs a row mapping each of them ",
      "(with sibvar=0). The ego dataset has columns: ",
      "{paste0(names(ego.dat), collapse=', ')}\n"))
  }

  ## rescale the weights if we were asked to.
  ##
  ## DHS publishes women's weights multiplied by 1,000,000, so dividing by 1e6
  ## recovers weights that average 1 (see DHS documentation). Surveys whose
  ## weights are already normalized -- MICS wmweight, and simulated data --
  ## must pass weight.scale=1, since dividing those by 1e6 would be wrong by
  ## six orders of magnitude and nothing downstream would complain.
  if('wwgt' %in% names(ego.dat) && weight.scale != 1) {
    if(verbose) {
      cat(paste0("\nScaling wwgt by 1/", format(weight.scale, scientific=FALSE),
                 " (pass weight.scale=1 if these weights are already normalized).\n"))
    }
    ego.dat <- ego.dat %>%
      mutate(wwgt = wwgt / weight.scale)

  }

  ego.dat <- ego.dat %>%
    mutate(
      ## for convenience, add 5- and 10-year age groups
      age.cat=forcats::fct_drop(cut(age,
                                    breaks=c(0, seq(from=15,to=50,by=5),95),
                                    include.lowest=TRUE, right=FALSE)),
      age.cat10=forcats::fct_drop(cut(age,
                                      breaks=c(0, seq(from=15,to=50,by=10),95),
                                      include.lowest=TRUE, right=FALSE))
    )


  if (length(ego.dat$survey) == 0) {
    stop("There appears to be no 'survey' column in the ego dataset.\n")
  }
  cur.survey <- ego.dat$survey[1]

  return(ego.dat)

}

##' helper to prep the sib dataset
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
##' @return a prepped sibling dataset, used in [siblingsurvival::prep_dhs_sib_histories]
##'
get_sib_df <- function(ego.dat, sib.attrib, verbose=FALSE, reshape=TRUE,
                       max.plausible.age=110) {

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

  sib.dat <- sib.dat %>%
    mutate(sib.sex = ifelse(sib.sex == 2, 'f', 'm'))


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

  ## make the assumption that
  ##  (1) sibs who died lived all the way through
  ##      the month in which they are reported to have died
  ##  (2) the interview took place on the first of the month
  ## these assumptions are necessary to get these tables to line up
  ## with the DHS reports
  sib.dat$sib.endobs <- pmin(sib.dat$doi,
                             sib.dat$sib.death.date + 1,
                             na.rm=TRUE)

  ## siblings who haven't died get their death dates
  ## recoded to -1 so we don't lose the exposures they
  ## contribute...
  sib.dat$sib.death.date[ is.na(sib.dat$sib.death.date) ] <- -1

  ## create a unique id for each sib
  sib.dat$sibid <- 1:nrow(sib.dat)


  return(sib.dat)

}

##' add pregnancy-related and maternal death info to a sibling dataset
##'
##' @param sib_df the prepped sibling dataset (probably from
##'        [siblingsurvival::prep_dhs_sib_histories] or
##'        [siblingsurvival::prep_mics_sib_histories])
##' @param style which questionnaire the coding follows: `"dhs"` (the default),
##'        `"mics6"` for MICS6/MICS7, or `"mics4"` for MICS4/MICS5
##' @param na.action how to treat a death that falls in the right window but
##'        whose timing detail is missing -- `"include"` counts it, `"exclude"`
##'        does not. **Required for the MICS styles**; see Details
##' @param keep_missing not currently used
##' @param verbose report detailed summaries?
##' @return `sib_df` with columns `sib.preg_related.death.date` and
##'         `sib.maternal.death.date` added
##' @examples
##'   # TODO - write example code
##' @section Details:
##'
##' Two quantities are computed, and they are not the same thing:
##'
##' * **pregnancy-related** -- died while pregnant, during childbirth, or within
##'   two months of the end of a pregnancy, *whatever the cause*
##' * **maternal** -- as above but within 42 days, and excluding deaths due to
##'   violence or an accident
##'
##' `sib.maternal.death.date` is only computable when the data identify
##' accidental deaths, which means DHS phase 7 and later, or MICS6 and later.
##' Otherwise it is `NA` and only the pregnancy-related column is usable -- the
##' same limitation applies to DHS phases 2--6 and to MICS4/MICS5.
##'
##' Siblings who did not die of the relevant cause get a death date of `-1`
##' rather than `NA`, so that they still contribute exposure. Both columns are
##' set to `NA` for male siblings.
##'
##' **Note for MICS users:** the tables published in MICS survey reports under
##' the heading "Maternal mortality", with a column labelled "Maternal Deaths",
##' actually report the **pregnancy-related** count. Compare against
##' `sib.preg_related.death.date`, not `sib.maternal.death.date`. See the
##' vignette "Working with MICS sibling history data".
##'
##' ## Choosing `na.action`
##'
##' `na.action` has **no default for the MICS styles**, because it is a
##' substantive choice about the estimand rather than a coding detail, and it
##' should be made deliberately and reported.
##'
##' In MICS, the number of days after the end of a pregnancy (`MM25`) is asked
##' only of sisters who died within two months (`MM24 = 1`). When that day count
##' is missing, `na.action` decides whether she falls inside the 42-day maternal
##' window. In three MICS6 surveys examined this affected 5 of 38 such deaths in
##' Iraq 2018 and none at all in Zimbabwe 2019 or Madagascar 2018, so the choice
##' is usually immaterial -- but not always, and it only ever moves the maternal
##' column, never the pregnancy-related one.
##'
##' For `style = "dhs"`, `na.action` defaults to `"include"`, which is what this
##' package has always done: a missing `sib.time.delivery.death` was treated as
##' falling in the window. That default is kept so existing results do not move.
##'
##' @export
##' @md
add_maternal_deaths <- function(sib_df,
                                style = c("dhs", "mics6", "mics4"),
                                na.action = NULL,
                                keep_missing = FALSE,
                                verbose = TRUE) {

  style <- match.arg(style)

  if (is.null(na.action)) {

    if (style == "dhs") {
      ## the behaviour this package has always had; kept so that existing
      ## results do not silently change
      na.action <- "include"
    } else {
      stop(glue::glue(
        "`na.action` is required for style = '{style}'.\n",
        "In MICS the number of days after the end of a pregnancy (MM25) is ",
        "asked only of sisters who died within two months (MM24 = 1). When it ",
        "is missing, you have to say whether she counts as inside the 42-day ",
        "maternal window:\n",
        "  na.action = 'include'  -- count her as maternal\n",
        "  na.action = 'exclude'  -- require an observed day count\n",
        "This is a choice about the estimand, so it has no default. It moves ",
        "only sib.maternal.death.date, never sib.preg_related.death.date.\n"))
    }
  }

  na.action <- match.arg(na.action, c("include", "exclude"))

  #########################
  # pregnancy-related deaths
  #########################
  ## available for every DHS phase and every MICS round with a sibling roster
  if (style == "dhs") {
    is.pr <- is_preg_related_dhs(sib_df, na.action)
  } else {
    is.pr <- is_preg_related_mics(sib_df)
  }

  ## siblings who did not die a pregnancy-related death get -1 rather than NA,
  ## so that we keep the exposure they contribute
  sib_df$sib.preg_related.death.date <- ifelse(is.pr, sib_df$sib.death.date, -1)
  sib_df$sib.preg_related.death.date[is.na(sib_df$sib.preg_related.death.date)] <- -1

  #########################
  # maternal deaths
  #########################
  ## only possible where accidental deaths are identified: DHS phase 7 and
  ## later, MICS6 and later
  can.do.maternal <- 'sib.died.accident' %in% names(sib_df)

  if (can.do.maternal) {

    if (style == "dhs") {
      is.mat <- is_maternal_dhs(sib_df, na.action)
    } else {
      is.mat <- is_maternal_mics(sib_df, na.action)
    }

    sib_df$sib.maternal.death.date <- ifelse(is.mat, sib_df$sib.death.date, -1)
    sib_df$sib.maternal.death.date[is.na(sib_df$sib.maternal.death.date)] <- -1

  } else {

    if (verbose) {
      cat("\n...sib.died.accident column not found; only pregnancy-related deaths can be identified here\n")
    }
    sib_df$sib.maternal.death.date <- NA
  }

  #########################
  # males
  #########################
  sib_df <- sib_df %>%
    mutate(sib.preg_related.death.date = ifelse(sib.sex == 'm', NA, sib.preg_related.death.date)) %>%
    mutate(sib.maternal.death.date     = ifelse(sib.sex == 'm', NA, sib.maternal.death.date))

  if (verbose) {
    n.pr  <- sum(sib_df$sib.preg_related.death.date > 0, na.rm = TRUE)
    n.mat <- sum(sib_df$sib.maternal.death.date > 0, na.rm = TRUE)
    cat(paste0("Identified ", n.pr, " pregnancy-related death(s)",
               if (can.do.maternal) paste0(" and ", n.mat, " maternal death(s)") else "",
               " (style = '", style, "', na.action = '", na.action, "').\n"))
  }

  return(sib_df)
}
