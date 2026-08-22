## get_ego_df(): build the respondent-level dataset. Shared by all three
## prep_*_sib_histories() functions.

##' helper to prep the ego dataset
##'
##' Shared by [siblingsurvival::prep_dhs_sib_histories],
##' [siblingsurvival::prep_nrsim_sib_histories] and
##' [siblingsurvival::prep_mics_sib_histories].
##'
##' @param df the survey dataset
##' @param resp.attrib vector with respondent attribute columns (see [siblingsurvival::prep_dhs_sib_histories])
##' @param verbose see [siblingsurvival::prep_dhs_sib_histories]
##' @param weight.scale divide the `wwgt` column by this number. DHS weights are
##'        published multiplied by 1,000,000, so `1e6` recovers weights that
##'        average 1; surveys whose weights are already normalized (MICS, and
##'        simulated data) should pass `1`. See
##'        [siblingsurvival::prep_dhs_sib_histories]
##' @return a prepped ego dataset, used by the `prep_*_sib_histories()` functions
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
