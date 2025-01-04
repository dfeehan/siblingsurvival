##' prepare a DHS dataset for analysis
##'
##' @param df the raw DHS dataset (indvidual recode)
##' @param varmap see Details
##' @param keep_missing should we keep reported sibs that are missing sex or survival status?
##' @param keep_varmap_only should we only keep ego variables mentioned in the varmap? (Default: FALSE)
##' @param verbose report detailed summaries?
##' @return a list; see Details
##' @examples
##'   # TODO - write example code
##' @section Details:
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
##' * `sib.sex` (the sex of the sibling).
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
                                   keep_missing=FALSE,
                                   keep_varmap_only=FALSE,
                                   verbose=TRUE) {

  ## ego (respondent) variables to grab
  #tmp <- subset(varmap, sibvar==0)
  tmp <- varmap %>% filter(sibvar==0)
  resp.attrib <- tmp$orig.varname
  names(resp.attrib) <- tmp$new.varname

  ## alter (sibling) variables to grab
  #tmp <- subset(varmap, sibvar==1)
  tmp <- varmap %>% filter(sibvar==1)
  sib.attrib <- tmp$orig.varname
  names(sib.attrib) <- tmp$new.varname

  #########################
  # prepare ego data
  #########################

  ## in some cases, variables in the varmap may not be in the specific DHS dataset
  ## we are preparing (for example, some surveys don't have the 'literacy' variable, v155)
  ## in those cases, we print a message reporting this but proceed
  miss_col <- resp.attrib[which(! resp.attrib %in% names(df))]

  if(length(miss_col > 0)) {
    cat(glue::glue("

                    Warning: Column(s) found in the varmap are missing in the dataset:
                    {paste0(miss_col, collapse=',')}
                    These will be ignored...

                    "))
  }

  ego.dat <- df %>%
    as_tibble() %>%
    # use information from the varmap to rename ego variables
    #rename(!!!resp.attrib)
    rename(any_of(resp.attrib))

  if(is.null(ego.dat$sex)) {
    if(verbose) {
      cat(paste0("No information on respondent sex given; assuming all respondents are female.\n"))
    }

    ## typically, only women are asked sibling histories in DHS surveys
    ego.dat$sex <- 'f'
  }

  ego.dat <- ego.dat %>%
    mutate(
      ## for convenience, add 5- and 10-year age groups
      age.cat=forcats::fct_drop(cut(age,
                                    breaks=c(0, seq(from=15,to=50,by=5),95),
                                    include.lowest=TRUE, right=FALSE)),
      age.cat10=forcats::fct_drop(cut(age,
                                      breaks=c(0, seq(from=15,to=50,by=10),95),
                                      include.lowest=TRUE, right=FALSE)),
      ## we'll rescale the weights, dividing them by 1,000,000
      ## (so that their average is 1); see DHS documentation
      wwgt=wwgt/1e6
    )


  if (length(ego.dat$survey) == 0) {
    stop("There appears to be no 'survey' column in the ego dataset.\n")
  }
  cur.survey <- ego.dat$survey[1]

  #########################
  # prepare sibling data
  #########################

  sib.dat <- attributes.to.long(ego.dat,
                                attribute.prefix=sib.attrib,
                                ego.vars=c('caseid', 'wwgt',
                                           'psu', 'doi', 'sex'),
                                idvar="caseid")

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
    mutate(sib.dob = case_when((is.na(sib.dob) &
                               (! is.na(sib.death.yrsago)) &
                               (! is.na(sib.death.date))  ) ~
                                 approx_birth_date_from_death(sib.death.date,
                                                              sib.death.age),
                               TRUE ~ sib.dob))

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

  #########################
  # calculate some summary statistics
  #########################


  ## get some summary statistics
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

    cat(paste0(miss.sex, " out of ", n.sib.raw, " (", round(100*(pre.n-post.n)/pre.n,2), "%)",
               " reports about sibs have unknown sex.\n"))
  }

  sibs.removed.n <- 0
  sibs.removed.pct <- 0

  if(! keep_missing) {

    ## take siblings missing sex and missing survival status out of the analysis,
    cat("Removing reported sibs missing survival status or sex.\n")
    pre.n <- nrow(sib.dat)
    sib.dat <- sib.dat %>% filter(sib.alive %in% c(0,1)) %>% filter(sib.sex %in% c('f', 'm'))
    post.n <- nrow(sib.dat)
    sibs.removed.n <- pre.n - post.n
    sibs.removed.pct <- 100 * sibs.removed.n / n.sib.raw

    cat("... this removes ", pre.n-post.n, " out of ", pre.n," (", round(100*(pre.n-post.n)/pre.n,2), "%)",
        " sibling reports.")
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
                 sibs.removed.pct = sibs.removed.pct)

  if (keep_varmap_only) {
    ego.dat <- ego.dat %>%
      select_at(c(names(resp.attrib), 'sex', 'age.cat', 'age.cat10', 'wwgt'))
  }

  return(list(survey=cur.survey,
              ego.dat = ego.dat,
              sib.dat = sib.dat,
              summ = summ))
}
