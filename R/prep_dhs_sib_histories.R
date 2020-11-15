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

  ego.dat <- df %>%
    as_tibble() %>%
    # use information from the varmap to rename ego variables
    rename(!!!resp.attrib)

  ego.dat <- ego.dat %>%
    mutate(
      ## typically, only women are asked sibling histories in DHS surveys
      sex='f',
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

  sib.dat <- attributes.to.long(ego.dat,
                                attribute.prefix=sib.attrib,
                                ego.vars=c('caseid', 'wwgt',
                                           'psu', 'doi'),
                                idvar="caseid")

  sib.dat <- sib.dat %>%
    mutate(sib.sex = ifelse(sib.sex == 2, 'f', 'm'))

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
