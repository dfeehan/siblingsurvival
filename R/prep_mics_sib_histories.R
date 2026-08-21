##' prepare a MICS dataset for analysis
##'
##' @param mm.df the MICS maternal mortality file (`mm.sav`), one row per
##'        reported sibling
##' @param survey string identifying the survey, eg `"ZW2019"`. Required: MICS
##'        has no `v000` equivalent, so this cannot be derived; see Details
##' @param wm.df optional women's file (`wm.sav`), joined on `id.vars` to bring
##'        across respondent attributes that `mm.df` does not carry
##' @param varmap see Details; defaults to `sibhist_varmap_mics6`
##' @param id.vars columns of `mm.df` that together identify a respondent.
##'        Combined to form `caseid`
##' @param doi.var column holding the date of interview as a CMC. Used when
##'        present; otherwise `doi.ym` is used
##' @param doi.ym year and month columns from which to construct a CMC date of
##'        interview, when `doi.var` is absent
##' @param dob.var column holding the respondent's date of birth as a CMC, used
##'        to derive respondent age
##' @param lowercase lowercase all variable names before matching the varmap?
##'        See Details
##' @param weight.scale divide the weight by this number. Defaults to `1`, since
##'        MICS weights are already normalized; see Details
##' @param add_maternal should maternal/pregnancy-related death info be added?
##' @param keep_missing should we keep reported sibs that are missing sex or survival status?
##' @param keep_varmap_only should we only keep ego variables mentioned in the varmap?
##' @param verbose report detailed summaries?
##' @return a list; see Details
##' @examples
##'   # TODO - write example code
##' @section Details:
##'
##' MICS publishes the sibling history as a dedicated file with **one row per
##' reported sibling**, unlike the DHS, which publishes a wide women's file with
##' one column per sibling attribute per sibling. So no reshape is needed, and
##' this function does not call [siblingsurvival::attributes.to.long].
##'
##' `survey` has no default because MICS has no `v000` equivalent. Supplying it
##' explicitly keeps MICS survey ids comparable with the DHS codes.
##'
##' `weight.scale` defaults to `1` because MICS weights (`wmweight`) are already
##' normalized to average 1. Do not pass `1e6` here: that is a DHS convention.
##'
##' MICS `.sav` files use **mixed** case -- questionnaire items and link keys are
##' uppercase (`MM15`, `HH1`, `WDOI`) while derived and design variables are
##' lowercase (`wmweight`, `psu`, `welevel`) -- so `lowercase = TRUE` normalizes
##' them and every shipped MICS varmap is written in lowercase.
##'
##' Several columns the rest of the pipeline requires are not in every MICS file
##' and are constructed here when absent:
##' * `caseid` -- from `id.vars`; MICS has no single respondent id
##' * `doi` -- from `doi.var`, else `doi.ym`. Must end up a CMC
##' * `age` -- respondent age, from `(doi - dob.var) / 12`
##' * `psu` -- from the first of `id.vars` (the cluster) when no `psu` column
##'   exists, which is the case in most MICS surveys
##' * `sex` -- constant `'f'`; MICS interviews only women
##'
##' `sib.dob` and `sib.death.date` are used when the varmap supplies them
##' (`MM17C`/`MM18C` in MICS6, `MM7C`/`MM8C` in MICS4/5) and approximated from
##' reported ages and years-since-death otherwise.
##'
##' `varmap` has the same format as the DHS varmaps: columns `orig.varname`,
##' `new.varname` and `sibvar`.
##'
##' Returns a list whose entries include
##' * `ego.dat` - dataset with information about the survey respondents
##' * `sib.dat` - dataset with information about the reported siblings
##' * `summ` - a one-row tibble with a summary of the data
##'
##' @export
##' @md
prep_mics_sib_histories <- function(mm.df,
                                    survey,
                                    wm.df=NULL,
                                    varmap=sibhist_varmap_mics6,
                                    id.vars=c('hh1', 'hh2', 'ln'),
                                    doi.var='wdoi',
                                    doi.ym=c('wm6y', 'wm6m'),
                                    dob.var='wdob',
                                    lowercase=TRUE,
                                    weight.scale=1,
                                    add_maternal=FALSE,
                                    keep_missing=FALSE,
                                    keep_varmap_only=FALSE,
                                    verbose=TRUE) {

  if (missing(survey) || is.null(survey) || length(survey) != 1) {
    stop("`survey` is required: MICS has no v000 equivalent, so the survey id ",
         "cannot be derived from the data. Pass something like survey='ZW2019'.\n")
  }

  #########################
  # normalize names
  #########################
  if (lowercase) {
    names(mm.df) <- tolower(names(mm.df))
    if (!is.null(wm.df)) names(wm.df) <- tolower(names(wm.df))
  }

  check_mics_varmap(varmap)

  ## ego (respondent) and sibling variables to grab
  tmp <- varmap %>% filter(sibvar==0)
  resp.attrib <- tmp$orig.varname
  names(resp.attrib) <- tmp$new.varname

  tmp <- varmap %>% filter(sibvar==1)
  sib.attrib <- tmp$orig.varname
  names(sib.attrib) <- tmp$new.varname

  #########################
  # optionally bring in the women's file
  #########################
  if (!is.null(wm.df)) {

    join.by <- intersect(id.vars, intersect(names(mm.df), names(wm.df)))

    if (length(join.by) == 0) {
      stop(glue::glue(
        "Cannot join wm.df to mm.df: none of id.vars ",
        "({paste0(id.vars, collapse=', ')}) is in both files.\n"))
    }

    ## don't clobber columns mm.df already has
    dup <- setdiff(intersect(names(mm.df), names(wm.df)), join.by)
    wm.df <- wm.df %>% select(-any_of(dup))

    if (verbose) {
      cat(glue::glue("\nJoining wm.df on {paste0(join.by, collapse=', ')}.\n"))
    }

    mm.df <- mm.df %>% left_join(wm.df, by=join.by)
  }

  #########################
  # report varmap columns that are missing
  #########################
  ## note the sibling data is already long, so sibling variables appear under
  ## their own names rather than as <prefix><sep><number> -- check them exactly
  miss_col <- list(
    ego = resp.attrib[which(! resp.attrib %in% names(mm.df))],
    sib = sib.attrib[which(! sib.attrib %in% names(mm.df))])

  if (verbose) {
    for (this in c('ego', 'sib')) {
      if (length(miss_col[[this]]) > 0) {
        cat(glue::glue("

                        Warning: {this} column(s) found in the varmap are missing in the dataset:
                        {paste0(miss_col[[this]], collapse=',')}
                        These will be ignored...

                        "))
      }
    }
  }

  #########################
  # construct what MICS does not supply
  #########################
  ## NB: before the varmap rename, since id.vars, doi.var and dob.var name
  ## columns as they appear in the raw file (hh1, ln, wdoi, wdob)
  mm.df <- build_mics_ego_cols(mm.df,
                               survey=survey,
                               id.vars=id.vars,
                               doi.var=doi.var,
                               doi.ym=doi.ym,
                               dob.var=dob.var,
                               verbose=verbose)

  #########################
  # rename via the varmap
  #########################
  ## don't rename onto a column that build_mics_ego_cols() just constructed --
  ## a varmap that maps eg wdoi -> doi would otherwise collide with the `doi`
  ## already built from it
  ren <- c(resp.attrib, sib.attrib)
  ren <- ren[! names(ren) %in% names(mm.df)]

  mm.df <- mm.df %>% rename(any_of(ren))

  #########################
  # recode sibling variables to the package's conventions
  #########################
  mm.df <- recode_mics_sib_vars(mm.df, verbose=verbose)

  #########################
  # prepare ego data
  #########################
  ## one row per respondent. resp.attrib has already been applied above, so
  ## get_ego_df() has nothing left to rename
  ego.cols <- intersect(c('caseid', 'survey', 'wwgt', 'psu', 'stratum', 'doi',
                          'age', 'sex', names(resp.attrib)),
                        names(mm.df))

  ego.raw <- mm.df %>%
    select(all_of(ego.cols)) %>%
    distinct(caseid, .keep_all=TRUE)

  ego.dat <- get_ego_df(ego.raw,
                        resp.attrib=character(0),
                        verbose=verbose,
                        weight.scale=weight.scale)

  cur.survey <- ego.dat$survey[1]

  #########################
  # prepare sibling data
  #########################
  ## keep the sibling columns plus the respondent key, then attach the prepped
  ## ego columns (which carry the scaled weights and the age categories)
  sib.cols <- intersect(names(sib.attrib), names(mm.df))

  long.dat <- mm.df %>%
    select(all_of(c('caseid', sib.cols))) %>%
    left_join(ego.dat, by='caseid')

  sib.dat <- get_sib_df(long.dat,
                        sib.attrib=sib.attrib,
                        verbose=verbose,
                        reshape=FALSE)

  #########################
  # add maternal vars, if needed
  #########################
  if (add_maternal) {
    if (verbose) cat("Adding pregnancy-related/maternal death info\n")
    sib.dat <- add_maternal_deaths(sib.dat, verbose=verbose)
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


##' recode MICS sibling variables to the conventions the package expects
##'
##' MICS and the DHS code survival status differently, and the difference is
##' silent: MICS `MM16` is 1 yes / 2 no / 8 don't know, while the DHS `mm2` is
##' 1 alive / 0 dead. Everything downstream filters on `sib.alive %in% c(0,1)`,
##' so passing MICS codes through unchanged drops **every dead sibling** as
##' though its survival status were missing -- which silently drives every
##' mortality estimate to zero.
##'
##' @param df the renamed MICS sibling data
##' @param verbose report the recode?
##' @return `df` with `sib.alive` recoded, and `sib.sex` validated
##'
recode_mics_sib_vars <- function(df, verbose=TRUE) {

  if ('sib.alive' %in% names(df)) {

    orig <- df$sib.alive
    bad <- setdiff(stats::na.omit(unique(orig)), c(1, 2, 8, 9))

    if (length(bad) > 0) {
      stop(glue::glue(
        "Unexpected values in sib.alive (MICS MM16): ",
        "{paste0(sort(bad), collapse=', ')}.
",
        "MICS codes this 1 = yes, 2 = no, 8 = don't know.
"))
    }

    ## 1 yes -> 1 alive; 2 no -> 0 dead; 8/9 don't know or missing -> NA,
    ## which the prep then drops unless keep_missing=TRUE
    df$sib.alive <- dplyr::case_when(orig == 1 ~ 1,
                                     orig == 2 ~ 0,
                                     TRUE      ~ NA_real_)

    if (verbose) {
      cat(glue::glue("

                      Recoded sib.alive from MICS codes (1 yes / 2 no / 8 DK) \\
                      to the package convention (1 alive / 0 dead / NA unknown): \\
                      {sum(orig == 1, na.rm=TRUE)} alive, \\
                      {sum(orig == 2, na.rm=TRUE)} dead, \\
                      {sum(is.na(df$sib.alive))} unknown.

                      "))
    }
  }

  ## MICS codes don't-know and no-response on the numeric sibling items as 98
  ## and 99, not as missing. Passed through as real values these are silently
  ## catastrophic: a sibling with sib.death.age = 98 would get a date of birth
  ## 98 years before her death, and sib.age = 99 would put a living sibling
  ## outside every age group. The damage is currently masked whenever MICS
  ## supplies its own imputed CMC dates (MM17C/MM18C, MM7C/MM8C), because then
  ## the derivations never fire -- but surveys such as BTN_2010 ship no CMC
  ## columns at all.
  dk.numeric <- intersect(c('sib.age', 'sib.death.yrsago', 'sib.death.age',
                            'sib.days.postpartum.death', 'sib.num.children'),
                          names(df))

  n.dk <- 0

  for (this.col in dk.numeric) {
    v <- df[[this.col]]
    hit <- v %in% c(98, 99)
    n.dk <- n.dk + sum(hit, na.rm=TRUE)
    df[[this.col]] <- ifelse(hit, NA_real_, as.numeric(v))
  }

  if (verbose && n.dk > 0) {
    cat(paste0("\nSet ", n.dk, " value(s) coded 98/99 (don't know / no response) ",
               "to NA across: ", paste0(dk.numeric, collapse=', '), ".\n"))
  }

  if ('sib.sex' %in% names(df)) {

    orig <- df$sib.sex
    bad <- setdiff(stats::na.omit(unique(orig)), c(1, 2, 8, 9))

    if (length(bad) > 0) {
      stop(glue::glue(
        "Unexpected values in sib.sex (MICS MM15): ",
        "{paste0(sort(bad), collapse=', ')}. ",
        "MICS codes this 1 = male, 2 = female, 9 = no response. Values other ",
        "than 1 or 2 would be silently recoded to male downstream."))
    }

    ## 8/9 are don't know / no response. get_sib_df() recodes with
    ## ifelse(sib.sex == 2, 'f', 'm'), which would turn these into males, so
    ## blank them here and let the missing-sex filter deal with them
    n.miss <- sum(orig %in% c(8, 9), na.rm = TRUE)
    df$sib.sex <- ifelse(orig %in% c(1, 2), orig, NA_real_)

    if (verbose && n.miss > 0) {
      cat(paste0("\nSet sib.sex to NA for ", n.miss,
                 " sibling(s) coded 8/9 (don't know / no response).\n"))
    }
  }

  return(df)
}


##' guard against the MM16 collision between MICS and the DHS
##'
##' In MICS6 `MM16` is "Is (name) still alive?"; in DHS-VII and later `mm16` is
##' "died of violence or an accident". Mapping one onto the other would silently
##' reclassify survival status as cause of death, so refuse to run.
##'
##' @param varmap the varmap to check
##' @return `varmap`, invisibly; called for the error
##'
check_mics_varmap <- function(varmap) {

  bad <- varmap %>%
    filter(tolower(orig.varname) == 'mm16',
           new.varname %in% c('sib.died.accident', 'sib.died.violence'))

  if (nrow(bad) > 0) {
    stop(glue::glue(
      "This varmap maps mm16 -> {bad$new.varname[1]}, which is the DHS meaning ",
      "of mm16 ('died of violence or an accident').\n",
      "In MICS6, MM16 is 'Is (name) still alive?' -- it should map to ",
      "sib.alive. The MICS cause-of-death items are MM26 (violence) and ",
      "MM27 (accident).\n"))
  }

  invisible(varmap)
}


##' construct the respondent-level columns MICS does not supply
##'
##' @param df the (renamed) MICS sibling file
##' @param survey the survey id
##' @param id.vars columns identifying a respondent
##' @param doi.var CMC date-of-interview column, used when present
##' @param doi.ym year and month columns, used when `doi.var` is absent
##' @param dob.var CMC respondent date-of-birth column
##' @param verbose report what was constructed?
##' @return `df` with `caseid`, `survey`, `doi`, `age`, `psu` and `sex` added
##'
build_mics_ego_cols <- function(df,
                                survey,
                                id.vars=c('hh1', 'hh2', 'ln'),
                                doi.var='wdoi',
                                doi.ym=c('wm6y', 'wm6m'),
                                dob.var='wdob',
                                verbose=TRUE) {

  built <- character(0)

  ## --- caseid ------------------------------------------------------------
  if (! 'caseid' %in% names(df)) {

    missing.id <- id.vars[! id.vars %in% names(df)]
    if (length(missing.id) > 0) {
      stop(glue::glue(
        "Cannot build caseid: id.vars not found in the data: ",
        "{paste0(missing.id, collapse=', ')}.\n",
        "The data has columns: {paste0(names(df), collapse=', ')}\n"))
    }

    df <- df %>%
      mutate(caseid = do.call(paste, c(across(all_of(id.vars)), sep='.')))
    built <- c(built, 'caseid')
  }

  ## --- survey ------------------------------------------------------------
  if (! 'survey' %in% names(df)) {
    df$survey <- survey
    built <- c(built, 'survey')
  }

  ## --- doi, as a CMC -----------------------------------------------------
  if (! 'doi' %in% names(df)) {

    if (!is.null(doi.var) && doi.var %in% names(df)) {

      df <- df %>% mutate(doi = as.numeric(.data[[doi.var]]))
      built <- c(built, paste0('doi (from ', doi.var, ')'))

    } else if (all(doi.ym %in% names(df))) {

      ## CMC: months since January 1900
      df <- df %>%
        mutate(doi = 12 * (as.numeric(.data[[doi.ym[1]]]) - 1900) +
                          as.numeric(.data[[doi.ym[2]]]))
      built <- c(built, paste0('doi (from ', paste0(doi.ym, collapse='+'), ')'))

    } else {
      stop(glue::glue(
        "Cannot build doi: neither '{doi.var}' nor all of ",
        "({paste0(doi.ym, collapse=', ')}) is in the data. doi has to be a CMC ",
        "(century month code), since the sibling date derivations are ",
        "arithmetic in months.\n"))
    }
  }

  ## --- respondent age ----------------------------------------------------
  if (! 'age' %in% names(df)) {

    if (!is.null(dob.var) && dob.var %in% names(df)) {

      ## age last birthday
      df <- df %>% mutate(age = floor((doi - as.numeric(.data[[dob.var]])) / 12))
      built <- c(built, paste0('age (from doi and ', dob.var, ')'))

    } else {
      stop(glue::glue(
        "Cannot build respondent age: no 'age' column and no '{dob.var}'.\n",
        "Some MICS files (eg MDG_2018) omit the respondent's CMC date of ",
        "birth; join the women's file via wm.df to supply it.\n"))
    }
  }

  ## --- psu ---------------------------------------------------------------
  if (! 'psu' %in% names(df)) {
    ## most MICS mm.sav files have no psu column; the cluster is the PSU
    df$psu <- df[[id.vars[1]]]
    built <- c(built, paste0('psu (from ', id.vars[1], ')'))
  }

  ## --- sex ---------------------------------------------------------------
  if (! 'sex' %in% names(df)) {
    ## MICS asks the sibling history of women only
    df$sex <- 'f'
    built <- c(built, 'sex (all respondents female)')
  }

  if (verbose && length(built) > 0) {
    cat(glue::glue("

                    Constructed: {paste0(built, collapse='; ')}

                    "))
  }

  return(df)
}
