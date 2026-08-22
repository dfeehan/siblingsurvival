## prep_nrsim_sib_histories(): prepare simulated (nrsimulatr-style) sibling
## history data. Unlike the DHS and MICS preps this one has no helpers of its
## own -- it is a thin, generic wrapper over the shared internals in
## prep_dhs_sib_histories.R and finalize_sib_prep.R.

##' prepare a dataset from nrsimulatr for sibling analysis
##'
##' @param df the raw dataset (each row is a survey response)
##' @param varmap see Details; defaults to NULL
##' @param keep_missing should we keep reported sibs that are missing sex or survival status?
##' @param keep_varmap_only should we only keep ego variables mentioned in the varmap? (Default: FALSE)
##' @param weight.scale divide the weight by this number. Defaults to `1`, since
##'        non-DHS weights are typically already normalized; see Details
##' @param death.exposure whether a sibling who died contributes the month of
##'        death as exposure; `"dhs"` (default) counts it, `"mics"` stops the
##'        month before. See [siblingsurvival::get_sib_df]
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
                                     death.exposure=c("dhs", "mics"),
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

  sib.dat <- get_sib_df(ego.dat, sib.attrib, verbose,
                        death.exposure = death.exposure)

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
