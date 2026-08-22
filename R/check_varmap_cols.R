##' report varmap columns that are missing from a dataset
##'
##' In some cases, variables in the varmap will not be in the specific dataset
##' being prepared (for example, some DHS surveys don't have the 'literacy'
##' variable, v155). In those cases we report the missing columns and proceed.
##'
##' Ego (respondent) variables are matched by exact name. Sibling variables are
##' matched as *prefixes*, since the wide-form data has one column per reported
##' sibling: the varmap entry `mm1` corresponds to columns `mm1_1`, `mm1_2`,
##' and so on. The regular expression used here has to match the one used by
##' [siblingsurvival::attributes.to.long], which is what actually reshapes those
##' columns.
##'
##' @param df the survey dataset
##' @param resp.attrib named vector of ego (respondent) variables from the varmap
##' @param sib.attrib named vector of sibling variables from the varmap
##' @param sep regular expression separating the sibling variable prefix from the
##'        sibling number; must match the `sep` passed to
##'        [siblingsurvival::attributes.to.long]
##' @param verbose print a message describing the missing columns?
##' @return a list with entries `ego` and `sib`, each a (possibly empty) named
##'         vector of varmap columns that were not found in `df`
##'
check_varmap_cols <- function(df,
                              resp.attrib,
                              sib.attrib,
                              sep="\\.|_",
                              verbose=TRUE) {

  cn <- names(df)

  ## ego variables appear in the data under their own name
  miss_ego <- resp.attrib[which(! resp.attrib %in% cn)]

  ## sibling variables appear as <prefix><sep><sibling number>, so a sibling
  ## variable is present if at least one column matches its prefix
  if (length(sib.attrib) > 0) {
    sib_found <- vapply(sib.attrib,
                        function(this.prefix) {
                          any(stringr::str_detect(
                            cn,
                            paste0("^", this.prefix, "(", sep, ")(.+)")))
                        },
                        logical(1))
    miss_sib <- sib.attrib[! sib_found]
  } else {
    miss_sib <- sib.attrib
  }

  if (verbose) {

    if (length(miss_ego) > 0) {
      cat(glue::glue("

                      Warning: Respondent column(s) found in the varmap are missing in the dataset:
                      {paste0(miss_ego, collapse=',')}
                      These will be ignored...

                      "))
    }

    if (length(miss_sib) > 0) {
      cat(glue::glue("

                      Warning: Sibling column(s) found in the varmap are missing in the dataset:
                      {paste0(miss_sib, collapse=',')}
                      These will be ignored...

                      "))
    }

  }

  return(list(ego = miss_ego,
              sib = miss_sib))
}
