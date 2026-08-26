##' Estimate death rates from sibling history data
##'
##' @param sib.dat The long-form sibling history dataset (likely produced by [prep_dhs_sib_histories])
##' @param ego.id  String with the name of the column of \code{sib.dat} that has the ID of the survey respondent
##' @param sib.id  String with the name of the column of \code{sib.dat} that has the sibling ID. Defaults to \code{'sibid'}, which is the column created by \code{\link{prep_dhs_sib_histories}} and \code{\link{prep_nrsim_sib_histories}}.
##' @param sib.frame.indicator String with the name of the column in \code{sib.dat} containing a 0/1 coded variable indicating whether or not each sib is in the frame population
##' @param sib.sex String with the name fo the column of \code{sib.dat} that has the sibling's sex
##' @param cell.config An object containing the configuration of cells; see TODO for more information
##' @param weights String with the name of the column of \code{sib.dat} that has the sampling weight
##' @param boot.weights Optional dataframe with bootstrap resampled weights. See Details for more info.
##' @param return.boot If TRUE, and if \code{boot.weights} is specified, then return each bootstrap estimate
##' @param visibility A visibility rule saying how each reported sibling's
##'        visibility is derived. Defaults to
##'        [networkreporting::vis_from_clique()], the exact rule this function
##'        has always applied, so the default changes nothing. See
##'        [networkreporting::vis_from_donor()] and
##'        [networkreporting::vis_coalesce()] for the approximating rules that
##'        non-clique ties need.
##' @param tie What kind of tie the reports are about, as a
##'        [networkreporting::tie_config()]. Defaults to
##'        `tie_config("clique", name = "siblings")`, which is what siblings
##'        are, so the default changes nothing.
##'
##'        **Set this if you are using this function for a tie that is not a
##'        clique.** It is used for maternal cousins and cousins-plus-siblings
##'        in the socsim work, and cousinship is not transitive, so the default
##'        clique rule silently overstates visibility there --- by 1.55x for
##'        off-frame alters against 1.29x for on-frame ones, measured against
##'        socsim ground truth. Because a death is always off-frame while
##'        exposure is a mixture, that differential biases the rate rather than
##'        cancelling. Declaring `tie_config("group")` makes the clique rule
##'        refuse rather than mislead.
##' @param discretize.exp Boolean for whether or not expsoure should be discretized. Not yet implemented.
##' @return a list with two entries: \code{asdr.ind}, individual visibility asdr estimates; and \code{asdr.agg}, aggregate visibility asdr estimates
##' @seealso [networkreporting::network_survival_estimator()], which this wraps
##'
##' @section Details:
##' If you want estimated sampling variances, you can pass in a data frame \code{boot.weights}.
##' \code{boot.weights} is assumed to have a column that is named whatever the \code{ego.id} is,
##' and then a series of columns named \code{boot_weight_1}, ..., \code{boot_weight_M}.
##'
##' @export
sibling_estimator <- function(sib.dat,
                              # the name of the id of the ego in the sibling histories
                              ego.id,
                              # the name of the id of the sib in the sibling histories;
                              # 'sibid' is what the prep_*_sib_histories functions create
                              sib.id = 'sibid',
                              # the name of the indicator for whether or not each sib is on the
                              # frame
                              sib.frame.indicator,
                              # variable for sibling's sex
                              sib.sex = 'sex',
                              cell.config,
                              weights,
                              boot.weights = NULL,
                              return.boot = FALSE,
                              # how each reported sibling's visibility is derived.
                              # the default is the exact clique rule, which is what
                              # this function has always used
                              visibility = networkreporting::vis_from_clique(),
                              # what kind of tie these reports are about. siblings are
                              # a clique, which is why the clique rule is exact here.
                              # override for cousins and other non-clique ties.
                              tie = networkreporting::tie_config('clique',
                                                                 name = 'siblings'),
                              # by default, we report continuous exposure (ie, number of months of exposure)
                              # but the formal results are based on exposed/not exposed; use this setting to
                              # discretize exposure
                              discretize.exp=FALSE) {

  ## This is networkreporting::network_survival_estimator() with the sibling
  ## names and the clique tie filled in. The estimator itself is not
  ## sibling-specific -- nothing in the pipeline assumes anything about
  ## siblings, only about a tie -- so it lives there and is called from here,
  ## rather than existing twice and drifting.
  ##
  ## Two things this wrapper is responsible for, and they are the only reasons
  ## it is more than an alias:
  ##
  ##  1. The clique tie. Siblings ARE a clique, so the default is right here,
  ##     whereas the generic deliberately has no default: applicability cannot
  ##     be read off the data, and a wrong default would silently bias a rate.
  ##  2. The output column names. Callers of this function have always got back
  ##     `sib.age` and their own sex column; the generic returns `alter.age`.

  res <- networkreporting::network_survival_estimator(
    rel.dat         = sib.dat,
    ego.id          = ego.id,
    alter.id        = sib.id,
    frame.indicator = sib.frame.indicator,
    alter.sex       = sib.sex,
    cell.config     = cell.config,
    weights         = weights,
    boot.weights    = boot.weights,
    return.boot     = return.boot,
    visibility      = visibility,
    tie             = tie,
    discretize.exp  = discretize.exp,
    ## so a mismatched column is reported in the names the caller actually used
    .arg.labels     = c(alter.id        = 'sib.id',
                        frame.indicator = 'sib.frame.indicator',
                        alter.sex       = 'sib.sex'),
    .data.label     = 'sib.dat')

  ## alter.age -> sib.age, everywhere it appears
  rename_age <- function(x) {
    if (is.data.frame(x) && 'alter.age' %in% names(x)) {
      x <- dplyr::rename(x, sib.age = 'alter.age')
    }
    x
  }
  keep <- setdiff(names(res), 'vis_provenance')
  res[keep] <- lapply(res[keep], rename_age)

  return(res)
}
