## Re-exports of the estimator spine, which now lives in networkreporting.
##
## These functions were defined in this package through v0.3.0. They moved to
## networkreporting so that the tie-agnostic parts of the estimator could be
## shared with non-sibling ties (see networkreporting/dev/VISIBILITY-PLAN.md).
## They are re-exported here, unchanged, so that existing callers of
## siblingsurvival keep working without qualifying the package name.
##
## Nothing here wraps or alters behaviour -- each name is the networkreporting
## object itself.

##' @importFrom networkreporting occ.exp
##' @export
networkreporting::occ.exp

##' @importFrom networkreporting cell_config
##' @export
networkreporting::cell_config

##' @importFrom networkreporting make.age.groups
##' @export
networkreporting::make.age.groups

##' @importFrom networkreporting make.even.age.groups
##' @export
networkreporting::make.even.age.groups

##' @importFrom networkreporting make.time.periods
##' @export
networkreporting::make.time.periods

##' @importFrom networkreporting nmx_to_nqx
##' @export
networkreporting::nmx_to_nqx

##' @importFrom networkreporting q15_to_50
##' @export
networkreporting::q15_to_50

##' @importFrom networkreporting get_visibility
##' @export
networkreporting::get_visibility

##' @importFrom networkreporting sib_ic_checks
##' @export
networkreporting::sib_ic_checks
