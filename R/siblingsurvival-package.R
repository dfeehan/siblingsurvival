## usethis namespace: start
#' @import dplyr
## The tie-agnostic estimator spine now lives in networkreporting. These are the
## pieces the sibling-specific code here calls directly; the public names are
## re-exported from R/reexports.R instead.
#' @importFrom networkreporting get_esc_reports get_ec_reports add_esc_ind_vis
#' @importFrom networkreporting get_ind_est_from_ec get_agg_est_from_ec get_boot_ests_matrix
#' @importFrom stats quantile sd weighted.mean
#' @importFrom rlang sym ':='
## usethis namespace: end
NULL
