##' the reproductive age groups used for maternal mortality estimates
##'
##' Maternal and pregnancy-related mortality quantities are conventionally
##' defined over women of reproductive age, 15 to 49. This function is the
##' single definition of those age groups, so that the respondent age
##' distribution ([siblingsurvival::get_ego_age_distn]) and the age-specific
##' estimates being aggregated ([siblingsurvival::aggregate_maternal_estimates])
##' cannot drift apart.
##'
##' The labels match the ones produced by `cut(..., right=FALSE)` and by
##' [siblingsurvival::make.even.age.groups], which is what both the ego age
##' categories (`age.cat`) and the estimate age groups (`sib.age`) use.
##'
##' @return a character vector with the seven 5-year age group labels covering
##'         ages 15 through 49
##' @examples
##'   reproductive_age_groups()
##' @export
##' @md
reproductive_age_groups <- function() {
  c("[15,20)",
    "[20,25)",
    "[25,30)",
    "[30,35)",
    "[35,40)",
    "[40,45)",
    "[45,50)")
}
