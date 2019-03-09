##' calculate visibility for each sibship
##'
##' @param sib.dat The long-form sibling dataset (likely produced by TODO)
##' @param ego.id  String with the name of the column in \code{sib.dat} containing the survey respondent ID
##' @param sib.frame.indicator String with the name of the column in \code{sib.dat} containing a 0/1 coded variable indicating whether or not each sib is in the frame population
##' @return A tibble with a row for each survey respondent (each unique value of \code{ego.id}), and the number of sibs the respondent reported on the frame, including and not including herself
##' @examples
##'   # TODO write example code
get_sibship_visibility <- function(sib.dat,
                                   ego.id,
                                   sib.frame.indicator) {

  sib.dat <- sib.dat %>% rename(.ego.id = !!sym(ego.id),
                                .sib.in.F = !!sym(sib.frame.indicator))

  # visibility for each sibship, based on summing reports across cells
  vis.dat <- sib.dat %>%
    group_by(.ego.id) %>%
    summarize(y.F = sum(.sib.in.F)) %>%
    mutate(yprime.F = y.F + 1)

  vis.dat <- vis.dat %>% rename(!!ego.id := .ego.id)

  return(vis.dat)
}
