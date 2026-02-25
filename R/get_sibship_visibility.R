##' calculate number of sibs on frame for each respondent
##'
##' this quantity, y.F, is related to the visibility of each respondent
##'
##' @param sib.dat The long-form sibling dataset (likely produced by [siblingsurvival::prep_dhs_sib_histories])
##' @param ego.id  String with the name of the column in \code{sib.dat} containing the survey respondent ID
##' @param sib.frame.indicator String with the name of the column in \code{sib.dat} containing a 0/1 coded variable indicating whether or not each sib is in the frame population
##' @return A tibble with a row for each survey respondent (each unique value of \code{ego.id}), and the number of sibs the respondent reported on the frame, including and not including herself
##' @examples
##'   # TODO write example code
get_sibship_info <- function(sib.dat,
                             ego.id,
                             sib.frame.indicator) {

  sib.dat <- sib.dat %>% rename(.ego.id = !!sym(ego.id),
                                .sib.in.F = !!sym(sib.frame.indicator))

  # y_F for each sibship, based on summing reports across cells
  vis.dat <- sib.dat %>%
    group_by(.ego.id) %>%
    summarize(# number of sibs in the sampling frame
              y.F = sum(.sib.in.F),
              # total size of sibship, which is number of reported
              # siblings plus one (for the respondent)
              sib.size = n() + 1) %>%
    # number of sibs in the sampling frame, including respondent
    mutate(yprime.F = y.F + 1)

  vis.dat <- vis.dat %>% rename(!!ego.id := .ego.id)

  return(vis.dat)
}

##' calculate visibility for each sibship and ego
##'
##' @param ego.dat The ego dataset (likely produced by [siblingsurvival::prep_dhs_sib_histories])
##' @param ego.id  String with the name of the column in \code{sib.dat} containing the survey respondent ID
##' @param sib.dat The long-form sibling dataset (likely produced by [siblingsurvival::prep_dhs_sib_histories])
##' @param sib.frame.indicator String with the name of the column in \code{sib.dat} containing a 0/1 coded variable indicating whether or not each sib is in the frame population
##' @param weight string with the name of the column in \code{ego.dat} and \code{sib.dat} containing the sampling weight. Defaults to `wwgt`
##' @param age string with the name of the column in \code{ego.dat} containing the age group. Defaults to `age.cat`
##' @return A list with three entries:
##'   * `ego_vis` - a tibble with one row per ego and the ego-specific visibilities
##'   * `ego_vis_agg` - a tibble with summarized adjustment factors
##'   * `sib_res` - a tibble with one row per reported sibling, along with
##'   tibble with a row for each survey respondent (each unique value of \code{ego.id}),
##'   and the number of sibs the respondent reported on the frame, including and not including herself
##' @examples
##'   # TODO write example code
##' @export
get_visibility <- function(ego.dat,
                           ego.id,
                           sib.dat,
                           sib.frame.indicator,
                           weight='wwgt',
                           age='age.cat') {

  sib.dat <- sib.dat %>%
    # rename the ego id and sibling frame indicator variables
    # to make the code more flexible
    rename(.ego.id = !!sym(ego.id),
           .sib.in.F = !!sym(sib.frame.indicator),
           .weight = !!sym(weight))
           #.agecat = !!sym(age))

  ego.dat <- ego.dat %>%
    rename(.ego.id=!!sym(ego.id),
           .weight = !!sym(weight),
           .agecat = !!sym(age))

  ###################################
  ## calculate quantities related to visibility for each respondent's sibship

  # for each sibship, calculate the number of reported sibs on the frame
  # and the size of the sibship
  sib_F_dat <- get_sibship_info(sib.dat,
                                ego.id='.ego.id',
                                sib.frame.indicator='.sib.in.F')

  #sib_F_dat <- sib.dat %>%
  #  group_by(.ego.id) %>%
  #  summarize(# y.F is the number of siblings in the frame population
  #            # reported by the respondent
  #            # (note that dead sibs are never in the frame popn)
  #            y.F = sum(.sib.in.F),
  #            # total size of sibship, which is number of reported
  #            # siblings plus one (for the respondent)
  #            sib.size = n() + 1)

  # add the sibling size to the ego data
  ego_vis <- ego.dat %>%
    select(.ego.id, .weight, .agecat, sex) %>%
    left_join(sib_F_dat, by='.ego.id')

  # if nothing was joined in, there are no sibs
  ego_vis <- ego_vis %>%
    mutate(y.F=ifelse(is.na(y.F), 0, y.F),
           yprime.F = ifelse(is.na(y.F), 1, y.F + 1),
           #y.Fplusone = y.F + 1,
           # if no sibs were reported, the sibship size is 1
           # (just the respondent)
           sib.size = ifelse(is.na(sib.size), 1, sib.size))

  sib_res <- sib.dat %>%
    left_join(ego_vis)

  #add_sib_ind_vis <- function(esc.dat,
  #                            ego.id,
  #                            sib.dat,
  #                            sib.frame.indicator,
  #                            varname='ind_vis') {


  ###################################
  ## calculate summaries + adjustment factors based on the
  ## ego-specific visibilities

  # weighted harmonic mean
  wh.mean <- function(x, w) {
    return(sum(w) / sum(w/x))
  }

  # TODO comment
  S.hat <- wh.mean((ego_vis$y.F + 1),
                   ego_vis$.weight)
  S.adj.factor <- 1 - (1/S.hat)

  # TODO comment
  y.F.bar <- weighted.mean(ego_vis$y.F,
                           ego_vis$.weight)
  approx.S.hat <- y.F.bar + 1
  approx.S.adj.factor <- 1 - (1/approx.S.hat)

  # TODO comment
  ego_vis_agg <- ego_vis %>%
    mutate(.agecat = paste(.agecat)) %>%
    group_by(sex, .agecat) %>%
    summarise(# this is the average y.F value across egos
              y.F.bar = weighted.mean(y.F, .weight),
              # this is the average sibship size (which will be
              # size-biased)
              avg.sib.size = weighted.mean(sib.size, .weight)) %>%

    mutate(adj.factor = S.adj.factor,
           # this is the all-ages approximation
           adj.factor.allage = approx.S.adj.factor,
           # this is the age-specific approximation
           adj.factor.agespec = y.F.bar / (y.F.bar + 1))

  #asdr.agg.dat <- asdr.agg.dat %>%
  #  rename(!!sib.sex := .sib.sex,
  #         sib.age = agelabel)
  ego_vis <- ego_vis %>%
    rename(!!ego.id := .ego.id,
           !!weight := .weight,
           !!age := .agecat)

  ego_vis_agg <- ego_vis_agg %>%
    rename(!!age := .agecat)

  return(lst(ego_vis,
             ego_vis_agg,
             sib_res))

}


##' get_ego_age_distribution
##'
##' @param ego.dat the ego dataset (probably from [siblingsurvival::prep_dhs_sib_histories])
##' @param only_females should only females be used to calculate age distribution? (default: True)
##'
##' @return dataframe with distribution of respondent ages by 5-year category,
##' typically used in calculating the maternal or pregnancy-related mortality
##' @section Details:
##' `ego_dat` is assumed to have two columns: `wwgt` and `age.cat`
get_ego_age_distn <- function(ego.dat,
                              only_females = TRUE) {

  if(only_females) {
    ego.dat <- ego.dat %>% filter(sex == 'f')
  }

  respondent_age <- ego.dat %>%
    ## age.cat and wwgt are assumed to come with the dataset; they have
    ## ego age in 5-year groups and the women's weight
    filter(age.cat %in% c("[15,20)",
                          "[20,25)",
                          "[25,30)",
                          "[30,35)",
                          "[35,40)",
                          "[40,45)",
                          "[45,50)")) %>%
    group_by(age.cat) %>%
    # note that [siblingsurvival::prep_dhs_sib_histories]
    # will have already scaled these weights
    summarize(total = sum(wwgt))

  respondent_age$agegrp_prop <- respondent_age$total / (sum(respondent_age$total))

  return(respondent_age)
}


##' add individual visibility based on sib reprots to ego X sib X cell reports
##'
##' Takes a dataframe that has a row for each respondent X sib X cell
##' and adds individual visibility to it
##'
##' @param esc.dat Dataset with a row for each respondent X sibling X cell, likely produced by \code{\link{get_esc_reports}}
##' @param ego.id  String with the name of the column in \code{esc.dat} containing the survey respondent's id
##' @param sib.dat Dataset with a row for each reported sibling, likely produced by TODO
##' @param sib.frame.indicator String with the name of the column in \code{sib.dat} containing a 0/1 coded variable indicating whether or not each sib is in the frame population
##' @param varname String with the name of the new column to be added to \code{sib.dat} containing the individual visibility
##' @return the ESC dataframe with additional columns that have information about the sibship size and visibility:
##'   - \code{ind_vis_weight} - the individual visibility weight
##'   - \code{y.F} - number of reported sibs in frame popn (not including respondent)
##'   - \code{sibship.size} - total size of reported sibship (including respondent)
##' @examples
##'   # TODO - add example
add_esc_ind_vis <- function(esc.dat,
                            ego.id,
                            sib.dat,
                            sib.frame.indicator,
                            varname='ind_vis') {

  esc.dat <- esc.dat %>% dplyr::rename(.ego.id = !!sym(ego.id),
                                       .sib.in.F = !!sym(sib.frame.indicator))

  sib.dat <- sib.dat %>% dplyr::rename(.ego.id = !!sym(ego.id))

  # calculate y.F, which is closely related to the visibility of each sibship
  yFdat <- get_sibship_info(sib.dat,
                            ego.id=".ego.id",
                            sib.frame.indicator=".sib.in.F")

  # add individual visibility weights to the esc data
  # (these are sibling individual visibility weights)
  esc.dat.with.indviswgt <- esc.dat %>%
    left_join(yFdat %>% select(.ego.id, y.F),
              by='.ego.id') %>%
    calculate_sib_ind_visibility(sib.frame.indicator = '.sib.in.F',
                                 num.sibs.on.frame.var = 'y.F',
                                 varname = 'ind_vis')
    #mutate(ind_vis_weight = case_when(.sib.in.F == 1 ~ 1 / y.F,
    #                                  .sib.in.F == 0 ~ 1 / (y.F + 1)))

  if (any(is.na(esc.dat.with.indviswgt$ind_vis))) {
    stop("esc data has rows for which we have no individual visibility weight. something must be wrong. is there any missingness in the indicator variable for sibling frame membership?")
  }

  esc.dat.with.indviswgt <- esc.dat.with.indviswgt %>%
    dplyr::rename(!!ego.id := .ego.id,
                  !!sib.frame.indicator := .sib.in.F)

  return(esc.dat.with.indviswgt)

}


##' given a sib dataset, calculate individual visibility weight for each sib
##'
##' @param df Dataset on sibling reports (possibly by cell)
##' @param sib.frame.indicator String with the name of the column in \code{df} containing a 0/1 coded variable indicating whether or not each sib is in the frame population
##' @param num.sibs.on.frame.var String with the name of the column in \code{df} containing the number of sibs on the frame for each ego
##' @param varname String with the name of the new column to create with the individual visibility of each sib
##' @return \code{df} with a new column that has the individual visibility of each sibling
calculate_sib_ind_visibility <- function(df,
                                         sib.frame.indicator='.sib.in.F',
                                         num.sibs.on.frame.var='y.F',
                                         varname = 'ind_vis') {

  df <- df %>%
    dplyr::rename(.sib.in.F = !!sym(sib.frame.indicator),
                  .y.F = !!sym(num.sibs.on.frame.var))

  # individual visibility weight depends on whether the sib is on the frame
  # if yes, then individual vis weight is y.F.
  # if no (including if sib is dead), it is y.F + 1
  df <- df %>%
    mutate(.ind_vis_weight = case_when(.sib.in.F == 1 ~ 1 / .y.F,
                                       .sib.in.F == 0 ~ 1 / (.y.F + 1)))

  df <- df %>%
    rename(!!sib.frame.indicator := .sib.in.F,
           !!num.sibs.on.frame.var := .y.F,
           !!varname := .ind_vis_weight)

  return(df)

}
