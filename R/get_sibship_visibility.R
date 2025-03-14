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
##' @return A list with two entries:
##'   * `ego_vis_agg` - a tibble with summarized adjustment factors
##'   * `ego_vis` - a tibble with one row per ego and the ego-specific visibilities
##'   tibble with a row for each survey respondent (each unique value of \code{ego.id}),
##'   and the number of sibs the respondent reported on the frame, including and not including herself
##' @examples
##'   # TODO write example code
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
                                ego.dat,
                                sib.frame.indicator)

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
           y.Fplusone = y.F + 1,
           sib.size = ifelse(is.na(sib.size), 1, sib.size))

  sib_res <- sib.dat %>%
    left_join(ego_vis)

  ###################################
  ## calculate summaries + adjustment factors based on the
  ## ego-specific visibilities

  # weighted harmonic mean
  wh.mean <- function(x, w) {
    return(sum(w) / sum(w/x))
  }

  # TODO comment
  S.hat <- wh.mean(ego_vis$y.Fplusone, ego_vis$.weight)
  S.adj.factor <- 1 - (1/S.hat)

  # TODO comment
  y.F.bar <- weighted.mean(ego_vis$y.F, ego_vis$.weight)
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
##' @param ego_dat the ego dataset (probably from [siblingsurvival::prep_dhs_sib_histories])
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



