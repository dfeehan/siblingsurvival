##' get a dataset with reports used for internal-consistency checks
##'
##' @param esc.dat The ego X sibling X cell dataset (see \link{\code{get_esc_reports}})
##' @param ego.dat The ego dataset, containing one row for each survey respondent
##' @param ego.id  String with the name of the column in \code{esc.dat} containing the survey respondent ID
##' @param sib.id  String with the name of the column in \code{esc.dat} containing the unique sibling ID
##' @param sib.frame.indicator String with the name of the column in \code{sib.dat} containing a 0/1 coded variable indicating whether or not each sib is in the frame population
##' @param sib.cell.vars see Details
##' @param ego.cell.vars see Details
##' @return A tibble with a row for each reported sibling in the frame population, the sibling's cell info,
##' the cell info of the survey respondent who reported each sibling, and unified variables with the ego and sibs'
##' cell values; these unified variables are called \code{.ego.cell} and \code{.sib.cell}, and are used in
##' \link{\code{sib_ic_checks}}.
##' @examples
##'   # TODO write example code
##' @section Details:
##'   The \code{sib.cell.vars} and \code{ego.cell.vars} arguments should have a vector with Strings containing the names of
##'   columns in \code{esc.dat} identifying the cells to group reports by (typically age and sex, and possibly other variables).
##'   Note that, unlike the \code{cell.vars} argument in \link{\code{get_ec_reports}}, for this function \code{sib.cell.vars} and
##'   \code{ego.cell.vars} MUST have the age variable listed FIRST.
##'   The goal of \code{get_ic_reports} is to find the cell that each sibling was in at the time of the interview, meaning the oldest age group to which the
##'   sibling contributed any exposure.
get_ic_reports <- function(esc.dat,
                           ego.dat,
                           ego.id,
                           sib.id,
                           sib.frame.indicator,
                           sib.cell.vars,
                           ego.cell.vars) {

  esc.dat <- esc.dat %>% dplyr::rename(.ego.id = !!sym(ego.id),
                                       .sib.id = !!sym(sib.id),
                                       .sib.in.F = !!sym(sib.frame.indicator))

  ec.yf.dat <- esc.dat %>%
    ## we want sibs on the frame,
    ## and we want age groups to which they contributed some
    ## exposure
    filter(.sib.in.F == 1, sib.exp > 0) %>%
    ## within each sib, we want to pick the latest age group
    ## they contributed exposure to
    group_by_at(c('.sib.id', sib.cell.vars[-1])) %>%
    ## NB: assumes agegroup, when arranged, is in increasing
    ##     order of age, ie, age 15 precedes age 20, etc...
    arrange(desc(agegroup)) %>%
    slice(1)

  ## join in the ego data
  ec.yf.dat <- ec.yf.dat %>%
    left_join(ego.dat %>%
                dplyr::rename(.ego.id = !!sym(ego.id)) %>%
                select(.ego.id, !!!ego.cell.vars), by='.ego.id')

  ec.yf.dat <- ec.yf.dat %>% rename(!!ego.id := .ego.id,
                                    !!sib.id := .sib.id,
                                    !!sib.frame.indicator := .sib.in.F)
  return(ec.yf.dat)
}

##' get calculate internal consistency checks for sibling reports
##'
##' @param esc.dat The ego X sibling X cell dataset (see \code{get_esc_reports})
##' @param ego.dat The ego dataset, containing one row for each survey respondent
##' @param ego.id  String with the name of the column in \code{esc.dat} containing the survey respondent ID
##' @param sib.id  String with the name of the column in \code{esc.dat} containing the unique sibling ID
##' @param sib.frame.indicator String with the name of the column in \code{sib.dat} containing a 0/1 coded variable indicating whether or not each sib is in the frame population
##' @param sib.cell.vars see Details
##' @param ego.cell.vars see Details
##' @param boot.weights Dataframe with bootstrap resampled weights. See Details
##' @return A list with two entries: \code{ic.summ}, with summarized results; and \code{ic.boot.ests} with the full results for each bootstrap rep
##' @examples
##'   # TODO write example code
##' @section Details:
##'   The \code{sib.cell.vars} and \code{ego.cell.vars} arguments should have a vector with Strings containing the names of
##'   columns in \code{esc.dat} identifying the cells to group reports by (typically age and sex, and possibly other variables).
##'   Note that, unlike the \code{cell.vars} argument in \code{get_ec_reports}, for this function \code{sib.cell.vars} and
##'   \code{ego.cell.vars} MUST have the age variable listed FIRST.
##'   The goal of \code{get_ic_reports} is to find the cell that each sibling was in at the time of the interview, meaning the oldest age group to which the
##'   sibling contributed any exposure.
##'   \code{boot.weights} is assumed to have a column that is named whatever the \code{ego.id} is,
##'   and then a series of columns named \code{boot_weight_1}, ..., \code{boot_weight_M}.
##'
##' @export
sib_ic_checks <- function(esc.dat,
                          ego.dat,
                          ego.id,
                          sib.id,
                          sib.frame.indicator,
                          sib.cell.vars,
                          ego.cell.vars,
                          boot.weights) {

  ec.yf.dat <- get_ic_reports(esc.dat,
                              ego.dat,
                              ego.id,
                              sib.id,
                              sib.frame.indicator,
                              sib.cell.vars,
                              ego.cell.vars)

  # encode the cells into one variable, making it easier to work with
  ec.yf.dat <- ec.yf.dat %>%
    ungroup() %>%
    encode_cells('.ego.cell', ego.cell.vars) %>%
    encode_cells('.sib.cell', sib.cell.vars)

  # check that the levels are the same
  if(! setequal(unique(ec.yf.dat$.ego.cell), unique(ec.yf.dat$.sib.cell))) {
    warning("The cell values do not appear to be the same for ego and for sibs.")
  }

  cell.vals <- unique(ec.yf.dat$.ego.cell)

  M <- ncol(boot.weights) - 1

  ec.yf.dat <- ec.yf.dat %>%
    #left_join(boot.weights %>% dplyr::mutate(.ego.id = !!sym(ego.id)), by='.ego.id')
    left_join(boot.weights, by=ego.id)

  boot.cols <- stringr::str_subset(colnames(boot.weights), ego.id, negate=TRUE)

  # we'll map over bootstrap resamples and cells, calculating an
  # IC check for each bootstrap rep X cell
  boot.ests <- purrr::map_df(cell.vals,
                             function(cur_cell) {

                               # egos in group alpha
                               y.Falpha.dat <- ec.yf.dat %>%
                                 grab_cell('.ego.cell', cur_cell)
                               # reports from egos in group alpha about sibs in -alpha
                               y.Falpha.Fminusalpha.dat <- y.Falpha.dat %>%
                                 grab_cell_complement('.sib.cell', cur_cell)

                               # egos not in group alpha
                               y.Fminusalpha.dat <- ec.yf.dat %>%
                                 grab_cell_complement('.ego.cell', cur_cell)
                               # reports from egos not in group alpha about sibs in alpha
                               y.Fminusalpha.Falpha.dat <- y.Fminusalpha.dat %>%
                                 grab_cell('.sib.cell', cur_cell)

                               #####
                               ## size of frame (for normalization)

                               N.Falpha <- y.Falpha.dat %>%
                                 summarize_at(.vars=boot.cols,
                                              sum) %>%
                                 tidyr::gather(starts_with('boot_weight'),
                                               key='qty',
                                               value='N.Falpha') %>%
                                 mutate(boot_idx = as.integer(stringr::str_remove_all(qty, '[^\\d]'))) %>%
                                 select(-qty)


                               N.Fminusalpha <- y.Fminusalpha.dat %>%
                                 summarize_at(.vars=boot.cols,
                                              sum) %>%
                                 tidyr::gather(starts_with('boot_weight'),
                                               key='qty',
                                               value='N.Fminusalpha') %>%
                                 mutate(boot_idx = as.integer(stringr::str_remove_all(qty, '[^\\d]'))) %>%
                                 select(-qty)

                               #####
                               ## aggregate visibility estimators

                               ## use the current bootstrap weights to get estimated reports
                               ## from respondents in cell alpha to sibs in not alpha,
                               ## and from respondents in not alpha to sibs in alpha
                               ## NB: these are aggregate-visibility estimates
                               #y.Falpha.Fminusalpha <- sum(y.Falpha.Fminusalpha.dat %>% pull(!!sym(cur.wgt)))
                               #y.Fminusalpha.Falpha <- sum(y.Fminusalpha.Falpha.dat %>% pull(!!sym(cur.wgt)))

                               y.Falpha.Fminusalpha <- y.Falpha.Fminusalpha.dat %>%
                                 summarize_at(.vars=boot.cols,
                                              sum) %>%
                                 tidyr::gather(starts_with('boot_weight'),
                                        key='qty',
                                        value='y.Falpha.Fminusalpha') %>%
                                 mutate(boot_idx = as.integer(stringr::str_remove_all(qty, '[^\\d]'))) %>%
                                 select(-qty)

                               y.Fminusalpha.Falpha <- y.Fminusalpha.Falpha.dat %>%
                                 summarize_at(.vars=boot.cols,
                                              sum) %>%
                                 tidyr::gather(starts_with('boot_weight'),
                                        key='qty',
                                        value='y.Fminusalpha.Falpha') %>%
                                 mutate(boot_idx = as.integer(stringr::str_remove_all(qty, '[^\\d]'))) %>%
                                 select(-qty)

                               res <- y.Falpha.Fminusalpha %>%
                                 left_join(y.Fminusalpha.Falpha, by='boot_idx') %>%
                                 left_join(N.Falpha, by='boot_idx') %>%
                                 left_join(N.Fminusalpha, by='boot_idx') %>%
                                 mutate(cell = cur_cell,
                                        n_Falpha = nrow(y.Falpha.Fminusalpha.dat),
                                        n_Fminusalpha = nrow(y.Fminusalpha.Falpha.dat))

                               return(res)
                               #return(tibble(y.Falpha.Fminusalpha = y.Falpha.Fminusalpha,
                               #              y.Fminusalpha.Falpha = y.Fminusalpha.Falpha,
                               #              cell=cur_cell,
                               #              n_Falpha = nrow(y.Falpha.Fminusalpha.dat),
                               #              n_Fminusalpha = nrow(y.Fminusalpha.Falpha.dat)))

                             })

  ic.agg <- boot.ests %>%
    group_by(cell) %>%
    mutate(diff = y.Falpha.Fminusalpha - y.Fminusalpha.Falpha,
           abs_diff = abs(diff),
           diff2 = diff^2,
           normalized_diff = diff/(N.Falpha*N.Fminusalpha),
           abs_normalized_diff = abs(normalized_diff),
           normalized_diff2 = normalized_diff^2) %>%
    summarise_at(c('diff', 'abs_diff', 'diff2',
                   'normalized_diff', 'abs_normalized_diff', 'normalized_diff2'),
                 list(mean=~mean(.),
                      se=~sd(.),
                      ci_low=~quantile(., probs=.025),
                      ci_high=~quantile(., probs=.975))) %>%
    decode_cells('cell', ego.cell.vars, remove=FALSE)

  boot.ests <- boot.ests %>%
    decode_cells('cell', ego.cell.vars, remove=FALSE)

  ## TODO - should rename ic.agg to ic.summ (to avoid confusion w/ aggregate vis estimator)
  res <- list(ic.summ = ic.agg,
              ic.boot.ests = boot.ests)

  return(res)
}



# this is a useful helper function, used in sib_ic_checks
grab_cell <- function(dat, cell.var, cell.value) {
  res <- dat %>% filter_at(vars(cell.var), all_vars(. == cell.value))
  return(res)
}

# this is a useful helper function, used in sib_ic_checks
grab_cell_complement <- function(dat, cell.var, cell.value) {
  res <- dat %>% filter_at(vars(cell.var), all_vars(. != cell.value))
  return(res)
}

# helper function to encode cells as a single variable
encode_cells <- function(df, new.name, cell.vars, sep="_X_") {
  res <- df %>%
    mutate(!!new.name := paste(!!!c(syms(cell.vars), sep=sep))) %>%
    select(-one_of(cell.vars))
  return(res)
}

# helper function do decode cells into multiple variables
decode_cells <- function(df, cell.name, cell.vars, remove=TRUE, sep="_X_") {
  res <- df %>%
    tidyr::separate(!!sym(cell.name),
                    into=cell.vars,
                    remove=remove,
                    sep=sep)
  return(res)
}


## OLD VERSION, going to be removed eventually...

##' get calculate internal consistency checks for sibling reports
##'
##' @param esc.dat The ego X sibling X cell dataset (see \code{get_esc_reports})
##' @param ego.dat The ego dataset, containing one row for each survey respondent
##' @param ego.id  String with the name of the column in \code{esc.dat} containing the survey respondent ID
##' @param sib.id  String with the name of the column in \code{esc.dat} containing the unique sibling ID
##' @param sib.frame.indicator String with the name of the column in \code{sib.dat} containing a 0/1 coded variable indicating whether or not each sib is in the frame population
##' @param sib.cell.vars see Details
##' @param ego.cell.vars see Details
##' @param boot.weights Dataframe with bootstrap resampled weights. See Details
##' @return A tibble with a row for each reported sibling in the frame population, the sibling's cell info, and the cell info of the survey respondent who reported each sibling
##' @examples
##'   # TODO write example code
##' @section Details:
##'   The \code{sib.cell.vars} and \code{ego.cell.vars} arguments should have a vector with Strings containing the names of
##'   columns in \code{esc.dat} identifying the cells to group reports by (typically age and sex, and possibly other variables).
##'   Note that, unlike the \code{cell.vars} argument in \code{get_ec_reports}, for this function \code{sib.cell.vars} and
##'   \code{ego.cell.vars} MUST have the age variable listed FIRST.
##'   The goal of \code{get_ic_reports} is to find the cell that each sibling was in at the time of the interview, meaning the oldest age group to which the
##'   sibling contributed any exposure.
##'   \code{boot.weights} is assumed to have a column that is named whatever the \code{ego.id} is,
##'   and then a series of columns named \code{boot_weight_1}, ..., \code{boot_weight_M}.
sib_ic_checks_OLD <- function(esc.dat,
                          ego.dat,
                          ego.id,
                          sib.id,
                          sib.frame.indicator,
                          sib.cell.vars,
                          ego.cell.vars,
                          boot.weights) {

  ec.yf.dat <- get_ic_reports(esc.dat,
                              ego.dat,
                              ego.id,
                              sib.id,
                              sib.frame.indicator,
                              sib.cell.vars,
                              ego.cell.vars)

  # encode the cells into one variable, making it easier to work with
  ec.yf.dat <- ec.yf.dat %>%
    ungroup() %>%
    encode_cells('.ego.cell', ego.cell.vars) %>%
    encode_cells('.sib.cell', sib.cell.vars)

  # check that the levels are the same
  if(! setequal(unique(ec.yf.dat$.ego.cell), unique(ec.yf.dat$.sib.cell))) {
    warning("The cell values do not appear to be the same for ego and for sibs.")
  }

  cell.vals <- unique(ec.yf.dat$.ego.cell)

  M <- ncol(boot.weights) - 1

  ec.yf.dat <- ec.yf.dat %>%
    #left_join(boot.weights %>% dplyr::mutate(.ego.id = !!sym(ego.id)), by='.ego.id')
    left_join(boot.weights, by=ego.id)

  # we'll map over bootstrap resamples and cells, calculating an
  # IC check for each bootstrap rep X cell
  boot.ests <-
    purrr::map_df(1:M,
                  function(boot.idx) {
                    cur.wgt <- paste0('boot_weight_', boot.idx)

                    cur.rep.res <-
                      purrr::map_df(cell.vals,
                                    function(cur_cell) {

                                      # egos in group alpha
                                      y.Falpha.dat <- ec.yf.dat %>%
                                        grab_cell('.ego.cell', cur_cell)
                                      # reports from egos in group alpha about sibs in -alpha
                                      y.Falpha.Fminusalpha.dat <- y.Falpha.dat %>%
                                        grab_cell_complement('.sib.cell', cur_cell)

                                      # egos not in group alpha
                                      y.Fminusalpha.dat <- ec.yf.dat %>%
                                        grab_cell_complement('.ego.cell', cur_cell)
                                      # reports from egos not in group alpha about sibs in alpha
                                      y.Fminusalpha.Falpha.dat <- y.Fminusalpha.dat %>%
                                        grab_cell('.sib.cell', cur_cell)

                                      #####
                                      ## aggregate visibility estimators

                                      ## use the current bootstrap weights to get estimated reports
                                      ## from respondents in cell alpha to sibs in not alpha,
                                      ## and from respondents in not alpha to sibs in alpha
                                      ## NB: these are aggregate-visibility estimates
                                      y.Falpha.Fminusalpha <- sum(y.Falpha.Fminusalpha.dat %>% pull(!!sym(cur.wgt)))
                                      y.Fminusalpha.Falpha <- sum(y.Fminusalpha.Falpha.dat %>% pull(!!sym(cur.wgt)))

                                      return(tibble(y.Falpha.Fminusalpha = y.Falpha.Fminusalpha,
                                                    y.Fminusalpha.Falpha = y.Fminusalpha.Falpha,
                                                    cell=cur_cell,
                                                    n_Falpha = nrow(y.Falpha.Fminusalpha.dat),
                                                    n_Fminusalpha = nrow(y.Fminusalpha.Falpha.dat)))

                                      ## below adds individual visibility checks,
                                      ## which I no longer think make sense
                                      ##

                                      #####
                                      ## individual visibility estimators

                                      ## individual visibility estimator for y(F_alpha, F_-alpha) is
                                      ##    \sum_{i \in s_\alpha} w_i * [y(i, F_-alpha) / (y(i, F_alpha) + 1)]
                                      ##
                                      ## start w/ reports made by ego in cell alpha
                                      #y.Falpha.ind2 <- y.Falpha.dat %>%
                                      #  mutate(samecell = as.numeric(.ego.cell == .sib.cell)) %>%
                                      #  group_by(caseid) %>%
                                      #  summarize(# number of sibs ego reports who are not in the same cell as ego (ie sibs are not in cell alpha)
                                      #            ind.y.Falpha.Fminusalpha = sum((1 - samecell)),
                                      #            # number of sibs ego reports who are in the same cell as ego (ie sibs are in cell alpha)
                                      #            ind.y.Falpha.Falpha = sum(samecell),
                                      #            .cur.wgt = mean(!!sym(cur.wgt))) %>%
                                      #  mutate(ind.y.Falpha.Fminusalpha = .cur.wgt * ind.y.Falpha.Fminusalpha / (ind.y.Falpha.Falpha + 1))
                                      #y.ind.Falpha.Fminusalpha <- sum(y.Falpha.ind2$ind.y.Falpha.Fminusalpha)

                                      ### individual visibility estimator for y(F_-alpha, F_alpha) is
                                      ###    \sum_{i \in s_-\alpha} w_i * [y(i, F_alpha) / (y(i, F_-alpha) + 1)]
                                      ###
                                      ### start w/ reports made by ego in cell -alpha
                                      #y.Fminusalpha.ind2 <- y.Fminusalpha.dat %>%
                                      #  mutate(samecell = as.numeric(.ego.cell == .sib.cell)) %>%
                                      #  group_by(caseid) %>%
                                      #  summarize(# number of sibs ego reports who are not in same cell as ego (ie sibs are in cell alpha)
                                      #            ind.y.Fminusalpha.Falpha = sum((1 - samecell)),
                                      #            # number of sibs ego reports who are in the same cell as ego (ie sibs are not in cell alpha)
                                      #            ind.y.Fminusalpha.Fminusalpha = sum(samecell),
                                      #            .cur.wgt = mean(!!sym(cur.wgt))) %>%
                                      #  mutate(ind.y.Fminusalpha.Falpha = .cur.wgt * ind.y.Fminusalpha.Falpha / (ind.y.Fminusalpha.Fminusalpha + 1))
                                      #y.ind.Fminusalpha.Falpha <- sum(y.Fminusalpha.ind2$ind.y.Fminusalpha.Falpha)

                                      #return(tibble(y.agg.Falpha.Fminusalpha = y.Falpha.Fminusalpha,
                                      #              y.agg.Fminusalpha.Falpha = y.Fminusalpha.Falpha,
                                      #              y.ind.Falpha.Fminusalpha = y.ind.Falpha.Fminusalpha,
                                      #              y.ind.Fminusalpha.Falpha = y.ind.Fminusalpha.Falpha,
                                      #              cell=cur_cell,
                                      #              n_Falpha = nrow(y.Falpha.Fminusalpha.dat),
                                      #              n_Fminusalpha = nrow(y.Fminusalpha.Falpha.dat)))

                                    })

                    cur.rep.res$boot.idx <- boot.idx

                    return(cur.rep.res)
                  })



  ic.agg <- boot.ests %>%
    group_by(cell) %>%
    mutate(diff = y.Falpha.Fminusalpha - y.Fminusalpha.Falpha,
           abs_diff = abs(diff),
           diff2 = diff^2) %>%
    summarise_at(c('diff', 'abs_diff', 'diff2'),
                 list(mean=~mean(.),
                      se=~sd(.),
                      ci_low=~quantile(., probs=.025),
                      ci_high=~quantile(., probs=.975))) %>%
    decode_cells('cell', ego.cell.vars, remove=FALSE)

  # version w/ separate individual and aggregate IC checks
  #ic.agg <- boot.ests %>%
  #  group_by(cell) %>%
  #  mutate(agg_diff = y.agg.Falpha.Fminusalpha - y.agg.Fminusalpha.Falpha,
  #         agg_abs_diff = abs(agg_diff),
  #         agg_diff2 = agg_diff^2,
  #         ind_diff = y.ind.Falpha.Fminusalpha - y.ind.Fminusalpha.Falpha,
  #         ind_abs_diff = abs(ind_diff),
  #         ind_diff2 = ind_diff^2) %>%
  #  summarise_at(c('agg_diff', 'agg_abs_diff', 'agg_diff2',
  #                 'ind_diff', 'ind_abs_diff', 'ind_diff2'),
  #               list(mean=~mean(.),
  #               se=~sd(.),
  #               ci_low=~quantile(., probs=.025),
  #               ci_high=~quantile(., probs=.975))) %>%
  #  decode_cells('cell', ego.cell.vars, remove=FALSE)

  boot.ests <- boot.ests %>%
    decode_cells('cell', ego.cell.vars, remove=FALSE)

  ## TODO - should rename ic.agg to ic.summ (to avoid confusion w/ aggregate vis estimator)
  res <- list(ic.agg = ic.agg,
              ic.boot.ests = boot.ests)

  return(res)
}


