## Validate siblingsurvival against published MICS estimates.
##
## The target is the AGGREGATE visibility estimator: asdr.agg is
## sum(w*deaths)/sum(w*exposure) with no visibility weighting, which is exactly
## the conventional MICS calculation (MICS applies no sibship-size correction).
## asdr.ind is the contribution and is expected to differ.
##
## Requires MICS microdata, which is registration-gated and NOT in this repo.
## Place extracted files as data-raw/mics-data/<survey_id>/mm.sav and run:
##
##   source("data-raw/mics-validation/validate.R")
##   v <- validate_mics("ZW2019")
##
## See ../../dev/MICS-PLAN.md for the staged design (V1-V7) and the findings so far.

suppressMessages({
  library(dplyr); library(haven); library(tibble)
})
devtools::load_all(here::here())

mics_data_dir <- function() here::here("data-raw", "mics-data")

published_targets <- function() {
  read.csv(here::here("data-raw", "mics-validation", "published-targets.csv"),
           stringsAsFactors = FALSE)
}

REPRO_AGES <- c("[15,20)","[20,25)","[25,30)","[30,35)","[35,40)","[40,45)","[45,50)")

##' Build the maternal / pregnancy-related death-date columns for MICS6.
##'
##' Now just a thin wrapper on the package function. `preg.window = "42days"` is
##' the definition UNICEF's own tabulation syntax uses, and therefore the one
##' that published tables report; see spss-syntax-replica.R in this directory.
mics6_death_dates <- function(sib, na.action = "include",
                              preg.window = "42days") {
  add_maternal_deaths(sib, style = "mics6", na.action = na.action,
                      preg.window = preg.window, verbose = FALSE)
}

##' Run the staged validation for one survey.
validate_mics <- function(survey_id,
                          varmap = sibhist_varmap_mics6,
                          time.periods = "7yr_beforeinterview") {

  f <- file.path(mics_data_dir(), survey_id, "mm.sav")
  if (!file.exists(f)) stop("no mm.sav for ", survey_id, " at ", f)

  targets <- published_targets() %>% filter(survey == survey_id)

  raw <- read_sav(f)
  prep <- prep_mics_sib_histories(raw, survey = survey_id,
                                  varmap = varmap, verbose = FALSE)

  sib <- prep$sib.dat %>%
    mics6_death_dates() %>%
    mutate(in.F = as.numeric((sib.alive == 1) & (sib.sex == "f") &
                               (sib.age >= 15) & (sib.age <= 49)),
           in.F = ifelse(is.na(in.F), 0, in.F))

  ## ---- V1: shape and data quality -------------------------------------
  w <- as.numeric(raw$wmweight)
  alive <- as.numeric(raw$MM16)
  v1 <- tibble(
    quantity = c("n_rows", "sib_pct_living", "sib_pct_dead", "sib_pct_missing"),
    observed = c(nrow(raw),
                 100 * sum(w[alive == 1], na.rm = TRUE) / sum(w, na.rm = TRUE),
                 100 * sum(w[alive == 2], na.rm = TRUE) / sum(w, na.rm = TRUE),
                 100 * sum(w[alive %in% c(8, 9) | is.na(alive)], na.rm = TRUE) /
                   sum(w, na.rm = TRUE)))

  ## ---- V2-V4: exposure, deaths, rates ---------------------------------
  est_for <- function(event) {
    cc <- cell_config(age.groups = "5yr", time.periods = time.periods,
                      start.obs = "sib.dob", end.obs = "sib.endobs",
                      event = event, age.offset = "sib.dob",
                      time.offset = "doi", exp.scale = 1/12)
    sibling_estimator(sib.dat = sib, ego.id = "caseid",
                      sib.frame.indicator = "in.F", sib.sex = "sib.sex",
                      cell.config = cc, weights = "wwgt")
  }

  ## published MICS tables print a 42-day pregnancy-related count, whatever
  ## their column heading says -- see spss-syntax-replica.R
  mat <- est_for("sib.preg_related.death.date")$asdr.agg %>%
    filter(sib.sex == "f", sib.age %in% REPRO_AGES) %>%
    arrange(match(sib.age, REPRO_AGES))

  cells <- tibble(age_group = mat$sib.age,
                  exposure  = mat$denom.hat,
                  deaths    = mat$num.hat,
                  rate_per_1000 = 1000 * mat$asdr.hat)

  compare <- function(quantity) {
    obs <- switch(quantity,
                  exposure        = cells$exposure,
                  maternal_deaths = cells$deaths,
                  maternal_rate   = cells$rate_per_1000)
    pub <- targets %>% filter(quantity == !!quantity, age_group %in% REPRO_AGES) %>%
      arrange(match(age_group, REPRO_AGES))
    if (nrow(pub) == 0) return(NULL)
    tibble(quantity = quantity, age_group = pub$age_group,
           observed = obs[match(pub$age_group, cells$age_group)],
           published = pub$value) %>%
      mutate(ratio = observed / published)
  }

  list(survey = survey_id,
       v1 = v1,
       cells = cells,
       comparison = bind_rows(lapply(c("exposure", "maternal_deaths", "maternal_rate"),
                                     compare)),
       prep = prep)
}
