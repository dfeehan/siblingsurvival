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

##' Run every survey that has both a published target and microdata, and write
##' the result to mics-results.csv.
##'
##' This exists so that MICS has what the DHS side already had: a committed file
##' of numbers to diff against. `validate_mics()` returns its comparison in
##' memory, which is fine interactively but leaves a refactor with no way to
##' *demonstrate* that the MICS estimates did not move -- only to eyeball them.
##' The all-cause DHS check writes `allcause-results.csv` for exactly this
##' reason, and that file is what made the estimator-spine move verifiable.
##'
##' Three things are recorded deliberately:
##'
##'   * **Failures are rows, not skips.** A survey that starts failing, or stops
##'     failing, is signal. MDG2018 and ZW2014 currently fail for reasons that
##'     have nothing to do with the estimator -- a missing respondent CMC date of
##'     birth and a MICS4/5 varmap respectively -- and if either is fixed, the
##'     diff should say so rather than quietly gaining rows.
##'   * **The V1 shape checks**, even though they have no published counterpart.
##'     They have no `published` value to compare against, but a change in row
##'     count or living/dead composition means the prep moved, which is worth
##'     catching even when the rates happen not to shift.
##'   * **A fixed row order**, so the diff shows real changes rather than
##'     reordering.
##'
##' @param write write the CSV as well as returning the data frame
##' @return a data frame, one row per (survey, quantity, age_group)
validate_mics_all <- function(write = TRUE) {

  targets <- published_targets()
  have    <- list.dirs(mics_data_dir(), full.names = FALSE, recursive = FALSE)
  surveys <- sort(intersect(unique(targets$survey), have))

  if (length(surveys) == 0) {
    stop("no survey has both a published target and a directory under ",
         mics_data_dir(), ". See README.md for how to obtain the microdata.")
  }

  rows <- list()

  for (id in surveys) {

    r <- try(suppressWarnings(suppressMessages(validate_mics(id))), silent = TRUE)

    if (inherits(r, "try-error")) {
      rows[[length(rows) + 1]] <- data.frame(
        survey = id, status = "error", quantity = NA_character_,
        age_group = NA_character_, observed = NA_real_, published = NA_real_,
        ratio = NA_real_,
        note = gsub("[\r\n]+", " ", conditionMessage(attr(r, "condition"))),
        stringsAsFactors = FALSE)
      next
    }

    rows[[length(rows) + 1]] <- data.frame(
      survey = id, status = "ok", quantity = r$v1$quantity,
      age_group = NA_character_, observed = r$v1$observed,
      published = NA_real_, ratio = NA_real_, note = "",
      stringsAsFactors = FALSE)

    if (!is.null(r$comparison) && nrow(r$comparison) > 0) {
      rows[[length(rows) + 1]] <- data.frame(
        survey = id, status = "ok", quantity = r$comparison$quantity,
        age_group = r$comparison$age_group, observed = r$comparison$observed,
        published = r$comparison$published, ratio = r$comparison$ratio,
        note = "", stringsAsFactors = FALSE)
    }
  }

  res <- do.call(rbind, rows)
  res <- res[order(res$survey, res$quantity, res$age_group), ]

  ok  <- unique(res$survey[res$status == "ok"])
  bad <- unique(res$survey[res$status == "error"])
  cat("\nMICS validation\n===============\n")
  cat("ran:    ", if (length(ok))  paste(ok,  collapse = ", ") else "(none)", "\n")
  cat("failed: ", if (length(bad)) paste(bad, collapse = ", ") else "(none)", "\n\n")

  cmp <- res[res$status == "ok" & !is.na(res$ratio), ]
  if (nrow(cmp) > 0) {
    print(aggregate(ratio ~ survey + quantity, data = cmp,
                    FUN = function(x) c(min = min(x), max = max(x))),
          row.names = FALSE)
  }

  if (write) {
    f <- here::here("data-raw", "mics-validation", "mics-results.csv")
    write.csv(res, f, row.names = FALSE)
    cat("\nwrote data-raw/mics-validation/mics-results.csv\n")
  }

  invisible(res)
}
