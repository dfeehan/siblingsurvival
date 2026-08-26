## Run every DHS survey in the analysis sample through the package AND through
## the Stata reference replica, and compare.
##
## The paper covers every DHS survey with a maternal mortality module, so
## "reproduces the reference on all of them" is a deliverable, not a formality.
##
##   source("data-raw/dhs-validation/breadth-check.R")
##
## Writes data-raw/dhs-validation/breadth-results.csv.

suppressMessages({ library(dplyr); library(haven) })
devtools::load_all(here::here())
source(here::here("data-raw", "dhs-validation", "stata-reference-replica.R"))

REPRO <- c("[15,20)","[20,25)","[25,30)","[30,35)","[35,40)","[40,45)","[45,50)")

VARMAPS <- list("2" = sibhist_varmap_dhs2, "3" = sibhist_varmap_dhs3,
                "4" = sibhist_varmap_dhs4, "5" = sibhist_varmap_dhs5,
                "6" = sibhist_varmap_dhs6, "7" = sibhist_varmap_dhs7,
                "8" = sibhist_varmap_dhs8)

## Gabon 2000 has a byte sequence haven cannot convert under the default
## encoding; latin1 reads it. Try the default first so this stays a
## survey-specific fallback rather than a blanket setting.
read_ir <- function(path, ...) {
  tryCatch(read_dta(path, ...),
           error = function(e) read_dta(path, encoding = "latin1", ...))
}

one_survey <- function(path, survey_id, phase) {

  enc <- tryCatch({ read_dta(path, n_max = 0); NULL },
                  error = function(e) "latin1")

  ## ---- reference ---------------------------------------------------------
  ref <- dhs_am(path, lw = -6, uw = 0, encoding = enc)$totals
  rf  <- ref[ref$sex == 2, ]
  rm_ <- ref[ref$sex == 1, ]

  ## ---- package -----------------------------------------------------------
  raw  <- read_ir(path)
  prep <- prep_dhs_sib_histories(raw, varmap = VARMAPS[[phase]],
                                 add_maternal = TRUE, na.action = "include",
                                 verbose = FALSE)
  sib <- prep$sib.dat
  sib$in.F <- ifelse(is.na((sib$sib.alive == 1) & (sib$sib.sex == "f") &
                             (sib$sib.age >= 15) & (sib$sib.age <= 49)), 0,
                     as.numeric((sib$sib.alive == 1) & (sib$sib.sex == "f") &
                                  (sib$sib.age >= 15) & (sib$sib.age <= 49)))

  est <- function(event) {
    cc <- cell_config(age.groups = "5yr", time.periods = "7yr_beforeinterview",
                      start.obs = "sib.dob", end.obs = "sib.endobs",
                      event = event, age.offset = "sib.dob",
                      time.offset = "doi", exp.scale = 1/12)
    sibling_estimator(sib.dat = sib, ego.id = "caseid",
                      sib.frame.indicator = "in.F", sib.sex = "sib.sex",
                      cell.config = cc, weights = "wwgt")$asdr.agg %>%
      filter(sib.age %in% REPRO)
  }
  ac <- est("sib.death.date")
  pr <- est("sib.preg_related.death.date")

  ## age standardisation, women only (men would need the MR file -- see H4)
  prop <- get_ego_age_distn(prep$ego.dat, only_females = TRUE) %>%
    filter(age.cat %in% REPRO) %>% arrange(match(age.cat, REPRO))
  pw <- prop$agegrp_prop / sum(prop$agegrp_prop)
  prf <- pr %>% filter(sib.sex == "f") %>% arrange(match(sib.age, REPRO))

  data.frame(
    survey = survey_id, phase = phase,
    n_sib_raw = prep$summ$n.sib.raw, n_sib = prep$summ$n.sib,
    miss_alive = prep$summ$miss.alive, miss_sex = prep$summ$miss.sex,
    miss_dob = prep$summ$miss.dob, miss_wgt = prep$summ$miss.wgt,
    pkg_fexp = sum(ac$denom.hat[ac$sib.sex == "f"]), ref_fexp = rf$yexp,
    pkg_mexp = sum(ac$denom.hat[ac$sib.sex == "m"]), ref_mexp = rm_$yexp,
    pkg_fdea = sum(ac$num.hat[ac$sib.sex == "f"]),   ref_fdea = rf$deaths,
    pkg_mdea = sum(ac$num.hat[ac$sib.sex == "m"]),   ref_mdea = rm_$deaths,
    pkg_prd  = sum(prf$num.hat),                     ref_prd  = rf$prdeaths,
    pr_rate_adj = 1000 * sum(prf$asdr.hat * pw),
    n_na_cells = sum(is.na(ac$asdr.hat)) + sum(is.na(prf$asdr.hat)),
    encoding = if (is.null(enc)) "default" else enc,
    stringsAsFactors = FALSE)
}

idx <- as.data.frame(readRDS("~/Dropbox/maternal-mortality/maternal-mortality/out/survey-index.rds"))
idx$phase <- substr(idx$dhs_phase, 1, 1)

out <- list()
for (i in seq_len(nrow(idx))) {
  id <- idx$survey_id[i]
  cat(sprintf("[%2d/%d] %s ... ", i, nrow(idx), id)); flush.console()
  r <- tryCatch(one_survey(idx$path[i], id, idx$phase[i]),
                error = function(e) {
                  cat("FAILED:", conditionMessage(e), "\n")
                  data.frame(survey = id, phase = idx$phase[i],
                             error = conditionMessage(e), stringsAsFactors = FALSE)
                })
  if (is.null(r$error)) {
    cat(sprintf("exp %.4f  dth %.4f  pr %.4f\n",
                r$pkg_fexp / r$ref_fexp, r$pkg_fdea / r$ref_fdea,
                r$pkg_prd / r$ref_prd))
  }
  out[[id]] <- r
  rm(r); invisible(gc())
}

res <- bind_rows(out)
write.csv(res, here::here("data-raw", "dhs-validation", "breadth-results.csv"),
          row.names = FALSE)
cat("\nwrote data-raw/dhs-validation/breadth-results.csv\n")
