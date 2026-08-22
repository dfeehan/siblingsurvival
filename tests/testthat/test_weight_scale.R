# Tests for:
#   B2 - weight.scale, so that non-DHS weights are not divided by 1e6
#   the required-column guards in get_ego_df() and get_sib_df()
#   C2 - aggregate_maternal_estimates() accepting precomputed inputs

library(dplyr)

# A minimal non-DHS survey: 3 respondents, up to 2 siblings each, wide layout,
# weights already normalized to average 1.
make_fake_nrsim <- function() {
  tibble::tibble(
    id      = 1:3,
    svy     = "sim1",
    wt      = c(1.0, 1.2, 0.8),
    cl      = 1:3,
    intdate = 1400,
    respage = c(25, 30, 35),
    # sibling attributes, wide
    a_1  = c(30, 40, 50),      a_2  = c(35, NA, 45),
    al_1 = c(1, 1, 1),         al_2 = c(1, NA, 1),
    sx_1 = c(2, 2, 2),         sx_2 = c(2, NA, 2),
    dd_1 = c(NA_real_, NA, NA), dd_2 = c(NA_real_, NA, NA),
    db_1 = c(1000, 1000, 1000), db_2 = c(1010, NA, 1010),
    ya_1 = c(NA_real_, NA, NA), ya_2 = c(NA_real_, NA, NA),
    da_1 = c(NA_real_, NA, NA), da_2 = c(NA_real_, NA, NA)
  )
}

make_fake_varmap <- function() {
  tibble::tibble(
    orig.varname = c("id", "svy", "wt", "cl", "intdate", "respage",
                     "a", "al", "sx", "dd", "db", "ya", "da"),
    new.varname  = c("caseid", "survey", "wwgt", "psu", "doi", "age",
                     "sib.age", "sib.alive", "sib.sex", "sib.death.date",
                     "sib.dob", "sib.death.yrsago", "sib.death.age"),
    sibvar       = c(0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1)
  )
}

# =====================================================================
# B2: weight scaling
# =====================================================================

test_that("prep_nrsim_sib_histories: does not divide weights by 1e6 by default", {
  d <- make_fake_nrsim()
  res <- prep_nrsim_sib_histories(d, varmap = make_fake_varmap(), verbose = FALSE)

  # this is the whole point: non-DHS weights are already normalized
  expect_equal(res$ego.dat$wwgt, d$wt)
})

test_that("prep_nrsim_sib_histories: weight.scale=1e6 reproduces the DHS behaviour", {
  d <- make_fake_nrsim()
  res <- prep_nrsim_sib_histories(d, varmap = make_fake_varmap(),
                                  weight.scale = 1e6, verbose = FALSE)

  expect_equal(res$ego.dat$wwgt, d$wt / 1e6)
})

test_that("prep_nrsim_sib_histories: the scaled weight is what lands on the sibling rows", {
  d <- make_fake_nrsim()
  res <- prep_nrsim_sib_histories(d, varmap = make_fake_varmap(), verbose = FALSE)

  expect_setequal(unique(res$sib.dat$wwgt), unique(d$wt))
  expect_true(all(res$sib.dat$wwgt > 1e-3))
})

test_that("prep_dhs_sib_histories: still divides DHS weights by 1e6 by default", {
  res <- prep_dhs_sib_histories(model_dhs_dat, varmap = sibhist_varmap_dhs6,
                                verbose = FALSE)
  raw <- model_dhs_dat$v005

  expect_equal(res$ego.dat$wwgt, raw / 1e6)
  # DHS weights average 1 after scaling
  expect_equal(mean(res$ego.dat$wwgt), 1, tolerance = 0.05)
})

test_that("prep_dhs_sib_histories: weight.scale=1 leaves the weights alone", {
  res <- prep_dhs_sib_histories(model_dhs_dat, varmap = sibhist_varmap_dhs6,
                                weight.scale = 1, verbose = FALSE)

  expect_equal(res$ego.dat$wwgt, model_dhs_dat$v005)
})

# =====================================================================
# Required-column guards
# =====================================================================

test_that("get_ego_df: errors naming a missing required ego column", {
  d  <- make_fake_nrsim()
  vm <- make_fake_varmap() %>% filter(new.varname != "age")

  err <- expect_error(
    prep_nrsim_sib_histories(d, varmap = vm, verbose = FALSE))

  expect_match(conditionMessage(err), "missing required column")
  expect_match(conditionMessage(err), "age")
})

test_that("get_ego_df: errors when the survey column is absent", {
  d  <- make_fake_nrsim()
  vm <- make_fake_varmap() %>% filter(new.varname != "survey")

  expect_error(prep_nrsim_sib_histories(d, varmap = vm, verbose = FALSE),
               "survey")
})

test_that("get_sib_df: errors naming a missing required sibling column", {
  d  <- make_fake_nrsim()
  vm <- make_fake_varmap() %>% filter(new.varname != "sib.death.yrsago")

  err <- expect_error(
    prep_nrsim_sib_histories(d, varmap = vm, verbose = FALSE))

  # names the column, and says where it has to come from
  expect_match(conditionMessage(err), "sib.death.yrsago")
  expect_match(conditionMessage(err), "varmap")
})

test_that("get_sib_df: the doi error mentions that it has to be a CMC", {
  d  <- make_fake_nrsim()
  vm <- make_fake_varmap() %>% filter(new.varname != "doi")

  err <- expect_error(
    prep_nrsim_sib_histories(d, varmap = vm, verbose = FALSE))

  expect_match(conditionMessage(err), "doi")
})

# =====================================================================
# C2: precomputed inputs
# =====================================================================

test_that("aggregate_maternal_estimates: precomputed inputs give identical results", {
  ego <- ex.ego %>% mutate(sex = 'f')
  sib <- ex.sib %>%
    add_maternal_deaths() %>%
    mutate(in.F = as.numeric(
      (sib.alive == 1) & (sib.age >= 15) & (sib.age <= 49) & (sib.sex == 'f'))) %>%
    filter(!is.na(in.F), sib.sex == 'f')

  cc <- cell_config(age.groups = '5yr', time.periods = '7yr_beforeinterview',
                    start.obs = 'sib.dob', end.obs = 'sib.endobs',
                    event = 'sib.preg_related.death.date', age.offset = 'sib.dob',
                    time.offset = 'doi', event.name = 'pr_death', exp.scale = 1/12)

  est <- sibling_estimator(sib.dat = sib, ego.id = 'caseid',
                           sib.frame.indicator = 'in.F', sib.sex = 'sib.sex',
                           cell.config = cc, weights = 'wwgt')

  internal <- aggregate_maternal_estimates(est, ego, sib)

  ap <- get_ego_age_distn(ego, only_females = TRUE)
  vr <- get_visibility(ego, ego.id = 'caseid', sib,
                       sib.frame.indicator = 'in.F')
  supplied <- aggregate_maternal_estimates(est, ego, sib,
                                           age_prop = ap, vis_res = vr)

  expect_equal(internal, supplied)
})
