# Tests for the cleanup items in the package handoff plan:
#   C1 - get_ego_age_distn() is exported
#   C3 - reproductive_age_groups() is the single definition of the 15-49 groups
#   D1 - missing *sibling* variables are reported, not just missing ego variables
#   D4 - sib.dob is derived from age at death, guarded on the variable it uses
#   A2 - sibling_estimator() defaults sib.id to 'sibid' and errors legibly

library(dplyr)

# =====================================================================
# C3: reproductive_age_groups()
# =====================================================================

test_that("reproductive_age_groups: returns the seven 5-year groups from 15 to 49", {
  expect_equal(reproductive_age_groups(),
               c("[15,20)", "[20,25)", "[25,30)", "[30,35)",
                 "[35,40)", "[40,45)", "[45,50)"))
})

test_that("reproductive_age_groups: is exported", {
  expect_true("reproductive_age_groups" %in% getNamespaceExports("siblingsurvival"))
})

test_that("reproductive_age_groups: matches the labels make.even.age.groups produces", {
  ag <- make.even.age.groups(5, min.age = 15, max.age = 50)
  expect_true(all(reproductive_age_groups() %in% ag$name))
})

test_that("reproductive_age_groups: agrees with the old exclusion filter for standard 5yr cells", {
  # the previous code excluded the three post-reproductive groups rather than
  # including the seven reproductive ones; for the standard '5yr' age groups
  # (15 to 65) the two are equivalent, and this test pins that down
  ag <- make.even.age.groups(5, min.age = 15, max.age = 65)$name
  expect_equal(sort(setdiff(ag, c("[50,55)", "[55,60)", "[60,65)"))),
               sort(intersect(ag, reproductive_age_groups())))
})

# =====================================================================
# C1: get_ego_age_distn() is exported
# =====================================================================

test_that("get_ego_age_distn: is exported and callable without :::", {
  expect_true("get_ego_age_distn" %in% getNamespaceExports("siblingsurvival"))
  res <- get_ego_age_distn(ex.ego)
  expect_setequal(as.character(res$age.cat), reproductive_age_groups())
})

# =====================================================================
# D1: missing sibling variables are reported
# =====================================================================

test_that("prep_dhs_sib_histories: reports sibling variables missing from the data", {
  # mm3 is the sibling's current age; drop every wide column for it
  d <- model_dhs_dat %>% select(-starts_with("mm3_"))

  expect_output(
    try(prep_dhs_sib_histories(d, varmap = sibhist_varmap_dhs6, verbose = TRUE),
        silent = TRUE),
    "Sibling column\\(s\\) found in the varmap are missing"
  )
})

test_that("prep_dhs_sib_histories: summ records missing ego and sibling columns separately", {
  res <- prep_dhs_sib_histories(model_dhs_dat,
                                varmap = sibhist_varmap_dhs6,
                                verbose = FALSE)

  expect_true("ego.cols.notfound" %in% names(res$summ))
  expect_true("sib.cols.notfound" %in% names(res$summ))

  # the shipped model data has no mm16 (died of violence/accident), which is
  # why add_maternal_deaths() can only identify pregnancy-related deaths on it
  expect_true("mm16" %in% unlist(res$summ$sib.cols.notfound))
})

test_that("prep_dhs_sib_histories: a complete varmap reports nothing missing", {
  # dhs2 has no mm16, and model_dhs_dat has every other sibling variable
  res <- prep_dhs_sib_histories(model_dhs_dat,
                                varmap = sibhist_varmap_dhs2,
                                verbose = FALSE)
  expect_length(unlist(res$summ$sib.cols.notfound), 0)
})

test_that("prep_dhs_sib_histories: sibling variables are matched as prefixes, not exact names", {
  # 'mm3' is never a column name; the data has mm3_01, mm3_02, ...
  # so an exact-name check would report every sibling variable as missing
  res <- prep_dhs_sib_histories(model_dhs_dat,
                                varmap = sibhist_varmap_dhs6,
                                verbose = FALSE)
  expect_false("mm3" %in% unlist(res$summ$sib.cols.notfound))
})

# =====================================================================
# D4: sib.dob derived from age at death
# =====================================================================

test_that("prep_dhs_sib_histories: derives sib.dob from age at death when years-ago is missing", {
  # blank out sibling date of birth (mm4) and years-since-death (mm6), leaving
  # age at death (mm7) and date of death (mm8). The birth date derivation has
  # everything it needs, and used to return NA because it was guarded on mm6.
  d <- model_dhs_dat %>%
    mutate(across(starts_with("mm4_"), ~ NA_real_)) %>%
    mutate(across(starts_with("mm6_"), ~ NA_real_))

  res <- prep_dhs_sib_histories(d, varmap = sibhist_varmap_dhs6, verbose = FALSE)

  derivable <- res$sib.dat %>%
    filter(!is.na(sib.death.age), !is.na(sib.death.date), sib.death.date != -1)

  expect_gt(nrow(derivable), 0)
  expect_true(all(!is.na(derivable$sib.dob)))

  # and the derived date is death date minus age at death, in months
  expect_equal(derivable$sib.dob,
               as.integer(derivable$sib.death.date - 12 * derivable$sib.death.age))
})

# =====================================================================
# A2: sibling_estimator() sib.id default and error message
# =====================================================================

test_that("sibling_estimator: sib.id defaults to the column prep creates", {
  expect_equal(formals(sibling_estimator)$sib.id, "sibid")
  expect_true("sibid" %in% names(ex.sib))
})

test_that("sibling_estimator: runs without sib.id being supplied", {
  sib <- ex.sib %>%
    mutate(in.F = as.numeric(
      (sib.alive == 1) & (sib.age >= 15) & (sib.age <= 49) & (sib.sex == 'f'))) %>%
    filter(!is.na(in.F), sib.sex == 'f')

  cc <- cell_config(age.groups = '5yr', time.periods = '5yr_beforeinterview',
                    start.obs = 'sib.dob', end.obs = 'sib.endobs',
                    event = 'sib.death.date', age.offset = 'sib.dob',
                    time.offset = 'doi', exp.scale = 1/12)

  res <- sibling_estimator(sib.dat = sib, ego.id = 'caseid',
                           sib.frame.indicator = 'in.F', sib.sex = 'sib.sex',
                           cell.config = cc, weights = 'wwgt')

  expect_true(all(c('asdr.ind', 'asdr.agg') %in% names(res)))
  expect_gt(nrow(res$asdr.ind), 0)
})

test_that("sibling_estimator: a wrong column name errors with the columns that do exist", {
  cc <- cell_config(age.groups = '5yr', time.periods = '5yr_beforeinterview',
                    start.obs = 'sib.dob', end.obs = 'sib.endobs',
                    event = 'sib.death.date', age.offset = 'sib.dob',
                    time.offset = 'doi', exp.scale = 1/12)

  err <- expect_error(
    sibling_estimator(sib.dat = ex.sib, ego.id = 'caseid', sib.id = 'sib.id',
                      sib.frame.indicator = 'in.F', sib.sex = 'sib.sex',
                      cell.config = cc, weights = 'wwgt'))

  # names the argument and the value it was given ...
  expect_match(conditionMessage(err), "sib.id='sib.id'")
  # ... and lists what is actually there, so 'sibid' is discoverable
  expect_match(conditionMessage(err), "sibid")
})

# =====================================================================
# a weightless respondent must not NA out the whole age distribution
# =====================================================================
# Sao Tome and Principe 2014 has exactly one respondent with no sampling
# weight. Left in, sum(wwgt) is NA, which propagates through the denominator
# and makes *every* agegrp_prop NA -- silently NA-ing out every age-adjusted
# rate computed from it, with no error anywhere.

test_that("get_ego_age_distn drops respondents with no weight", {
  ego <- data.frame(
    sex     = "f",
    age.cat = c("[15,20)", "[20,25)", "[25,30)", "[30,35)",
                "[35,40)", "[40,45)", "[45,50)", "[40,45)"),
    wwgt    = c(1, 1, 1, 1, 1, 1, 1, NA))

  expect_warning(d <- get_ego_age_distn(ego, only_females = TRUE),
                 "no sampling weight")

  expect_false(any(is.na(d$agegrp_prop)))
  expect_equal(sum(d$agegrp_prop), 1)
  expect_equal(nrow(d), 7)
})

test_that("get_ego_age_distn is silent and unchanged when weights are complete", {
  ego <- data.frame(
    sex     = "f",
    age.cat = c("[15,20)", "[20,25)", "[25,30)", "[30,35)",
                "[35,40)", "[40,45)", "[45,50)"),
    wwgt    = rep(1, 7))

  expect_silent(d <- get_ego_age_distn(ego, only_females = TRUE))
  expect_equal(d$agegrp_prop, rep(1/7, 7))
})
