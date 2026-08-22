library(testthat)
library(dplyr)
library(siblingsurvival)

# =====================================================================
# Helper: build a minimal synthetic sibling data frame for
# testing add_maternal_deaths() without any external data dependency
# =====================================================================

make_sib <- function(sib.sex              = 'f',
                     sib.death.date       = 1200L,
                     sib.died.pregnant    = 3,
                     sib.time.delivery.death = NA_real_,
                     sib.died.accident    = NULL) {
  df <- tibble(
    sib.sex                 = sib.sex,
    sib.death.date          = sib.death.date,
    sib.died.pregnant       = sib.died.pregnant,
    sib.time.delivery.death = sib.time.delivery.death
  )
  if (!is.null(sib.died.accident)) {
    df$sib.died.accident <- sib.died.accident
  }
  df
}


# =====================================================================
# Tests: add_maternal_deaths()
# =====================================================================

test_that("add_maternal_deaths: adds both new columns", {
  result <- add_maternal_deaths(make_sib())
  expect_true("sib.preg_related.death.date" %in% names(result))
  expect_true("sib.maternal.death.date"     %in% names(result))
})

test_that("add_maternal_deaths: males get NA for both new date columns", {
  result <- add_maternal_deaths(make_sib(sib.sex = 'm'))
  expect_true(all(is.na(result$sib.preg_related.death.date)))
  expect_true(all(is.na(result$sib.maternal.death.date)))
})

test_that("add_maternal_deaths: qualifying preg-related death keeps its date", {
  # sib.died.pregnant == 3 (during/after delivery), time.delivery.death NA -> qualifies
  result <- add_maternal_deaths(make_sib(sib.died.pregnant = 3,
                                         sib.time.delivery.death = NA))
  expect_equal(result$sib.preg_related.death.date, 1200L)
})

test_that("add_maternal_deaths: non-qualifying death (not pregnant) gets -1", {
  result <- add_maternal_deaths(make_sib(sib.died.pregnant = 1))
  expect_equal(result$sib.preg_related.death.date, -1)
})

test_that("add_maternal_deaths: sib.maternal.death.date is NA when sib.died.accident absent", {
  df <- make_sib()   # no sib.died.accident column
  result <- add_maternal_deaths(df)
  expect_true(all(is.na(result$sib.maternal.death.date)))
})

test_that("add_maternal_deaths: accidental preg death is preg-related but NOT maternal", {
  # died while pregnant (code 2) but death was accidental -> preg-related yes, maternal no
  df <- make_sib(sib.died.pregnant = 2, sib.died.accident = 1)
  result <- add_maternal_deaths(df)
  expect_equal(result$sib.preg_related.death.date, 1200L)  # preg-related: yes
  expect_equal(result$sib.maternal.death.date,     -1)      # maternal: no
})

test_that("add_maternal_deaths: non-accidental preg death qualifies as both preg-related and maternal", {
  df <- make_sib(sib.died.pregnant = 2, sib.died.accident = 0)
  result <- add_maternal_deaths(df)
  expect_equal(result$sib.preg_related.death.date, 1200L)
  expect_equal(result$sib.maternal.death.date,     1200L)
})

# ---------------------------------------------------------------------
# mm9 governs the DHS pregnancy-related column, and mm12 is not consulted
# ---------------------------------------------------------------------
# The DHS reference implementation (DHS-Indicators-Stata,
# Chap16_AM/AM_rates.do:725) counts `mm9 >= 2 & mm9 <= 6` and nothing else; its
# header states that "mm12 is not needed" and drops it before the reshape.
#
# This block used to pin the opposite: mm9 in 2..5 AND mm12 in 100..141. Both
# halves were wrong, and together they cost 39.5 of 90.7 pregnancy-related
# deaths in Rwanda 2010 against a published 91.

test_that("all of mm9 2 through 6 are pregnancy-related", {
  for (code in 2:6) {
    result <- add_maternal_deaths(make_sib(sib.died.pregnant = code))
    expect_equal(result$sib.preg_related.death.date, 1200L,
                 info = paste("mm9 =", code))
  }
})

test_that("mm9 = 6 is pregnancy-related but NOT maternal", {
  # code 6 is "between six weeks and two months of a delivery", so it is inside
  # the two-month window and outside the 42-day one. This is exactly the line
  # the DHS reference draws between prdied (mm9 2..6) and mdied (mm9 2..5).
  result <- add_maternal_deaths(make_sib(sib.died.pregnant = 6,
                                         sib.died.accident = 0))
  expect_equal(result$sib.preg_related.death.date, 1200L)
  expect_equal(result$sib.maternal.death.date,     -1)
})

test_that("mm9 codes 0, 1 and 98 are not pregnancy-related", {
  for (code in c(0, 1, 98)) {
    result <- add_maternal_deaths(make_sib(sib.died.pregnant = code))
    expect_equal(result$sib.preg_related.death.date, -1,
                 info = paste("mm9 =", code))
  }
})

test_that("a missing mm9 is not a pregnancy-related death", {
  result <- add_maternal_deaths(make_sib(sib.died.pregnant = NA_real_))
  expect_equal(result$sib.preg_related.death.date, -1)
})

test_that("mm12 does not affect the pregnancy-related column at all", {
  # every one of these used to change the answer; none of them should now
  for (t in c(99, 100, 141, 142, 202, 301, 997, 998, NA_real_)) {
    result <- add_maternal_deaths(make_sib(sib.time.delivery.death = t))
    expect_equal(result$sib.preg_related.death.date, 1200L,
                 info = paste("mm12 =", t))
  }
})

test_that("na.action no longer moves the DHS pregnancy-related column", {
  inc <- add_maternal_deaths(make_sib(sib.time.delivery.death = NA_real_),
                             na.action = "include")
  exc <- add_maternal_deaths(make_sib(sib.time.delivery.death = NA_real_),
                             na.action = "exclude")
  expect_equal(inc$sib.preg_related.death.date, exc$sib.preg_related.death.date)
})


# =====================================================================
# Tests: get_ego_age_distn()
# =====================================================================

data(ex.ego)

test_that("get_ego_age_distn: agegrp_prop sums to 1", {
  result <- siblingsurvival:::get_ego_age_distn(ex.ego)
  expect_equal(sum(result$agegrp_prop), 1, tolerance = 1e-10)
})

test_that("get_ego_age_distn: returns exactly the 7 age groups from 15 to 49", {
  result <- siblingsurvival:::get_ego_age_distn(ex.ego)
  expect_equal(nrow(result), 7)
  expected_groups <- c("[15,20)", "[20,25)", "[25,30)", "[30,35)",
                       "[35,40)", "[40,45)", "[45,50)")
  expect_setequal(result$age.cat, expected_groups)
})

test_that("get_ego_age_distn: respondents outside 15-49 are excluded", {
  extra <- ex.ego %>% slice(1) %>% mutate(age.cat = "[60,65)")
  result <- siblingsurvival:::get_ego_age_distn(bind_rows(ex.ego, extra))
  expect_false("[60,65)" %in% result$age.cat)
})

test_that("get_ego_age_distn: only_females=FALSE changes the proportions", {
  # Add a batch of 'male' respondents concentrated in one age group so that
  # including them shifts the age distribution
  extra_males <- ex.ego %>%
    filter(age.cat == "[20,25)") %>%
    slice(1:200) %>%
    mutate(sex = 'm')
  ego_mixed <- bind_rows(ex.ego, extra_males)
  res_f   <- siblingsurvival:::get_ego_age_distn(ego_mixed, only_females = TRUE)
  res_all <- siblingsurvival:::get_ego_age_distn(ego_mixed, only_females = FALSE)
  expect_false(isTRUE(all.equal(res_f$agegrp_prop, res_all$agegrp_prop)))
})

test_that("get_ego_age_distn: respects sampling weights", {
  simple_ego <- tibble(
    sex     = 'f',
    age.cat = c("[20,25)", "[30,35)"),
    wwgt    = c(100, 1)          # [20,25) massively up-weighted
  )
  result <- siblingsurvival:::get_ego_age_distn(simple_ego)
  prop_20 <- result$agegrp_prop[result$age.cat == "[20,25)"]
  prop_30 <- result$agegrp_prop[result$age.cat == "[30,35)"]
  expect_gt(prop_20, prop_30)
})


# =====================================================================
# Setup for aggregate_maternal_estimates() tests:
# build estimates once, reuse across all tests below
# =====================================================================

data(ex.ego)
data(ex.sib)

ex.ego_mat <- ex.ego %>% mutate(sex = 'f')

ex.sib_mat <- ex.sib %>%
  add_maternal_deaths() %>%
  mutate(in.F = as.numeric(
    (sib.alive == 1) & (sib.age >= 15) & (sib.age <= 49) & (sib.sex == 'f')
  )) %>%
  filter(!is.na(in.F)) %>%
  filter(sib.sex == 'f')

cc_pr <- cell_config(
  age.groups  = '5yr',
  time.periods = '7yr_beforeinterview',
  start.obs   = 'sib.dob',
  end.obs     = 'sib.endobs',
  event       = 'sib.preg_related.death.date',
  age.offset  = 'sib.dob',
  time.offset = 'doi',
  event.name  = 'pr_death',
  exp.scale   = 1/12
)

ex_ests <- sibling_estimator(
  sib.dat             = ex.sib_mat,
  ego.id              = 'caseid',
  sib.id              = 'sibid',
  sib.frame.indicator = 'in.F',
  sib.sex             = 'sib.sex',
  cell.config         = cc_pr,
  weights             = 'wwgt'
)

# Tiny two-rep bootstrap (identical weights) for structure tests
dummy_boot <- tibble(
  caseid          = ex.ego_mat$caseid,
  boot_weight_1   = ex.ego_mat$wwgt,
  boot_weight_2   = ex.ego_mat$wwgt
)

ex_boot_ests <- sibling_estimator(
  sib.dat             = ex.sib_mat,
  ego.id              = 'caseid',
  sib.id              = 'sibid',
  sib.frame.indicator = 'in.F',
  sib.sex             = 'sib.sex',
  cell.config         = cc_pr,
  boot.weights        = dummy_boot,
  return.boot         = TRUE,
  weights             = 'wwgt'
)


# =====================================================================
# Tests: aggregate_maternal_estimates()
# =====================================================================

test_that("aggregate_maternal_estimates: without bootstrap, returns a tibble", {
  result <- aggregate_maternal_estimates(ex_ests, ex.ego_mat, ex.sib_mat)
  expect_s3_class(result, "tbl_df")
})

test_that("aggregate_maternal_estimates: output has all expected columns", {
  result <- aggregate_maternal_estimates(ex_ests, ex.ego_mat, ex.sib_mat)
  expect_true(all(c("ind.est", "agg.est",
                    "adj.factor", "adj.factor.allage", "adj.factor.meanagespec",
                    "ratio.agg.ind", "ratio.ind.agg") %in% names(result)))
})

test_that("aggregate_maternal_estimates: point estimates are positive", {
  result <- aggregate_maternal_estimates(ex_ests, ex.ego_mat, ex.sib_mat)
  expect_gt(result$ind.est, 0)
  expect_gt(result$agg.est, 0)
})

test_that("aggregate_maternal_estimates: with bootstrap, returns named list of 3", {
  result <- aggregate_maternal_estimates(ex_boot_ests, ex.ego_mat, ex.sib_mat)
  expect_type(result, "list")
  expect_named(result, c("point", "boot_summ", "boot"))
})

test_that("aggregate_maternal_estimates: bootstrap point estimates match non-bootstrap run", {
  result_plain <- aggregate_maternal_estimates(ex_ests,      ex.ego_mat, ex.sib_mat)
  result_boot  <- aggregate_maternal_estimates(ex_boot_ests, ex.ego_mat, ex.sib_mat)
  expect_equal(result_plain$ind.est, result_boot$point$ind.est, tolerance = 1e-8)
  expect_equal(result_plain$agg.est, result_boot$point$agg.est, tolerance = 1e-8)
})


# =====================================================================
# Tests: add_maternal_deaths() on real package data (ex.sib)
# (integration-level checks that stand in for prep_dhs_sib_histories
#  add_maternal=TRUE, which calls add_maternal_deaths internally)
# =====================================================================

test_that("add_maternal_deaths on ex.sib: adds both expected columns", {
  result <- add_maternal_deaths(ex.sib)
  expect_true("sib.preg_related.death.date" %in% names(result))
  expect_true("sib.maternal.death.date"     %in% names(result))
})

test_that("add_maternal_deaths on ex.sib: all male sibs get NA for both date columns", {
  result <- add_maternal_deaths(ex.sib)
  males  <- result %>% filter(sib.sex == 'm')
  expect_true(all(is.na(males$sib.preg_related.death.date)))
  expect_true(all(is.na(males$sib.maternal.death.date)))
})

test_that("add_maternal_deaths on ex.sib: sib.maternal.death.date all NA (no accident col)", {
  expect_false("sib.died.accident" %in% names(ex.sib))
  result  <- add_maternal_deaths(ex.sib)
  females <- result %>% filter(sib.sex == 'f')
  expect_true(all(is.na(females$sib.maternal.death.date)))
})
