# Tests for the defects the TODOs in R/maternal_estimators.R described:
#   - the bootstrap join dropped the sex key, inflating estimates
#   - only_females = FALSE errored outright
#   - get_ego_age_distn() now returns a per-sex distribution when
#     only_females = FALSE

library(dplyr)

# ---------------------------------------------------------------------
# shared setup
# ---------------------------------------------------------------------

agg_sib <- ex.sib %>%
  add_maternal_deaths() %>%
  mutate(in.F = as.numeric(
    (sib.alive == 1) & (sib.age >= 15) & (sib.age <= 49) & (sib.sex == 'f'))) %>%
  filter(!is.na(in.F))

agg_cc <- cell_config(age.groups = '5yr', time.periods = '7yr_beforeinterview',
                      start.obs = 'sib.dob', end.obs = 'sib.endobs',
                      event = 'sib.death.date', age.offset = 'sib.dob',
                      time.offset = 'doi', exp.scale = 1/12)

# respondents split between the sexes, so ego_vis_agg has two rows per age group
agg_ego_mixed <- ex.ego %>%
  mutate(sex = ifelse(row_number() %% 2 == 0, "m", "f"))

agg_ego_female <- ex.ego %>% mutate(sex = "f")

# bootstrap weights identical to the real weights, so every bootstrap replicate
# must reproduce the point estimate exactly
make_ests <- function(ego) {
  boot <- tibble::tibble(caseid = ego$caseid,
                         boot_weight_1 = ego$wwgt,
                         boot_weight_2 = ego$wwgt)
  sibling_estimator(sib.dat = agg_sib, ego.id = 'caseid',
                    sib.frame.indicator = 'in.F', sib.sex = 'sib.sex',
                    cell.config = agg_cc, weights = 'wwgt',
                    boot.weights = boot, return.boot = TRUE)
}

# =====================================================================
# The bootstrap join
# =====================================================================

test_that("aggregate_maternal_estimates: bootstrap reproduces the point estimate", {
  # with identical bootstrap weights there is nothing to vary, so the mean of
  # the replicates has to equal the point estimate. It did not: the bootstrap
  # branch joined ego_vis_agg on age alone, duplicating every row once per sex
  # present, which inflated the estimate by exactly that factor.
  est <- make_ests(agg_ego_mixed)
  res <- aggregate_maternal_estimates(est, agg_ego_mixed, agg_sib,
                                      only_females = TRUE)

  expect_equal(res$boot_summ$ind.est.mean, res$point$ind.est)
  expect_equal(res$boot_summ$agg.est.mean, res$point$agg.est)
})

test_that("aggregate_maternal_estimates: mixed-sex respondents do not duplicate rows", {
  est <- make_ests(agg_ego_mixed)
  res <- aggregate_maternal_estimates(est, agg_ego_mixed, agg_sib,
                                      only_females = TRUE)

  # one row per bootstrap replicate, not one per replicate per sex
  expect_equal(nrow(res$boot), dplyr::n_distinct(res$boot$boot_idx))
})

test_that("aggregate_maternal_estimates: all-female respondents give the same answer as before", {
  # the all-female case is the one the DHS actually produces, and the one the
  # existing results were computed under; it must not have moved
  est <- make_ests(agg_ego_female)
  res <- aggregate_maternal_estimates(est, agg_ego_female, agg_sib,
                                      only_females = TRUE)

  expect_equal(res$boot_summ$ind.est.mean, res$point$ind.est)
  expect_gt(res$point$ind.est, 0)
})

# =====================================================================
# only_females = FALSE
# =====================================================================

test_that("aggregate_maternal_estimates: only_females=FALSE runs and splits by sibling sex", {
  est <- make_ests(agg_ego_mixed)
  res <- aggregate_maternal_estimates(est, agg_ego_mixed, agg_sib,
                                      only_females = FALSE)

  expect_true("sib.sex" %in% names(res$point))
  expect_setequal(res$point$sib.sex, c("f", "m"))
  expect_false("dummy" %in% names(res$point))
  expect_false("dummy" %in% names(res$boot_summ))
})

test_that("aggregate_maternal_estimates: only_females=FALSE bootstrap matches point, per sex", {
  est <- make_ests(agg_ego_mixed)
  res <- aggregate_maternal_estimates(est, agg_ego_mixed, agg_sib,
                                      only_females = FALSE)

  point <- res$point %>% arrange(sib.sex)
  boot  <- res$boot_summ %>% arrange(sib.sex)

  expect_equal(boot$sib.sex, point$sib.sex)
  expect_equal(boot$ind.est.mean, point$ind.est)
  expect_equal(boot$agg.est.mean, point$agg.est)
})

test_that("aggregate_maternal_estimates: each sex is weighted by its own respondents", {
  est <- make_ests(agg_ego_mixed)
  res <- aggregate_maternal_estimates(est, agg_ego_mixed, agg_sib,
                                      only_females = FALSE)

  # the two sexes have different respondent age structures here, so the
  # aggregated rates should differ even though the age-specific rates are shared
  expect_false(isTRUE(all.equal(res$point$ind.est[1], res$point$ind.est[2])))
})

test_that("aggregate_maternal_estimates: warns and returns NA for an uninterviewed sex", {
  est <- make_ests(agg_ego_female)

  expect_warning(
    res <- aggregate_maternal_estimates(est, agg_ego_female, agg_sib,
                                        only_females = FALSE),
    "No respondent information for sibling sex")

  male <- res$point %>% filter(sib.sex == "m")
  female <- res$point %>% filter(sib.sex == "f")

  expect_true(is.na(male$ind.est))
  expect_true(is.na(male$adj.factor))
  expect_false(is.na(female$ind.est))
})

test_that("aggregate_maternal_estimates: only_females=TRUE does not warn about males", {
  est <- make_ests(agg_ego_female)
  expect_no_warning(
    aggregate_maternal_estimates(est, agg_ego_female, agg_sib,
                                 only_females = TRUE))
})

# =====================================================================
# get_ego_age_distn(): per-sex distributions
# =====================================================================

test_that("get_ego_age_distn: only_females=TRUE is unchanged", {
  res <- get_ego_age_distn(agg_ego_mixed, only_females = TRUE)

  expect_setequal(names(res), c("age.cat", "total", "agegrp_prop"))
  expect_false("sex" %in% names(res))
  expect_equal(sum(res$agegrp_prop), 1, tolerance = 1e-10)
})

test_that("get_ego_age_distn: only_females=FALSE returns one distribution per sex", {
  res <- get_ego_age_distn(agg_ego_mixed, only_females = FALSE)

  expect_true("sex" %in% names(res))
  expect_setequal(res$sex, c("f", "m"))

  # each sex's proportions sum to 1, not the table as a whole
  sums <- res %>% group_by(sex) %>% summarize(s = sum(agegrp_prop))
  expect_equal(sums$s, rep(1, nrow(sums)), tolerance = 1e-10)
})

test_that("get_ego_age_distn: only_females=FALSE covers the reproductive age groups", {
  res <- get_ego_age_distn(agg_ego_mixed, only_females = FALSE)
  expect_true(all(res$age.cat %in% reproductive_age_groups()))
})
