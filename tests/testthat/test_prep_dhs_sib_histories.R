# Smoke tests for prep_dhs_sib_histories()
#
# These tests use the bundled model_dhs_dat dataset to verify that the
# preparation function runs without error and returns a structurally correct
# result.  They do not test for specific numerical values; they act as
# regression guards to catch accidental breakage of the prep pipeline.

test_that("prep_dhs_sib_histories runs without error on model_dhs_dat", {
  data(model_dhs_dat)
  expect_no_error(
    prepped <- prep_dhs_sib_histories(model_dhs_dat,
                                      varmap = sibhist_varmap_dhs6,
                                      keep_missing = FALSE,
                                      verbose = FALSE)
  )
})

test_that("prep_dhs_sib_histories returns ego.dat and sib.dat", {
  data(model_dhs_dat)
  prepped <- prep_dhs_sib_histories(model_dhs_dat,
                                    varmap = sibhist_varmap_dhs6,
                                    keep_missing = FALSE,
                                    verbose = FALSE)

  expect_true(is.list(prepped))
  expect_true("ego.dat" %in% names(prepped))
  expect_true("sib.dat" %in% names(prepped))
})

test_that("prep_dhs_sib_histories ego.dat has one row per respondent", {
  data(model_dhs_dat)
  prepped <- prep_dhs_sib_histories(model_dhs_dat,
                                    varmap = sibhist_varmap_dhs6,
                                    keep_missing = FALSE,
                                    verbose = FALSE)

  # ego.dat should have as many rows as there are unique respondents in the raw data
  n_ego <- nrow(prepped$ego.dat)
  expect_gt(n_ego, 0)
  # No duplicate respondent IDs
  expect_equal(n_ego, length(unique(prepped$ego.dat$caseid)))
})

test_that("prep_dhs_sib_histories sib.dat has required columns", {
  data(model_dhs_dat)
  prepped <- prep_dhs_sib_histories(model_dhs_dat,
                                    varmap = sibhist_varmap_dhs6,
                                    keep_missing = FALSE,
                                    verbose = FALSE)

  required <- c("caseid", "sib.sex", "sib.alive")
  for (col in required) {
    expect_true(col %in% names(prepped$sib.dat),
                info = paste("missing column:", col))
  }
})
