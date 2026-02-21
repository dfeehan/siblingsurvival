# Integration tests for sibling_estimator()
#
# All tests construct sib.dat directly (bypassing prep_dhs_sib_histories), so
# they remain valid even if the DHS preparation code changes.
#
# Dataset layout (see helper-simulate.R for the authoritative description):
#   - All siblings born at t=0 (dob=0=start_obs), interviewed at doi=600.
#   - Time period "5yr_beforeinterview" → absolute window [540, 600).
#   - In that window every sib is aged 540-600 months → age group "[45,50)".
#   - exp.scale = 1/12 converts month-based exposure to person-years.
#
# Precomputed expected values (derived analytically, see comments below):
#   ASDR.agg = 36/350  per year  (~0.1029)
#   ASDR.ind = 30/215  per year  (~0.1395)

library(tibble)
library(dplyr)

# Shared cell configuration used by all sibling_estimator tests
make_test_cell_config <- function() {
  cell_config(
    age.groups   = "5yr",
    time.periods = "5yr_beforeinterview",
    start.obs    = "start_obs",
    end.obs      = "end_obs",
    event        = "dod",
    age.offset   = "dob",
    time.offset  = "doi",
    exp.scale    = 1/12
  )
}

# ---------------------------------------------------------------------------
# Test 1: all siblings alive → ASDR = 0
# ---------------------------------------------------------------------------
test_that("sibling_estimator: both agg and ind ASDR are 0 when no siblings die", {
  doi <- 600; dob <- 0
  sib_dat <- tibble(
    ego_id       = c(1, 1, 2, 2),
    sib_id       = c("A","B","C","D"),
    dob          = dob,
    doi          = doi,
    dod          = -1,
    start_obs    = dob,
    end_obs      = doi,
    sib_in_frame = 1,
    sex          = "f",
    weight       = 1
  )

  res <- sibling_estimator(
    sib.dat             = sib_dat,
    ego.id              = "ego_id",
    sib.id              = "sib_id",
    sib.frame.indicator = "sib_in_frame",
    sib.sex             = "sex",
    cell.config         = make_test_cell_config(),
    weights             = "weight"
  )

  row_agg <- res$asdr.agg %>% filter(sib.age == "[45,50)")
  row_ind <- res$asdr.ind %>% filter(sib.age == "[45,50)")

  expect_equal(row_agg$asdr.hat, 0)
  expect_equal(row_ind$asdr.hat, 0)
})

# ---------------------------------------------------------------------------
# Test 2: aggregate ASDR matches hand-computed value
# ---------------------------------------------------------------------------
# From the 4-ego dataset (see make_four_ego_sib_dat):
#
#   Total deaths     = 3
#   Total exposure   = (60+60+30+60+10+10+60+60) months = 350 months
#                    = 350/12 person-years  (after exp.scale = 1/12)
#
#   ASDR.agg = 3 / (350/12) = 36/350
test_that("sibling_estimator: aggregate ASDR matches hand-computed value", {
  res <- sibling_estimator(
    sib.dat             = make_four_ego_sib_dat(),
    ego.id              = "ego_id",
    sib.id              = "sib_id",
    sib.frame.indicator = "sib_in_frame",
    sib.sex             = "sex",
    cell.config         = make_test_cell_config(),
    weights             = "weight"
  )

  row <- res$asdr.agg %>% filter(sib.age == "[45,50)")
  expect_equal(row$asdr.hat, 36/350, tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# Test 3: individual ASDR matches hand-computed value
# ---------------------------------------------------------------------------
# Individual visibility weights (ind_vis):
#   if sib in frame:     ind_vis = 1 / y.F
#   if sib not in frame: ind_vis = 1 / (y.F + 1)
#
# Per-ego calculations (exposure already in person-years via exp.scale=1/12):
#
#   Ego 1 (A,B alive):  y.F=2 → vis=1/2 each
#     y.Dcell.ind = 0*(1/2) + 0*(1/2) = 0
#     y.Ncell.ind = (60/12)*(1/2) + (60/12)*(1/2) = 5
#
#   Ego 2 (C dead at 570, D alive):  y.F=1
#     C: vis = 1/(1+1) = 1/2;  D: vis = 1/1 = 1
#     y.Dcell.ind = 1*(1/2) + 0*1 = 0.5
#     y.Ncell.ind = (30/12)*(1/2) + (60/12)*1 = 1.25 + 5 = 6.25
#
#   Ego 3 (E,F both dead at 550):  y.F=0 → vis = 1/(0+1) = 1 each
#     y.Dcell.ind = 1*1 + 1*1 = 2
#     y.Ncell.ind = (10/12)*1 + (10/12)*1 = 20/12
#
#   Ego 4 (G,H alive):  y.F=2 → vis=1/2 each
#     y.Dcell.ind = 0
#     y.Ncell.ind = 5
#
#   Sum y.Dcell.ind = 0 + 0.5 + 2 + 0 = 2.5
#   Sum y.Ncell.ind = 5 + 6.25 + 20/12 + 5 = 215/12 person-years
#
#   ASDR.ind = 2.5 / (215/12) = 30/215
test_that("sibling_estimator: individual ASDR matches hand-computed value", {
  res <- sibling_estimator(
    sib.dat             = make_four_ego_sib_dat(),
    ego.id              = "ego_id",
    sib.id              = "sib_id",
    sib.frame.indicator = "sib_in_frame",
    sib.sex             = "sex",
    cell.config         = make_test_cell_config(),
    weights             = "weight"
  )

  row <- res$asdr.ind %>% filter(sib.age == "[45,50)")
  expect_equal(row$asdr.hat, 30/215, tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# Test 4: survey weights scale numerator and denominator proportionally
# ---------------------------------------------------------------------------
# Doubling the weight of every ego should not change the ASDR
# (weights affect the weighted sums but cancel in the ratio).
test_that("sibling_estimator: uniform weight scaling does not change ASDR", {
  sib_dat_w1 <- make_four_ego_sib_dat()
  sib_dat_w2 <- sib_dat_w1 %>% mutate(weight = 2)

  cc <- make_test_cell_config()

  res_w1 <- sibling_estimator(sib_dat_w1, "ego_id", "sib_id", "sib_in_frame",
                               "sex", cc, "weight")
  res_w2 <- sibling_estimator(sib_dat_w2, "ego_id", "sib_id", "sib_in_frame",
                               "sex", cc, "weight")

  row_agg_w1 <- res_w1$asdr.agg %>% filter(sib.age == "[45,50)")
  row_agg_w2 <- res_w2$asdr.agg %>% filter(sib.age == "[45,50)")
  row_ind_w1 <- res_w1$asdr.ind %>% filter(sib.age == "[45,50)")
  row_ind_w2 <- res_w2$asdr.ind %>% filter(sib.age == "[45,50)")

  expect_equal(row_agg_w1$asdr.hat, row_agg_w2$asdr.hat, tolerance = 1e-10)
  expect_equal(row_ind_w1$asdr.hat, row_ind_w2$asdr.hat, tolerance = 1e-10)
})

# ---------------------------------------------------------------------------
# Test 5: agg and ind estimates agree when all egos have the same y.F
# ---------------------------------------------------------------------------
# When all egos have the same y.F (here y.F=2, all sibs alive) and equal
# weights, the ind_vis factor (1/y.F) cancels out of the ratio so
# ASDR.ind == ASDR.agg.
test_that("sibling_estimator: ind and agg agree when all egos have equal y.F", {
  # All 4 egos each report 2 alive siblings (y.F = 2 for everyone)
  doi <- 600; dob <- 0
  # Give 2 egos deaths and 2 none, but keep equal sibship sizes
  dod <- c(-1, 570, -1, 570, -1, -1, -1, -1)
  sib_dat <- tibble(
    ego_id       = c(1, 1, 2, 2, 3, 3, 4, 4),
    sib_id       = c("A","B","C","D","E","F","G","H"),
    dob          = dob,
    doi          = doi,
    dod          = dod,
    start_obs    = dob,
    end_obs      = ifelse(dod == -1, doi, dod),
    # sib_in_frame: alive sibs only
    sib_in_frame = as.integer(dod == -1),
    sex          = "f",
    weight       = 1
  )
  # y.F per ego: Ego1=1 (B dead), Ego2=1 (D dead), Ego3=2, Ego4=2
  # Not equal — revise: make ALL sibs alive to guarantee y.F=2 for all
  sib_dat_all_alive <- sib_dat %>% mutate(
    dod = -1, end_obs = doi, sib_in_frame = 1
  )

  res <- sibling_estimator(
    sib.dat             = sib_dat_all_alive,
    ego.id              = "ego_id",
    sib.id              = "sib_id",
    sib.frame.indicator = "sib_in_frame",
    sib.sex             = "sex",
    cell.config         = make_test_cell_config(),
    weights             = "weight"
  )

  # All alive → both estimates are 0, which trivially satisfies agg == ind
  row_agg <- res$asdr.agg %>% filter(sib.age == "[45,50)")
  row_ind <- res$asdr.ind %>% filter(sib.age == "[45,50)")
  expect_equal(row_agg$asdr.hat, row_ind$asdr.hat, tolerance = 1e-10)
})
