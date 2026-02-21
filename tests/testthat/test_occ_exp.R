# Unit tests for occ.exp()
#
# Tests use simple integer time units (no CMC encoding).
#
# Implementation quirk to be aware of:
#   The C++ function cpp_compute_occ_exp uses 0.0 as a sentinel for "no
#   overlap". Consequently, any age-group window whose intersection with the
#   observation window starts at exactly t=0 is incorrectly treated as having
#   no overlap, producing 0 exposure.  In practice this never matters because
#   DHS dates are CMC codes in the hundreds/thousands, so intersections never
#   start at t=0.
#
#   To avoid this edge case in tests, all absolute start times are >= 100.
#   The age_offset (= start.obs) shifts the age-group windows; a person with
#   start.obs = 100 has age 0 at t=100, age 60 at t=160, etc.
#
# All tests use exp.scale = 1 unless otherwise noted.

library(tibble)

# ---------------------------------------------------------------------------
# Helper: make two 60-unit age groups [0,60) and [60,120)
# ---------------------------------------------------------------------------
two_age_groups <- function() {
  make.age.groups(start = 0, widths = c(60, 60),
                  names = c("[0,60)", "[60,120)"))
}

# ---------------------------------------------------------------------------
# Test 1: single alive individual — exposure split correctly across two groups
# ---------------------------------------------------------------------------
# start.obs = 100  → alpha_offset = 100
# Absolute age windows: [0,60)+100 = [100,160) and [60,120)+100 = [160,220)
# Observation [100, 220), time period [100, 220).
# Expected: exp=60 in each age group; occ=0 everywhere.
test_that("occ.exp: alive individual accumulates correct exposure in each age group", {
  dat      <- tibble(start_obs = 100, end_obs = 220, event = -1)
  age_grps <- two_age_groups()
  time_per <- make.time.periods(start = 100, durations = 120, names = "full")

  res <- occ.exp(data = dat, start.obs = "start_obs", end.obs = "end_obs",
                 event = "event", age.groups = age_grps,
                 time.periods = time_per, exp.scale = 1)

  r1 <- res[res$agelabel == "[0,60)",   ]
  r2 <- res[res$agelabel == "[60,120)", ]

  expect_equal(r1$occ, 0);  expect_equal(r1$exp, 60)
  expect_equal(r2$occ, 0);  expect_equal(r2$exp, 60)
})

# ---------------------------------------------------------------------------
# Test 2: death within an age group — exposure truncated, one occurrence
# ---------------------------------------------------------------------------
# start.obs = 100.  Sib dies at t=175 (= age 75, in age group [60,120)).
# Time period [100, 220).
# Expected:
#   [0,60)  → [100,160) absolute: fully observed → exp=60, occ=0
#   [60,120) → [160,220) absolute: observed 160..175 → exp=15, occ=1
test_that("occ.exp: death is counted and exposure is truncated at death", {
  dat      <- tibble(start_obs = 100, end_obs = 175, event = 175)
  age_grps <- two_age_groups()
  time_per <- make.time.periods(start = 100, durations = 120, names = "full")

  res <- occ.exp(data = dat, start.obs = "start_obs", end.obs = "end_obs",
                 event = "event", age.groups = age_grps,
                 time.periods = time_per, exp.scale = 1)

  r1 <- res[res$agelabel == "[0,60)",   ]
  r2 <- res[res$agelabel == "[60,120)", ]

  expect_equal(r1$occ, 0);  expect_equal(r1$exp, 60)
  expect_equal(r2$occ, 1);  expect_equal(r2$exp, 15)
})

# ---------------------------------------------------------------------------
# Test 3: two individuals aggregate correctly
# ---------------------------------------------------------------------------
# A: alive [100, 220).  B: dies at 175 (same as Test 2).
# Expected totals:
#   [0,60):  exp = 60+60 = 120, occ = 0
#   [60,120): exp = 60+15 = 75,  occ = 0+1 = 1
test_that("occ.exp: exposures and deaths aggregate correctly across individuals", {
  dat <- tibble(
    start_obs = c(100, 100),
    end_obs   = c(220, 175),
    event     = c(-1,  175)
  )
  age_grps <- two_age_groups()
  time_per <- make.time.periods(start = 100, durations = 120, names = "full")

  res <- occ.exp(data = dat, start.obs = "start_obs", end.obs = "end_obs",
                 event = "event", age.groups = age_grps,
                 time.periods = time_per, exp.scale = 1)

  r1 <- res[res$agelabel == "[0,60)",   ]
  r2 <- res[res$agelabel == "[60,120)", ]

  expect_equal(r1$occ, 0);  expect_equal(r1$exp, 120)
  expect_equal(r2$occ, 1);  expect_equal(r2$exp, 75)
})

# ---------------------------------------------------------------------------
# Test 4: observation window covers only part of an age group
# ---------------------------------------------------------------------------
# start.obs = 130  → alpha_offset = 130
# Absolute age windows: [0,60)+130 = [130,190), [60,120)+130 = [190,250).
# Observation [130, 180), time period [100, 220).
# Expected:
#   [0,60):  exposure from 130 to 180 = 50; occ=0
#   [60,120): observation ends before [190,250) starts → exp=0, occ=0
test_that("occ.exp: exposure is limited when observation ends before age group ends", {
  dat      <- tibble(start_obs = 130, end_obs = 180, event = -1)
  age_grps <- two_age_groups()
  time_per <- make.time.periods(start = 100, durations = 120, names = "full")

  res <- occ.exp(data = dat, start.obs = "start_obs", end.obs = "end_obs",
                 event = "event", age.groups = age_grps,
                 time.periods = time_per, exp.scale = 1)

  r1 <- res[res$agelabel == "[0,60)",   ]
  r2 <- res[res$agelabel == "[60,120)", ]

  expect_equal(r1$occ, 0);  expect_equal(r1$exp, 50)
  expect_equal(r2$occ, 0);  expect_equal(r2$exp, 0)
})

# ---------------------------------------------------------------------------
# Test 5: exp.scale is applied to exposure but not to occurrence counts
# ---------------------------------------------------------------------------
# Same scenario as Test 1 but with exp.scale = 1/12.
# Expected: exp = 60/12 = 5 person-years per group; occ unchanged = 0.
test_that("occ.exp: exp.scale scales exposure but not occurrence counts", {
  dat      <- tibble(start_obs = 100, end_obs = 220, event = -1)
  age_grps <- two_age_groups()
  time_per <- make.time.periods(start = 100, durations = 120, names = "full")

  res <- occ.exp(data = dat, start.obs = "start_obs", end.obs = "end_obs",
                 event = "event", age.groups = age_grps,
                 time.periods = time_per, exp.scale = 1/12)

  r1 <- res[res$agelabel == "[0,60)",   ]
  r2 <- res[res$agelabel == "[60,120)", ]

  expect_equal(r1$occ, 0);  expect_equal(r1$exp, 5)
  expect_equal(r2$occ, 0);  expect_equal(r2$exp, 5)
})
