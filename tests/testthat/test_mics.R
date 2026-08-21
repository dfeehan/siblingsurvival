# Tests for the MICS path: prep_mics_sib_histories() and its helpers.
#
# The fixture is in helper-simulate-mics.R and is shaped after the real MICS6
# mm.sav. Real MICS microdata is registration-gated and cannot ship here; the
# validation of these results against published MICS estimates lives in
# data-raw/mics-validation/.

library(dplyr)

# =====================================================================
# Basic contract
# =====================================================================

test_that("prep_mics_sib_histories: returns the standard four-part list", {
  r <- prep_mics_sib_histories(make_mics6_mm_with_cmc(), survey = "TEST2019",
                               varmap = make_mics6_varmap_cmc(), verbose = FALSE)

  expect_setequal(names(r), c("survey", "ego.dat", "sib.dat", "summ"))
  expect_equal(r$survey, "TEST2019")
  expect_equal(nrow(r$ego.dat), 3)
})

test_that("prep_mics_sib_histories: survey is required", {
  expect_error(
    prep_mics_sib_histories(make_mics6_mm_with_cmc(),
                            varmap = make_mics6_varmap_cmc(), verbose = FALSE),
    "survey")
})

test_that("prep_mics_sib_histories: caseid is built and unique per respondent", {
  r <- prep_mics_sib_histories(make_mics6_mm_with_cmc(), survey = "TEST2019",
                               varmap = make_mics6_varmap_cmc(), verbose = FALSE)

  expect_equal(dplyr::n_distinct(r$ego.dat$caseid), nrow(r$ego.dat))
  expect_setequal(r$ego.dat$caseid, c("1.1.2", "1.3.1", "2.1.1"))
})

test_that("prep_mics_sib_histories: sibid is unique across siblings", {
  r <- prep_mics_sib_histories(make_mics6_mm_with_cmc(), survey = "TEST2019",
                               varmap = make_mics6_varmap_cmc(), verbose = FALSE)
  expect_equal(dplyr::n_distinct(r$sib.dat$sibid), nrow(r$sib.dat))
})

# =====================================================================
# The sib.alive recode -- the highest-consequence MICS/DHS difference
# =====================================================================

test_that("prep_mics_sib_histories: dead siblings are kept, not dropped", {
  # MICS codes MM16 as 1 yes / 2 no / 8 DK, while the package expects the DHS
  # 1 alive / 0 dead. Passing the MICS codes through unchanged makes every dead
  # sibling look like missing survival status, which silently drives every
  # mortality estimate to zero.
  r <- prep_mics_sib_histories(make_mics6_mm_with_cmc(), survey = "TEST2019",
                               varmap = make_mics6_varmap_cmc(), verbose = FALSE)

  expect_equal(sum(r$sib.dat$sib.alive == 0), 5)
  expect_equal(sum(r$sib.dat$sib.alive == 1), 3)
})

test_that("recode_mics_sib_vars: maps 1/2/8 to 1/0/NA", {
  d <- tibble(sib.alive = c(1, 2, 8, 9, NA))
  out <- siblingsurvival:::recode_mics_sib_vars(d, verbose = FALSE)
  expect_equal(out$sib.alive, c(1, 0, NA, NA, NA))
})

test_that("recode_mics_sib_vars: errors on an unexpected survival code", {
  expect_error(
    siblingsurvival:::recode_mics_sib_vars(tibble(sib.alive = c(1, 2, 3)),
                                           verbose = FALSE),
    "Unexpected values in sib.alive")
})

test_that("prep_mics_sib_histories: a DK sibling is dropped", {
  r <- prep_mics_sib_histories(make_mics6_mm_with_cmc(), survey = "TEST2019",
                               varmap = make_mics6_varmap_cmc(), verbose = FALSE)
  # fixture has one MM16 == 8
  expect_equal(r$summ$n.sib.raw - r$summ$n.sib, 1)
  expect_equal(r$summ$miss.alive, 1)
})

# =====================================================================
# sib.sex
# =====================================================================

test_that("recode_mics_sib_vars: 8/9 sex codes become NA, not male", {
  # get_sib_df() uses ifelse(sib.sex == 2, 'f', 'm'), so anything that is not
  # literally 2 becomes male unless it is blanked first
  d <- tibble(sib.sex = c(1, 2, 9, 8))
  out <- siblingsurvival:::recode_mics_sib_vars(d, verbose = FALSE)
  expect_equal(out$sib.sex, c(1, 2, NA, NA))
})

test_that("recode_mics_sib_vars: errors on an unexpected sex code", {
  expect_error(
    siblingsurvival:::recode_mics_sib_vars(tibble(sib.sex = c(1, 2, 3)),
                                           verbose = FALSE),
    "Unexpected values in sib.sex")
})

# =====================================================================
# The MM16 collision
# =====================================================================

test_that("check_mics_varmap: rejects mm16 -> sib.died.accident", {
  bad <- make_mics6_varmap() %>%
    mutate(new.varname = ifelse(orig.varname == "mm16",
                                "sib.died.accident", new.varname))
  expect_error(siblingsurvival:::check_mics_varmap(bad), "MM16 is 'Is \\(name\\) still alive")
})

test_that("check_mics_varmap: accepts the correct mm16 mapping", {
  expect_silent(siblingsurvival:::check_mics_varmap(make_mics6_varmap()))
})

test_that("the shipped MICS varmaps map mm16 to sib.alive", {
  expect_equal(
    sibhist_varmap_mics6$new.varname[sibhist_varmap_mics6$orig.varname == "mm16"],
    "sib.alive")
})

# =====================================================================
# Constructed columns
# =====================================================================

test_that("prep_mics_sib_histories: doi comes from the CMC column", {
  r <- prep_mics_sib_histories(make_mics6_mm_with_cmc(), survey = "TEST2019",
                               varmap = make_mics6_varmap_cmc(), verbose = FALSE)
  expect_true(all(r$ego.dat$doi == 1400))
})

test_that("prep_mics_sib_histories: doi is built from year and month when no CMC", {
  mm <- make_mics6_mm_with_cmc() %>%
    select(-WDOI) %>%
    mutate(WM6Y = 2016L, WM6M = 8L)
  # 12*(2016-1900) + 8 = 1400
  r <- prep_mics_sib_histories(mm, survey = "TEST2019",
                               varmap = make_mics6_varmap_cmc(), verbose = FALSE)
  expect_true(all(r$ego.dat$doi == 1400))
})

test_that("prep_mics_sib_histories: respondent age comes from doi and wdob", {
  r <- prep_mics_sib_histories(make_mics6_mm_with_cmc(), survey = "TEST2019",
                               varmap = make_mics6_varmap_cmc(), verbose = FALSE)
  expect_setequal(r$ego.dat$age, c(28, 35, 41))
})

test_that("prep_mics_sib_histories: psu is built from the cluster when absent", {
  mm <- make_mics6_mm_with_cmc() %>% select(-psu)
  r <- prep_mics_sib_histories(mm, survey = "TEST2019",
                               varmap = make_mics6_varmap_cmc(), verbose = FALSE)
  expect_equal(r$ego.dat$psu, c(1L, 1L, 2L))
})

test_that("prep_mics_sib_histories: errors helpfully when doi cannot be built", {
  mm <- make_mics6_mm_with_cmc() %>% select(-WDOI)
  expect_error(
    prep_mics_sib_histories(mm, survey = "TEST2019",
                            varmap = make_mics6_varmap_cmc(), verbose = FALSE),
    "Cannot build doi")
})

# =====================================================================
# Weights -- MICS weights are already normalized
# =====================================================================

test_that("prep_mics_sib_histories: weights are not divided by 1e6", {
  r <- prep_mics_sib_histories(make_mics6_mm_with_cmc(), survey = "TEST2019",
                               varmap = make_mics6_varmap_cmc(), verbose = FALSE)
  expect_setequal(r$ego.dat$wwgt, c(1.0, 1.2, 0.8))
})

# =====================================================================
# Case handling -- MICS files are mixed case
# =====================================================================

test_that("prep_mics_sib_histories: uppercase and lowercase input agree", {
  mm_up  <- make_mics6_mm_with_cmc()
  mm_low <- mm_up; names(mm_low) <- tolower(names(mm_low))

  a <- prep_mics_sib_histories(mm_up,  survey = "T", varmap = make_mics6_varmap_cmc(), verbose = FALSE)
  b <- prep_mics_sib_histories(mm_low, survey = "T", varmap = make_mics6_varmap_cmc(), verbose = FALSE)

  expect_equal(nrow(a$sib.dat), nrow(b$sib.dat))
  expect_equal(a$sib.dat$sib.dob, b$sib.dat$sib.dob)
})

test_that("prep_mics_sib_histories: lowercase=FALSE fails on an uppercase file", {
  # without normalisation the varmap matches nothing, so the required sibling
  # columns are absent
  expect_error(
    prep_mics_sib_histories(make_mics6_mm_with_cmc(), survey = "T",
                            varmap = make_mics6_varmap_cmc(),
                            lowercase = FALSE, verbose = FALSE))
})

# =====================================================================
# Supplied vs derived sibling dates (M1)
# =====================================================================

test_that("prep_mics_sib_histories: derives sib.dob/sib.death.date when absent", {
  # drop MM17C/MM18C, as MICS4/5 BTN_2010 and some MICS6 countries do
  r <- prep_mics_sib_histories(make_mics6_mm(), survey = "TEST2019",
                               varmap = make_mics6_varmap(), verbose = FALSE)

  expect_true(all(!is.na(r$sib.dat$sib.dob)))
  # dead siblings get a real death date, living ones the -1 sentinel
  expect_true(all(r$sib.dat$sib.death.date[r$sib.dat$sib.alive == 1] == -1))
  expect_true(all(r$sib.dat$sib.death.date[r$sib.dat$sib.alive == 0] > 0))
})

test_that("prep_mics_sib_histories: supplied and derived dates agree", {
  supplied <- prep_mics_sib_histories(make_mics6_mm_with_cmc(), survey = "T",
                                      varmap = make_mics6_varmap_cmc(), verbose = FALSE)
  derived  <- prep_mics_sib_histories(make_mics6_mm(), survey = "T",
                                      varmap = make_mics6_varmap(), verbose = FALSE)

  key <- function(x) paste(x$caseid, x$sibindex)
  expect_equal(key(supplied$sib.dat), key(derived$sib.dat))
  expect_equal(supplied$sib.dat$sib.dob, derived$sib.dat$sib.dob)
  expect_equal(supplied$sib.dat$sib.death.date, derived$sib.dat$sib.death.date)
})

# =====================================================================
# The shipped varmaps
# =====================================================================

test_that("shipped MICS varmaps have the expected roster numbering", {
  m6 <- sibhist_varmap_mics6
  m4 <- sibhist_varmap_mics4

  expect_equal(m6$orig.varname[m6$new.varname == "sib.sex"], "mm15")
  expect_equal(m6$orig.varname[m6$new.varname == "sib.death.age"], "mm19")
  expect_equal(m4$orig.varname[m4$new.varname == "sib.sex"], "mm5")
  expect_equal(m4$orig.varname[m4$new.varname == "sib.death.age"], "mm9")
})

test_that("only MICS6/7 carry the accident item", {
  # MICS4/5 have no violence/accident question, so they support
  # pregnancy-related mortality only
  expect_true("sib.died.accident" %in% sibhist_varmap_mics6$new.varname)
  expect_true("sib.died.accident" %in% sibhist_varmap_mics7$new.varname)
  expect_false("sib.died.accident" %in% sibhist_varmap_mics4$new.varname)
  expect_false("sib.died.accident" %in% sibhist_varmap_mics5$new.varname)
})

test_that("shipped MICS varmaps are all lowercase", {
  for (vm in list(sibhist_varmap_mics4, sibhist_varmap_mics6)) {
    expect_equal(vm$orig.varname, tolower(vm$orig.varname))
  }
})

# =====================================================================
# cell_config(): custom time.periods
# =====================================================================

test_that("cell_config: accepts a make.time.periods() object", {
  # the documentation says time.periods "can either be the output of
  # make.time.periods, or ... '7yr_beforeinterview'", and age.groups handles a
  # custom object, but time.periods used to stop() on anything non-character
  tp <- make.time.periods(start = -12*8, durations = 12*7, names = "alt")

  cc <- cell_config(age.groups = '5yr', time.periods = tp,
                    start.obs = 'sib.dob', end.obs = 'sib.endobs',
                    event = 'sib.death.date', age.offset = 'sib.dob',
                    time.offset = 'doi', exp.scale = 1/12)

  expect_equal(cc$time.periods$names, "alt")
  expect_equal(as.numeric(cc$time.periods$template[1, "start"]), -96)
})

test_that("cell_config: still rejects an unknown character setting", {
  expect_error(
    cell_config(age.groups = '5yr', time.periods = 'nonsense',
                start.obs = 'sib.dob', end.obs = 'sib.endobs',
                event = 'sib.death.date', age.offset = 'sib.dob',
                time.offset = 'doi', exp.scale = 1/12),
    "No setting found for time.periods")
})

# =====================================================================
# MICS 98/99 don't-know codes on the numeric sibling items
# =====================================================================

test_that("recode_mics_sib_vars: 98/99 on numeric items become NA", {
  d <- tibble(sib.age = c(30, 99, NA),
              sib.death.yrsago = c(98, 5, 99),
              sib.death.age = c(99, 40, 98))
  out <- siblingsurvival:::recode_mics_sib_vars(d, verbose = FALSE)

  expect_equal(out$sib.age, c(30, NA, NA))
  expect_equal(out$sib.death.yrsago, c(NA, 5, NA))
  expect_equal(out$sib.death.age, c(NA, 40, NA))
})

test_that("prep_mics_sib_histories: a DK age at death does not corrupt sib.dob", {
  # MM19 = 98 passed through as a real value would put the birth date 98 years
  # before the death date. The damage is masked whenever MICS supplies MM17C /
  # MM18C, so this exercises the derivation path, as in surveys that do not.
  mm <- make_mics6_mm()
  mm$MM19[mm$MM19 == 20] <- 98L          # a dead sibling with DK age at death

  r <- prep_mics_sib_histories(mm, survey = "T", varmap = make_mics6_varmap(),
                               verbose = FALSE)

  # every derived birth date should be within a plausible span of the interview
  expect_true(all(r$sib.dat$sib.dob > r$sib.dat$doi - 12 * 100, na.rm = TRUE))
  # and the DK row should have NA age at death rather than 98
  expect_false(any(r$sib.dat$sib.death.age %in% c(98, 99)))
})

test_that("prep_mics_sib_histories: DK current age does not survive as 99", {
  mm <- make_mics6_mm_with_cmc()
  mm$MM17[!is.na(mm$MM17)][1] <- 99L

  r <- prep_mics_sib_histories(mm, survey = "T", varmap = make_mics6_varmap_cmc(),
                               verbose = FALSE)
  expect_false(any(r$sib.dat$sib.age %in% c(98, 99), na.rm = TRUE))
})
