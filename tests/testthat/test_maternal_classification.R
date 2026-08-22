# Tests for add_maternal_deaths() and the classification helpers it dispatches
# to (R/maternal_classification.R).

library(dplyr)

# A tiny MICS6-coded sibling frame exercising each skip pattern.
mics_sibs <- function() {
  tibble::tribble(
    ~who,             ~sib.sex, ~sib.death.age, ~sib.death.date,
    "pregnant",            "f",            25L,           1300,
    "childbirth",          "f",            30L,           1310,
    "postpartum_20d",      "f",            28L,           1320,
    "postpartum_55d",      "f",            33L,           1330,
    "postpartum_NAdays",   "f",            29L,           1340,
    "accident",            "f",            40L,           1350,
    "under12",             "f",            8L,            1360,
    "male",                "m",            35L,           1370,
    "alive",               "f",            NA_integer_,     -1
  ) %>%
    mutate(
      # the "accident" row died *while pregnant* AND of an accident: that is the
      # case where the two estimands diverge on cause rather than on timing
      sib.preg.at.death         = c(1, 2, 2, 2, 2, 1, NA, NA, NA),
      sib.died.childbirth       = c(NA, 1, 2, 2, 2, 2, NA, NA, NA),
      sib.died.postpartum       = c(NA, NA, 1, 1, 1, 2, NA, NA, NA),
      sib.days.postpartum.death = c(NA, NA, 20, 55, NA, NA, NA, NA, NA),
      # MM23 == 1 skips MM26/MM27, so childbirth has NA on both
      sib.died.violence         = c(2, NA, 2, 2, 2, 2, 2, 2, NA),
      sib.died.accident         = c(2, NA, 2, 2, 2, 1, 2, 2, NA)
    )
}

pr_of <- function(x, na.action = "include") {
  d <- add_maternal_deaths(x, style = "mics6", na.action = na.action, verbose = FALSE)
  setNames(d$sib.preg_related.death.date > 0, x$who)
}
mat_of <- function(x, na.action = "include") {
  d <- add_maternal_deaths(x, style = "mics6", na.action = na.action, verbose = FALSE)
  setNames(d$sib.maternal.death.date > 0, x$who)
}

# =====================================================================
# na.action is required for MICS, defaulted for DHS
# =====================================================================

test_that("add_maternal_deaths: na.action is required for the MICS styles", {
  expect_error(
    add_maternal_deaths(mics_sibs(), style = "mics6", verbose = FALSE),
    "`na.action` is required")
  expect_error(
    add_maternal_deaths(mics_sibs(), style = "mics4", verbose = FALSE),
    "`na.action` is required")
})

test_that("add_maternal_deaths: the error explains the choice", {
  err <- expect_error(
    add_maternal_deaths(mics_sibs(), style = "mics6", verbose = FALSE))
  expect_match(conditionMessage(err), "MM25")
  expect_match(conditionMessage(err), "include")
  expect_match(conditionMessage(err), "exclude")
})

test_that("add_maternal_deaths: DHS keeps its historical default", {
  # existing DHS call sites pass no na.action and must keep working unchanged
  expect_no_error(add_maternal_deaths(ex.sib, verbose = FALSE))

  a <- add_maternal_deaths(ex.sib, verbose = FALSE)
  b <- add_maternal_deaths(ex.sib, na.action = "include", verbose = FALSE)
  expect_equal(a$sib.preg_related.death.date, b$sib.preg_related.death.date)
})

test_that("add_maternal_deaths: DHS na.action=exclude drops the missing-band deaths", {
  inc <- add_maternal_deaths(ex.sib, na.action = "include", verbose = FALSE)
  exc <- add_maternal_deaths(ex.sib, na.action = "exclude", verbose = FALSE)

  n_inc <- sum(inc$sib.preg_related.death.date > 0, na.rm = TRUE)
  n_exc <- sum(exc$sib.preg_related.death.date > 0, na.rm = TRUE)
  expect_gte(n_inc, n_exc)
})

# =====================================================================
# MICS classification, and the three skip-pattern traps
# =====================================================================

test_that("is_preg_related_mics: any of the three binaries qualifies", {
  pr <- pr_of(mics_sibs())
  expect_true(pr[["pregnant"]])
  expect_true(pr[["childbirth"]])
  expect_true(pr[["postpartum_20d"]])
  # pregnancy-related has no day-count condition, so 55 days still counts
  expect_true(pr[["postpartum_55d"]])
  # and no cause exclusion, so an accident still counts
  expect_true(pr[["accident"]])
})

test_that("trap 1: a childbirth death is maternal despite NA on MM26/MM27", {
  # MM23 == 1 skips the cause questions entirely, so they are NA by design.
  # Using == rather than %in% would propagate that NA and drop the death.
  expect_true(mat_of(mics_sibs())[["childbirth"]])
})

test_that("trap 2: an under-12 female death is never maternal or pregnancy-related", {
  # MM21 routes her past MM22-MM25, so those are NA by design. Without the age
  # guard, na.action = 'include' would sweep her in.
  for (na in c("include", "exclude")) {
    expect_false(pr_of(mics_sibs(), na)[["under12"]])
    expect_false(mat_of(mics_sibs(), na)[["under12"]])
  }
})

test_that("trap 3: males get NA, not FALSE", {
  d <- add_maternal_deaths(mics_sibs(), style = "mics6",
                           na.action = "include", verbose = FALSE)
  i <- which(d$who == "male")
  expect_true(is.na(d$sib.preg_related.death.date[i]))
  expect_true(is.na(d$sib.maternal.death.date[i]))
})

test_that("maternal excludes deaths past 42 days and deaths from accidents", {
  mat <- mat_of(mics_sibs())
  expect_true(mat[["postpartum_20d"]])
  expect_false(mat[["postpartum_55d"]])
  expect_false(mat[["accident"]])
})

test_that("na.action decides only the missing-day-count case", {
  inc <- mat_of(mics_sibs(), "include")
  exc <- mat_of(mics_sibs(), "exclude")

  expect_true(inc[["postpartum_NAdays"]])
  expect_false(exc[["postpartum_NAdays"]])

  # every other classification is identical
  # which() drops the NA comparisons for male siblings
  differing <- names(inc)[which(inc != exc)]
  expect_equal(differing, "postpartum_NAdays")
})

test_that("na.action never moves the pregnancy-related column", {
  expect_equal(pr_of(mics_sibs(), "include"), pr_of(mics_sibs(), "exclude"))
})

# =====================================================================
# MICS4/5 cannot identify maternal deaths
# =====================================================================

test_that("style='mics4' yields NA maternal deaths when no accident item exists", {
  d <- mics_sibs() %>% select(-sib.died.accident, -sib.died.violence)
  out <- add_maternal_deaths(d, style = "mics4", na.action = "include",
                             verbose = FALSE)

  expect_true(all(is.na(out$sib.maternal.death.date)))
  # but pregnancy-related is still computed
  expect_gt(sum(out$sib.preg_related.death.date > 0, na.rm = TRUE), 0)
})

# =====================================================================
# The -1 sentinel
# =====================================================================

test_that("non-qualifying female deaths get -1, not NA, so exposure is kept", {
  d <- add_maternal_deaths(mics_sibs(), style = "mics6",
                           na.action = "include", verbose = FALSE)
  females <- d %>% filter(sib.sex == "f")
  expect_false(any(is.na(females$sib.preg_related.death.date)))
  expect_true(any(females$sib.preg_related.death.date == -1))
})

# =====================================================================
# preg.window: reproducing what published MICS tables actually count
# =====================================================================
# UNICEF's own tabulation syntax (MICS6 - 06 - TM.9.1&TM.9.2&TM.9.3...) counts
#   MM22 = 1 | MM23 = 1 | (MM24 = 1 & MM25 < 42)
# and never reads MM26/MM27. That is a pregnancy-related count on a 42-day
# window, and it is what the "Maternal Deaths" column of TM.9.3 reports.

test_that("preg.window defaults to 2months, so existing results do not move", {
  d1 <- add_maternal_deaths(mics_sibs(), style = "mics6", na.action = "include",
                            verbose = FALSE)
  d2 <- add_maternal_deaths(mics_sibs(), style = "mics6", na.action = "include",
                            preg.window = "2months", verbose = FALSE)
  expect_equal(d1$sib.preg_related.death.date, d2$sib.preg_related.death.date)
})

test_that("preg.window='42days' drops postpartum deaths past 42 days", {
  x <- mics_sibs()
  d <- add_maternal_deaths(x, style = "mics6", na.action = "include",
                           preg.window = "42days", verbose = FALSE)
  pr <- setNames(d$sib.preg_related.death.date > 0, x$who)

  expect_true(pr[["pregnant"]])
  expect_true(pr[["childbirth"]])
  expect_true(pr[["postpartum_20d"]])
  expect_false(pr[["postpartum_55d"]])
  # a missing day count fails the test, matching `MM25 < 42` in SPSS
  expect_false(pr[["postpartum_NAdays"]])
  # no cause exclusion: the accident row died while pregnant and still counts
  expect_true(pr[["accident"]])
})

test_that("preg.window never moves the maternal column", {
  x <- mics_sibs()
  a <- add_maternal_deaths(x, style = "mics6", na.action = "include",
                           preg.window = "2months", verbose = FALSE)
  b <- add_maternal_deaths(x, style = "mics6", na.action = "include",
                           preg.window = "42days", verbose = FALSE)
  expect_equal(a$sib.maternal.death.date, b$sib.maternal.death.date)
})

# =====================================================================
# the age-at-death guard must not drop don't-know ages
# =====================================================================
# MM19 = 98 ("don't know") is set to NA by the prep. The guard used to require
# a *known* age of 12 or over, which silently dropped sisters who had answered
# the maternity questions affirmatively -- proof in itself that they were asked.
# Cost 7 pregnancy-related deaths in Iraq 2018 and 5 in Zimbabwe 2019.

test_that("a sister with a don't-know age at death is still classified", {
  x <- tibble::tibble(
    who                       = c("dk_age_pregnant", "known_age_pregnant", "under12"),
    sib.sex                   = c("f", "f", "f"),
    sib.death.age             = c(NA_integer_, 25L, 8L),
    sib.death.date            = c(1300, 1310, 1320),
    sib.preg.at.death         = c(1, 1, NA),
    sib.died.childbirth       = c(2, 2, NA),
    sib.died.postpartum       = c(2, 2, NA),
    sib.days.postpartum.death = c(NA, NA, NA),
    sib.died.violence         = c(2, 2, NA),
    sib.died.accident         = c(2, 2, NA))

  d <- add_maternal_deaths(x, style = "mics6", na.action = "include",
                           verbose = FALSE)
  pr <- setNames(d$sib.preg_related.death.date > 0, x$who)

  expect_true(pr[["dk_age_pregnant"]])
  expect_true(pr[["known_age_pregnant"]])
  # the guard still does its job: under-12 sisters were never asked
  expect_false(pr[["under12"]])
})

test_that("preg.window='42days' errors clearly when there is no day count", {
  # MICS4/MICS5 ask the three binaries but never MM25, so there is nothing to
  # cut on. Unguarded this collapses to a zero-length vector rather than erroring.
  x <- mics_sibs()
  x$sib.days.postpartum.death <- NULL

  expect_error(
    add_maternal_deaths(x, style = "mics4", na.action = "include",
                        preg.window = "42days", verbose = FALSE),
    "no sib.days.postpartum.death column")

  # a realistic MICS4/5 frame has no cause items either; the two-month default
  # works there, and is what MICS4/5 reports use
  x4 <- x %>% select(-sib.died.accident, -sib.died.violence)
  out <- add_maternal_deaths(x4, style = "mics4", na.action = "include",
                             verbose = FALSE)
  expect_gt(sum(out$sib.preg_related.death.date > 0, na.rm = TRUE), 0)
  expect_true(all(is.na(out$sib.maternal.death.date)))
})

# =====================================================================
# The DHS maternal rule, and the 2016 PRMR recode option
# =====================================================================

dhs_sibs <- function() {
  tibble::tribble(
    ~who,               ~sib.sex, ~sib.death.date, ~sib.died.pregnant, ~sib.died.accident,
    "pregnant_ok",           "f",           1200,                   2,                  0,
    "pregnant_accident",     "f",           1210,                   2,                  2,
    "delivery_no_mm16",      "f",           1220,                   3,                 NA,
    "delivery_accident",     "f",           1230,                   3,                  1,
    "sixweeks_ok",           "f",           1240,                   5,                  0,
    "sixweeks_violence",     "f",           1250,                   5,                  1,
    "twomonths",             "f",           1260,                   6,                  0,
    "not_related",           "f",           1270,                   1,                  0)
}

test_that("maternal follows the DHS rule: mm9 2-5, excluding mm16 1 or 2", {
  d <- add_maternal_deaths(dhs_sibs(), verbose = FALSE)
  m <- setNames(d$sib.maternal.death.date > 0, dhs_sibs()$who)

  expect_true(m[["pregnant_ok"]])
  expect_true(m[["sixweeks_ok"]])
  # mm16 is not asked for a death during delivery, so a missing value must pass
  expect_true(m[["delivery_no_mm16"]])

  expect_false(m[["pregnant_accident"]])
  expect_false(m[["sixweeks_violence"]])
  # code 6 is outside the 42-day window
  expect_false(m[["twomonths"]])
  expect_false(m[["not_related"]])
})

test_that("a delivery death reported as an accident is now excluded", {
  # the package used to take mm9 = 3 unconditionally. South Africa 2016 has 3
  # such deaths, the only survey of 43 that breaks the skip pattern.
  d <- add_maternal_deaths(dhs_sibs(), verbose = FALSE)
  m <- setNames(d$sib.maternal.death.date > 0, dhs_sibs()$who)
  expect_false(m[["delivery_accident"]])
})

test_that("mm12 no longer affects the maternal column either", {
  a <- add_maternal_deaths(dhs_sibs(), verbose = FALSE)
  b <- dhs_sibs(); b$sib.time.delivery.death <- 999
  b <- add_maternal_deaths(b, verbose = FALSE)
  expect_equal(a$sib.maternal.death.date, b$sib.maternal.death.date)
})

test_that("prmr.accident.recode defaults off and matches the reference", {
  d <- add_maternal_deaths(dhs_sibs(), verbose = FALSE)
  pr <- setNames(d$sib.preg_related.death.date > 0, dhs_sibs()$who)
  # the shipped reference applies no cause condition to pregnancy-related
  expect_true(pr[["pregnant_accident"]])
  expect_true(pr[["delivery_accident"]])
  expect_true(pr[["twomonths"]])
})

test_that("prmr.accident.recode drops accident deaths during pregnancy only", {
  d <- add_maternal_deaths(dhs_sibs(), prmr.accident.recode = TRUE,
                           verbose = FALSE)
  pr <- setNames(d$sib.preg_related.death.date > 0, dhs_sibs()$who)

  # mm9 = 2 with mm16 in (1,2) is recoded away
  expect_false(pr[["pregnant_accident"]])
  # every other code is untouched -- the DHS rule names mm9 = 2 specifically
  expect_true(pr[["delivery_accident"]])
  expect_true(pr[["sixweeks_violence"]])
  expect_true(pr[["pregnant_ok"]])
})
