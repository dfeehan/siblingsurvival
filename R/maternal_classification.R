##' is each sibling's death pregnancy-related, by DHS coding?
##'
##' The DHS records one coded item, `sib.died.pregnant` (`mm9`), plus a
##' time-since-delivery band, `sib.time.delivery.death` (`mm12`).
##'
##' @param sib_df the prepped sibling dataset
##' @param na.action how to treat a missing `sib.time.delivery.death` when the
##'        sibling died after a delivery: `"include"` counts her, `"exclude"`
##'        does not. See [siblingsurvival::add_maternal_deaths]
##' @return a logical vector, one entry per row of `sib_df`
##'
is_preg_related_dhs <- function(sib_df, na.action) {

  ## the time-since-delivery band is only informative for deaths *after* a
  ## delivery; 997 and 998 are the DHS's own "don't know"/"inconsistent" codes,
  ## which the published DHS calculation counts
  within.window <- (sib_df$sib.time.delivery.death >= 100 &
                      sib_df$sib.time.delivery.death <= 141) |
    sib_df$sib.time.delivery.death %in% c(997, 998)

  if (na.action == "include") {
    within.window <- within.window | is.na(sib_df$sib.time.delivery.death)
  }

  ## 2 = died while pregnant, 3 = died during childbirth,
  ## 4 = died since a delivery, 5 = died within six weeks of a delivery
  died.pregnant <- sib_df$sib.died.pregnant %in% c(2, 3, 4, 5)

  res <- died.pregnant & within.window
  ifelse(is.na(res), FALSE, res)
}


##' is each sibling's death maternal, by DHS coding?
##'
##' As [is_preg_related_dhs], but additionally excluding deaths reported as due
##' to violence or an accident. Only computable when `sib.died.accident` is
##' present, which is DHS phase 7 and later.
##'
##' @param sib_df the prepped sibling dataset
##' @param na.action see [siblingsurvival::add_maternal_deaths]
##' @return a logical vector, one entry per row of `sib_df`
##'
is_maternal_dhs <- function(sib_df, na.action) {

  within.window <- (sib_df$sib.time.delivery.death >= 100 &
                      sib_df$sib.time.delivery.death <= 141) |
    sib_df$sib.time.delivery.death %in% c(997, 998)

  if (na.action == "include") {
    within.window <- within.window | is.na(sib_df$sib.time.delivery.death)
  }

  not.accident <- sib_df$sib.died.accident %in% 0

  ## NB the accident exclusion applies to codes 2 and 5 only, matching the
  ## behaviour this package has always had
  died.pregnant <- (sib_df$sib.died.pregnant %in% 3) |
    (sib_df$sib.died.pregnant %in% 2 & not.accident) |
    (sib_df$sib.died.pregnant %in% 5 & not.accident) |
    (sib_df$sib.died.pregnant %in% 4)

  res <- died.pregnant & within.window
  ifelse(is.na(res), FALSE, res)
}


##' is each sibling's death pregnancy-related, by MICS coding?
##'
##' MICS asks three separate binaries where the DHS uses one coded item:
##' `MM22` pregnant when she died, `MM23` died during childbirth, and `MM24`
##' died within two months of the end of a pregnancy (`MM10`, `MM11`, `MM12` in
##' MICS4/5). Any of the three makes the death pregnancy-related; there is no
##' day-count condition and no cause exclusion.
##'
##' This is the quantity published MICS tables actually report -- see the
##' "Working with MICS sibling history data" vignette.
##'
##' @param sib_df the prepped sibling dataset
##' @return a logical vector, one entry per row of `sib_df`
##'
is_preg_related_mics <- function(sib_df) {

  res <- (sib_df$sib.preg.at.death %in% 1) |
    (sib_df$sib.died.childbirth %in% 1) |
    (sib_df$sib.died.postpartum %in% 1)

  res & mics_asked_maternity_questions(sib_df)
}


##' is each sibling's death maternal, by MICS coding?
##'
##' As [is_preg_related_mics], but restricted to deaths within 42 days of the
##' end of a pregnancy and excluding deaths due to violence (`MM26`) or an
##' accident (`MM27`). Only computable for MICS6 and MICS7; MICS4 and MICS5 ask
##' no cause-of-death questions.
##'
##' @param sib_df the prepped sibling dataset
##' @param na.action how to treat a sibling who died within two months of the
##'        end of a pregnancy (`MM24 = 1`) but whose day count (`MM25`) is
##'        missing. See [siblingsurvival::add_maternal_deaths]
##' @return a logical vector, one entry per row of `sib_df`
##'
is_maternal_mics <- function(sib_df, na.action) {

  ## MM25 is asked only when MM24 == 1, so the 42-day cut bites only on that
  ## branch. Deaths while pregnant (MM22) or during childbirth (MM23) are
  ## unconditionally inside the window.
  within42 <- sib_df$sib.days.postpartum.death <= 42

  if (na.action == "include") {
    within42 <- within42 | is.na(sib_df$sib.days.postpartum.death)
  }

  within42 <- ifelse(is.na(within42), FALSE, within42)

  in.window <- (sib_df$sib.preg.at.death %in% 1) |
    (sib_df$sib.died.childbirth %in% 1) |
    (sib_df$sib.died.postpartum %in% 1 & within42)

  ## NB `%in% 1` rather than `== 1`: MM23 = 1 skips MM26 and MM27 entirely, so
  ## they are NA by design for childbirth deaths, and `==` would propagate that
  ## NA and drop a death that is unconditionally maternal
  not.accident <- !((sib_df$sib.died.violence %in% 1) |
                      (sib_df$sib.died.accident %in% 1))

  in.window & not.accident & mics_asked_maternity_questions(sib_df)
}


##' were the MICS maternity questions asked of this sibling?
##'
##' MICS routes male siblings, and sisters who died before age 12, past the
##' maternity items -- so `MM22` through `MM25` are `NA` *by design* for them,
##' not missing data. Without this guard, and with `na.action = "include"`,
##' every under-12 female death would be classified as maternal.
##'
##' @param sib_df the prepped sibling dataset
##' @return a logical vector, one entry per row of `sib_df`
##'
mics_asked_maternity_questions <- function(sib_df) {

  female <- sib_df$sib.sex %in% 'f'

  ## MM21 checks whether the sister died before age 12 and skips ahead if so
  aged12 <- (!is.na(sib_df$sib.death.age)) & (sib_df$sib.death.age >= 12)

  female & aged12
}
