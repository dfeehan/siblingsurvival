##' is each sibling's death pregnancy-related, by DHS coding?
##'
##' The DHS records this in one coded item, `sib.died.pregnant` (`mm9`):
##'
##' | `mm9` | meaning | pregnancy-related | maternal |
##' |---|---|---|---|
##' | 2 | died while pregnant | yes | yes |
##' | 3 | died during delivery | yes | yes |
##' | 4 | since delivery -- never assigned in practice | yes | yes |
##' | 5 | within six weeks of a delivery | yes | yes |
##' | 6 | between six weeks and two months of a delivery | **yes** | no |
##'
##' Code 6 is what makes this the **two-month** quantity rather than a 42-day
##' one, and it is exactly the line The DHS Program draws between this and
##' [is_maternal_dhs], which stops at code 5.
##'
##' No other condition is applied. In particular the time-since-delivery band
##' `mm12` is *not* used: the reference implementation
##' (`DHS-Indicators-Stata`, `Chap16_AM/AM_rates.do:725`) counts
##' `mm9 >= 2 & mm9 <= 6` and nothing else, and its header states plainly that
##' "mm12 is not needed" -- it is dropped before the roster is reshaped.
##'
##' @section Changed in this version:
##'
##' This function previously required `mm9` to be 2, 3, 4 or 5 **and**
##' `mm12` to fall in the band `100`--`141` (0--41 days) or be `997`/`998`.
##' Both conditions were wrong:
##'
##' * excluding code 6 dropped postpartum deaths that the function's own
##'   documentation described as in scope, and
##' * the `mm12` band imposed a 42-day cut on a quantity defined over two
##'   months, and applied a *postpartum* timing test even to deaths that
##'   occurred during pregnancy or delivery.
##'
##' **This changes DHS results, in some surveys substantially.** Which code a
##' survey uses for postpartum deaths is a property of its questionnaire, so
##' the old behaviour lost anywhere from none to about 48% of pregnancy-related
##' deaths depending on the survey. On Rwanda 2010 it gave 51.2 deaths against a
##' published 91; it now gives 90.7. See the DHS validation plan in the package
##' repository.
##'
##' @section The 2016 PRMR redefinition:
##'
##' `AM_rates.do:307-320` carries, **inside a comment block and therefore never
##' executed**, a rule introduced as "Important for redefinition of Pregnancy
##' Related Mortality Ratio (PRMR) in surveys from 2016 onwards":
##'
##'     If mm9=2, and mm16=1 or 2, recode mm9 to 1
##'
##' That is: a death *during pregnancy* that is reported as due to violence or
##' an accident stops counting as pregnancy-related. The DHS Program documents
##' the rule (see `blog.dhsprogram.com/mmr-prmr/`) but the code it ships does
##' not apply it, so published figures produced with that code do not reflect
##' it either.
##'
##' `prmr.accident.recode = TRUE` applies it. The default is `FALSE`, which is
##' what the shipped reference code does and therefore what reproduces published
##' tables. It can only bite on the five surveys that carry `mm16` at all.
##'
##' @param sib_df the prepped sibling dataset
##' @param na.action retained for symmetry with [is_maternal_dhs] and ignored
##'        here. It used to decide how a missing `mm12` was treated; `mm12` is
##'        no longer consulted, so it has no effect on this column
##' @param prmr.accident.recode apply the documented-but-unexecuted 2016 PRMR
##'        rule, which drops a death during pregnancy (`mm9 = 2`) that is
##'        reported as violence or an accident. Default `FALSE`, matching the
##'        reference implementation. See Details
##' @return a logical vector, one entry per row of `sib_df`
##' @md
##'
is_preg_related_dhs <- function(sib_df, na.action = NULL,
                                prmr.accident.recode = FALSE) {

  ## AM_rates.do:725 -- `prdeaths_in = 1 if deaths_in == 1 & mm9>=2 & mm9<=6`
  ## `%in%` gives FALSE for NA, which is what is wanted: an unknown mm9 is not
  ## a pregnancy-related death
  res <- sib_df$sib.died.pregnant %in% c(2, 3, 4, 5, 6)

  if (prmr.accident.recode) {
    ## the commented-out AM_rates.do:314; applies to mm9 = 2 only
    res <- res & !(sib_df$sib.died.pregnant %in% 2 &
                     sib_df$sib.died.accident %in% c(1, 2))
  }

  res
}


##' is each sibling's death maternal, by DHS coding?
##'
##' As [is_preg_related_dhs], but stopping at `mm9 = 5` --- the 42-day window ---
##' and excluding deaths reported as due to violence or an accident (`mm16`).
##' Only computable when `sib.died.accident` is present, which is DHS phase 7
##' and later, and in practice only 5 of the 43 surveys with a sibling roster
##' carry it.
##'
##' The rule is the reference implementation's, verbatim
##' (`DHS-Indicators-Stata`, `Chap16_AM/AM_rates.do:728`):
##'
##'     mm9 >= 2 & mm9 <= 5 & mm16 != 1 & mm16 != 2
##'
##' Two details that look like edge cases but are not:
##'
##' * **A missing `mm16` counts as "not an accident".** `mm16` is *not asked*
##'   when the death occurred during delivery, so for `mm9 = 3` it is missing by
##'   design --- in The Gambia 2019-20 all 40 such in-window deaths have `mm16`
##'   missing and none of the `mm9 = 2` or `mm9 = 5` deaths do. Requiring
##'   `mm16 == 0` would silently drop every delivery death.
##' * **`mm9 = 4` is included** even though it is never assigned in practice.
##'   That matches the reference, which tests a range rather than a set.
##'
##' @section Changed in this version:
##'
##' This function previously applied the accident exclusion to codes 2 and 5
##' only, took code 3 unconditionally, and additionally required `mm12` to fall
##' in the band `100`--`141`. Given the skip pattern above, the first two are
##' *equivalent* to the reference wherever a survey respects it --- The Gambia
##' 2019-20 reproduces its published Table 14.3 exactly either way.
##'
##' They part company where a survey does not. **South Africa 2016 has 3 deaths
##' coded `mm9 = 3` with `mm16` reported as violence or an accident**, which the
##' old rule counted as maternal and the reference does not. The `mm12`
##' condition is dropped for the same reason it was dropped from
##' [is_preg_related_dhs]: the reference does not use it.
##'
##' @param sib_df the prepped sibling dataset
##' @param na.action retained for symmetry and ignored. It used to decide how a
##'        missing `mm12` was treated; `mm12` is no longer consulted
##' @return a logical vector, one entry per row of `sib_df`
##' @md
##'
is_maternal_dhs <- function(sib_df, na.action = NULL) {

  ## AM_rates.do:728. `mm16 != 1 & mm16 != 2` is TRUE for a missing mm16 in
  ## Stata, and must stay TRUE here -- see the note about delivery deaths above
  not.accident <- ! (sib_df$sib.died.accident %in% c(1, 2))

  (sib_df$sib.died.pregnant %in% c(2, 3, 4, 5)) & not.accident
}


##' is each sibling's death pregnancy-related, by MICS coding?
##'
##' MICS asks three separate binaries where the DHS uses one coded item:
##' `MM22` pregnant when she died, `MM23` died during childbirth, and `MM24`
##' died within two months of the end of a pregnancy (`MM10`, `MM11`, `MM12` in
##' MICS4/5). Any of the three makes the death pregnancy-related, with no cause
##' exclusion.
##'
##' `preg.window` chooses how wide the postpartum window is:
##'
##' * `"2months"` (the default) takes `MM24 = 1` at face value, so the window is
##'   however long the respondent understood "two months" to be. This is the
##'   widest reading and the one this package has always used for MICS.
##' * `"42days"` additionally requires `MM25 < 42`. This is the WHO definition of
##'   a pregnancy-related death, it is what UNICEF's own tabulation syntax
##'   computes, and it is therefore what published MICS tables report. It is also
##'   consistent with [is_preg_related_dhs], which already applies a 42-day cut
##'   through the `mm12` band `100`--`141`.
##'
##' Only `"42days"` reproduces published MICS figures. On Iraq 2018 it gives 64.4
##' pregnancy-related deaths against a published 64, and on Madagascar 2018 136.6
##' against a published 137; `"2months"` gives 67.7 and 140.4.
##'
##' See the "Working with MICS sibling history data" vignette.
##'
##' @param sib_df the prepped sibling dataset
##' @param preg.window width of the postpartum window: `"2months"` (default) or
##'        `"42days"`. See Details
##' @return a logical vector, one entry per row of `sib_df`
##' @md
##'
is_preg_related_mics <- function(sib_df, preg.window = c("2months", "42days")) {

  preg.window <- match.arg(preg.window)

  postpartum <- sib_df$sib.died.postpartum %in% 1

  if (preg.window == "42days") {

    ## MICS4/MICS5 ask the three binaries but no day count, so there is nothing
    ## to cut on. Left unguarded this silently collapses to a zero-length vector
    ## rather than erroring.
    if (! 'sib.days.postpartum.death' %in% names(sib_df)) {
      stop(paste0(
        "preg.window = '42days' needs the number of days after the end of a ",
        "pregnancy (MM25), and there is no sib.days.postpartum.death column.\n",
        "MICS4 and MICS5 do not ask it, so only preg.window = '2months' is ",
        "available for them. Published MICS4/5 tables use the two-month window ",
        "too, so this is the right choice there, not a compromise."))
    }

    ## the cut UNICEF's own tabulation syntax applies: MM25 < 42, with a missing
    ## or don't-know day count failing the test
    postpartum <- postpartum &
      (!is.na(sib_df$sib.days.postpartum.death)) &
      (sib_df$sib.days.postpartum.death < 42)
  }

  res <- (sib_df$sib.preg.at.death %in% 1) |
    (sib_df$sib.died.childbirth %in% 1) |
    postpartum

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

  ## Same hazard as in is_preg_related_mics: without the day count this
  ## collapses to a zero-length vector rather than erroring. Real MICS4/5 data
  ## has neither MM25 nor the cause items, so add_maternal_deaths() skips this
  ## path entirely -- but say so plainly rather than relying on that.
  if (! 'sib.days.postpartum.death' %in% names(sib_df)) {
    stop(paste0(
      "a maternal death needs the number of days after the end of a pregnancy ",
      "(MM25) to apply the 42-day window, and there is no ",
      "sib.days.postpartum.death column.\n",
      "MICS4 and MICS5 ask neither MM25 nor the cause-of-death items, so they ",
      "support pregnancy-related mortality only."))
  }

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
##' not missing data.
##'
##' The age test is deliberately "not *known* to have died under 12" rather than
##' "known to have died at 12 or over". A sister whose age at death was reported
##' as don't-know (`MM19 = 98`) is set to `NA` by the prep, and the stricter test
##' silently dropped her even when she had answered `MM22`--`MM24` affirmatively
##' -- which is itself proof she was asked. That cost 7 pregnancy-related deaths
##' in Iraq 2018 and 5 in Zimbabwe 2019.
##'
##' @param sib_df the prepped sibling dataset
##' @return a logical vector, one entry per row of `sib_df`
##'
mics_asked_maternity_questions <- function(sib_df) {

  female <- sib_df$sib.sex %in% 'f'

  ## MM21 checks whether the sister died before age 12 and skips ahead if so
  not.under.12 <- is.na(sib_df$sib.death.age) | (sib_df$sib.death.age >= 12)

  female & not.under.12
}
