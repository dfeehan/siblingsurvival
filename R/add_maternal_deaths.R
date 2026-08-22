## add_maternal_deaths(): classify sibling deaths as pregnancy-related or
## maternal. Despite once living in prep_dhs_sib_histories.R, this is NOT
## DHS-specific -- it dispatches on `style` across DHS, MICS6/7 and MICS4/5.
## The per-questionnaire rules live in maternal_classification.R.

##' add pregnancy-related and maternal death info to a sibling dataset
##'
##' @param sib_df the prepped sibling dataset (probably from
##'        [siblingsurvival::prep_dhs_sib_histories] or
##'        [siblingsurvival::prep_mics_sib_histories])
##' @param style which questionnaire the coding follows: `"dhs"` (the default),
##'        `"mics6"` for MICS6/MICS7, or `"mics4"` for MICS4/MICS5
##' @param na.action how to treat a death that falls in the right window but
##'        whose timing detail is missing -- `"include"` counts it, `"exclude"`
##'        does not. **Required for the MICS styles**; see Details
##' @param preg.window width of the postpartum window used for the
##'        *pregnancy-related* column under the MICS styles: `"2months"` (the
##'        default, and what this package has always done) or `"42days"`, which
##'        is what published MICS tables report. Ignored for `style = "dhs"`,
##'        which already applies a 42-day cut. See Details
##' @param keep_missing not currently used
##' @param verbose report detailed summaries?
##' @return `sib_df` with columns `sib.preg_related.death.date` and
##'         `sib.maternal.death.date` added
##' @examples
##'   # TODO - write example code
##' @section Details:
##'
##' Two quantities are computed, and they are not the same thing:
##'
##' * **pregnancy-related** -- died while pregnant, during childbirth, or within
##'   the postpartum window, *whatever the cause*
##' * **maternal** -- as above but within 42 days, and excluding deaths due to
##'   violence or an accident
##'
##' `sib.maternal.death.date` is only computable when the data identify
##' accidental deaths, which means DHS phase 7 and later, or MICS6 and later.
##' Otherwise it is `NA` and only the pregnancy-related column is usable -- the
##' same limitation applies to DHS phases 2--6 and to MICS4/MICS5.
##'
##' Siblings who did not die of the relevant cause get a death date of `-1`
##' rather than `NA`, so that they still contribute exposure. Both columns are
##' set to `NA` for male siblings.
##'
##' **Note for MICS users:** the tables published in MICS survey reports under
##' the heading "Maternal mortality", with a column labelled "Maternal Deaths",
##' do *not* report maternal deaths as defined above. UNICEF's own tabulation
##' syntax counts `MM22 = 1 | MM23 = 1 | (MM24 = 1 & MM25 < 42)` and never reads
##' the violence (`MM26`) or accident (`MM27`) items at all, despite the
##' footnote in the reports saying those causes are excluded. So the published
##' column is a **pregnancy-related** count on a 42-day window. To reproduce it,
##' use `sib.preg_related.death.date` with `preg.window = "42days"`. See the
##' vignette "Working with MICS sibling history data".
##'
##' ## Choosing `na.action`
##'
##' `na.action` has **no default for the MICS styles**, because it is a
##' substantive choice about the estimand rather than a coding detail, and it
##' should be made deliberately and reported.
##'
##' In MICS, the number of days after the end of a pregnancy (`MM25`) is asked
##' only of sisters who died within two months (`MM24 = 1`). When that day count
##' is missing, `na.action` decides whether she falls inside the 42-day maternal
##' window. In three MICS6 surveys examined this affected 5 of 38 such deaths in
##' Iraq 2018 and none at all in Zimbabwe 2019 or Madagascar 2018, so the choice
##' is usually immaterial -- but not always, and it only ever moves the maternal
##' column, never the pregnancy-related one.
##'
##' For `style = "dhs"`, `na.action` defaults to `"include"`, which is what this
##' package has always done: a missing `sib.time.delivery.death` was treated as
##' falling in the window. That default is kept so existing results do not move.
##'
##' @export
##' @md
add_maternal_deaths <- function(sib_df,
                                style = c("dhs", "mics6", "mics4"),
                                na.action = NULL,
                                preg.window = c("2months", "42days"),
                                keep_missing = FALSE,
                                verbose = TRUE) {

  style <- match.arg(style)
  preg.window <- match.arg(preg.window)

  if (is.null(na.action)) {

    if (style == "dhs") {
      ## the behaviour this package has always had; kept so that existing
      ## results do not silently change
      na.action <- "include"
    } else {
      stop(glue::glue(
        "`na.action` is required for style = '{style}'.\n",
        "In MICS the number of days after the end of a pregnancy (MM25) is ",
        "asked only of sisters who died within two months (MM24 = 1). When it ",
        "is missing, you have to say whether she counts as inside the 42-day ",
        "maternal window:\n",
        "  na.action = 'include'  -- count her as maternal\n",
        "  na.action = 'exclude'  -- require an observed day count\n",
        "This is a choice about the estimand, so it has no default. It moves ",
        "only sib.maternal.death.date, never sib.preg_related.death.date.\n"))
    }
  }

  na.action <- match.arg(na.action, c("include", "exclude"))

  #########################
  # pregnancy-related deaths
  #########################
  ## available for every DHS phase and every MICS round with a sibling roster
  if (style == "dhs") {
    is.pr <- is_preg_related_dhs(sib_df, na.action)
  } else {
    is.pr <- is_preg_related_mics(sib_df, preg.window = preg.window)
  }

  ## siblings who did not die a pregnancy-related death get -1 rather than NA,
  ## so that we keep the exposure they contribute
  sib_df$sib.preg_related.death.date <- ifelse(is.pr, sib_df$sib.death.date, -1)
  sib_df$sib.preg_related.death.date[is.na(sib_df$sib.preg_related.death.date)] <- -1

  #########################
  # maternal deaths
  #########################
  ## only possible where accidental deaths are identified: DHS phase 7 and
  ## later, MICS6 and later
  can.do.maternal <- 'sib.died.accident' %in% names(sib_df)

  if (can.do.maternal) {

    if (style == "dhs") {
      is.mat <- is_maternal_dhs(sib_df, na.action)
    } else {
      is.mat <- is_maternal_mics(sib_df, na.action)
    }

    sib_df$sib.maternal.death.date <- ifelse(is.mat, sib_df$sib.death.date, -1)
    sib_df$sib.maternal.death.date[is.na(sib_df$sib.maternal.death.date)] <- -1

  } else {

    if (verbose) {
      cat("\n...sib.died.accident column not found; only pregnancy-related deaths can be identified here\n")
    }
    sib_df$sib.maternal.death.date <- NA
  }

  #########################
  # males
  #########################
  sib_df <- sib_df %>%
    mutate(sib.preg_related.death.date = ifelse(sib.sex == 'm', NA, sib.preg_related.death.date)) %>%
    mutate(sib.maternal.death.date     = ifelse(sib.sex == 'm', NA, sib.maternal.death.date))

  if (verbose) {
    n.pr  <- sum(sib_df$sib.preg_related.death.date > 0, na.rm = TRUE)
    n.mat <- sum(sib_df$sib.maternal.death.date > 0, na.rm = TRUE)
    cat(paste0("Identified ", n.pr, " pregnancy-related death(s)",
               if (can.do.maternal) paste0(" and ", n.mat, " maternal death(s)") else "",
               " (style = '", style, "', na.action = '", na.action,
               if (style != "dhs") paste0("', preg.window = '", preg.window) else "",
               "').\n"))
  }

  return(sib_df)
}
