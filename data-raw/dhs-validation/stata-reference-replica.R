## A literal R transcription of The DHS Program's own adult/maternal mortality
## tabulation code:
##   DHSProgram/DHS-Indicators-Stata, Chap16_AM/AM_rates.do
##   ("Program: AM_RATES.do", Thomas Pullum, last modified 2021-03-22)
##
## Its header states: "The program agrees exactly with DHS procedures, except
## for confidence intervals."
##
## This is NOT part of the package and is not what the package does. It exists so
## that published DHS tables can be reproduced independently of the package, and
## so that any gap between the two can be attributed to a specific convention
## rather than guessed at. Same role spss-syntax-replica.R plays for MICS.
##
## Usage:
##   source("data-raw/dhs-validation/stata-reference-replica.R")
##   r <- dhs_am("<path>/RWIR61FL.DTA", mr.file = "<path>/RWMR61FL.DTA")
##   report("RW 2010", r)
##
## `mr.file` is optional. Without it, everything for women is still produced --
## which is where the pregnancy-related and maternal estimands live -- and only
## the age-standardised male summary and male 35q15 are skipped, since those need
## the age distribution of men.
##
## Conventions this encodes, with their line numbers in AM_rates.do:
##   * window [v008 - 84, v008 - 1]; the interview month is excluded  (:230-240)
##   * siblings with sex missing (mm1 > 2) are dropped                (:305)
##   * siblings with unknown survival (mm2 > 1) are dropped           (:328)
##   * a decedent's exposure ends AT the month of death, inclusive    (:711)
##     -- note MICS ends it the month *before* death
##   * pregnancy-related = mm9 in 2..6, with NO cause exclusion       (:725)
##   * maternal          = mm9 in 2..5 & mm16 not in (1,2)            (:728)
##   * mm12 is not used at all; it is dropped before the reshape      (:151, :298)
##   * women's age distribution is weighted by v005/1e6 * awfactt/100 (:551)
##   * men's age distribution comes from the MR file, mv005/1e6       (:522)
##   * 35q15 uses 5*mx/(1 + 2.4*mx), i.e. nax = 2.6                   (:1085)
##
## Deliberate simplification: DHS fits stacked Poisson models to get the rates,
## but with a saturated age term and a log(exposure) offset the point estimates
## are exactly deaths/exposure. AM_rates.do:844 says so itself -- "To match with
## the reports, we also produce the numerators and denominators of these rates,
## which can also be used to calculate the rates. The model-based calculation is
## included largely for constructing confidence intervals." So plain ratios are
## used here. Confidence intervals are out of scope.

suppressMessages({ library(haven); library(dplyr) })

## Stata treats system-missing as larger than any number, and that matters here:
## `replace first = . if first > mm8` must NOT fire for a living sibling, whose
## mm8 is missing. These helpers reproduce that comparison semantics.
gt_ <- function(a, b) { r <- a > b; r[is.na(b) & !is.na(a)] <- FALSE; r[is.na(a)] <- TRUE; r & !is.na(r) }
le_ <- function(a, b) { r <- a <= b; r[is.na(a)] <- FALSE; r[is.na(b) & !is.na(a)] <- TRUE; r & !is.na(r) }
ge_ <- function(a, b) { r <- a >= b; r[is.na(a)] <- TRUE;  r[is.na(b) & !is.na(a)] <- FALSE; r & !is.na(r) }
lt_ <- function(a, b) { r <- a < b;  r[is.na(a)] <- FALSE; r[is.na(b) & !is.na(a)] <- TRUE; r & !is.na(r) }

##' Read an IR file and reshape the sibling roster to one row per sibling.
dhs_sib_long <- function(ir.file, encoding = NULL) {

  hdr <- read_dta(ir.file, n_max = 0, encoding = encoding)
  nm  <- names(hdr)
  low <- tolower(nm)

  ## AM_rates.do:267 keeps v000 v001 v002 v003 v005 v008 v010 v013 v021-v025 mm* awfact*
  ego.want <- c("v001","v002","v003","v005","v008","v013","v021","v023","v024","v025","awfactt")
  ego.cols <- nm[low %in% ego.want]

  ## the roster items the program actually uses; mm12 is deliberately absent
  items <- c("mm1","mm2","mm3","mm4","mm6","mm7","mm8","mm9","mm16")
  sib.cols <- nm[grepl(paste0("^(", paste(items, collapse = "|"), ")_[0-9]+$"), low)]

  d <- read_dta(ir.file, col_select = all_of(c(ego.cols, sib.cols)), encoding = encoding)
  names(d) <- tolower(names(d))
  sib.cols <- tolower(sib.cols)

  ## AM_rates.do:285-295 -- if mm16 is absent (pre-2016), give it a missing value
  has.mm16 <- any(grepl("^mm16_", sib.cols))

  idx <- sort(unique(as.integer(sub("^.*_", "", sib.cols))))
  long <- lapply(idx, function(i) {
    get1 <- function(v) {
      cn <- paste0(v, "_", formatC(i, width = 2, flag = "0"))
      if (!cn %in% names(d)) cn <- paste0(v, "_", i)
      if (cn %in% names(d)) as.numeric(d[[cn]]) else rep(NA_real_, nrow(d))
    }
    out <- d[, intersect(tolower(ego.want), names(d)), drop = FALSE]
    out$mmidx <- i
    for (v in items) out[[v]] <- get1(v)
    out
  })
  long <- bind_rows(long)

  ## a respondent with no siblings reported contributes only missing rows
  long <- long[!is.na(long$mm1) | !is.na(long$mm2), ]

  attr(long, "has.mm16") <- has.mm16
  long
}

##' Adult, pregnancy-related and maternal mortality, the DHS way.
##'
##' @param lw,uw window bounds in years relative to interview; the DHS default
##'        is lw = -6, uw = 0, i.e. the seven years before the survey
dhs_am <- function(ir.file, mr.file = NULL, lw = -6, uw = 0,
                   encoding = NULL, nax_coef = 2.4) {

  d <- dhs_sib_long(ir.file, encoding = encoding)
  has.mm16 <- attr(d, "has.mm16")

  ## --- setup_adult_mm_vars ------------------------------------------------
  d <- d[!is.na(d$mm1) & d$mm1 <= 2, ]        # :305  drop if mm1 > 2
  d <- d[!is.na(d$mm2) & d$mm2 <= 1, ]        # :328  drop if mm2 > 1
  d$sex <- d$mm1

  ## --- start_month_end_month ----------------------------------------------
  doi <- d$v008
  start_month <- doi + 12 * lw - 12           # :230
  end_month   <- pmin(doi + 12 * uw - 1, doi) # :231, :240

  ## --- get_exposure_and_deaths --------------------------------------------
  ## one row per sibling per five-year age interval 1..7 (ages 15-19 .. 45-49)
  per.age <- lapply(1:7, function(i) {

    first <- d$mm4 + (i + 2) * 60             # :693
    last  <- first + 59                       # :694

    ## truncate at the end of the window
    sel <- le_(first, end_month) & ge_(last, end_month)
    last[sel] <- end_month[sel]
    drop <- gt_(first, end_month)
    first[drop] <- NA; last[drop] <- NA

    ## a death closes the interval it falls in, and voids all later ones
    sel <- le_(first, d$mm8) & ge_(last, d$mm8) & !is.na(d$mm8)
    last[sel] <- d$mm8[sel]
    drop <- gt_(first, d$mm8)
    first[drop] <- NA; last[drop] <- NA

    died <- as.numeric(le_(first, d$mm8) & ge_(last, d$mm8) &
                         le_(start_month, d$mm8) & ge_(end_month, d$mm8))

    ## :725 -- no cause condition, and mm9 = 6 is included
    prdied <- as.numeric(died == 1 & d$mm9 >= 2 & d$mm9 <= 6)
    prdied[is.na(prdied)] <- 0

    ## :728 -- cause exclusion applies uniformly across mm9 2..5
    mdied <- as.numeric(died == 1 & d$mm9 >= 2 & d$mm9 <= 5 &
                          !(d$mm16 %in% c(1, 2)))
    mdied[is.na(mdied)] <- 0

    ## exposure by subtraction, in the order the Stata applies it
    mexp <- rep(0, nrow(d))
    s <- lt_(first, start_month) & ge_(last, start_month)
    mexp[s] <- (last - start_month + 1)[s]
    s <- ge_(first, start_month) & le_(last, end_month)
    mexp[s] <- (last - first + 1)[s]
    s <- le_(first, end_month) & gt_(last, end_month)
    mexp[s] <- (end_month - first + 1)[s]
    mexp[!is.na(mexp) & mexp < 0] <- 0

    data.frame(age_grp = i, sex = d$sex, v005 = d$v005,
               died = died, prdied = prdied, mdied = mdied, mexp = mexp)
  })

  micro <- bind_rows(per.age)
  micro <- micro[!is.na(micro$mexp) & micro$mexp != 0, ]   # :746
  micro$yexp <- micro$mexp / 12
  micro$died[is.na(micro$died)] <- 0

  ## --- calc_mortality_rates ------------------------------------------------
  w <- micro$v005 / 1e6
  cells <- micro %>%
    mutate(wtd_deaths   = died   * w,
           wtd_prdeaths = prdied * w,
           wtd_mdeaths  = mdied  * w,
           wtd_yexp     = yexp   * w,
           unwtd_deaths = died, unwtd_yexp = yexp) %>%
    group_by(sex, age_grp) %>%
    summarise(across(c(wtd_deaths, wtd_prdeaths, wtd_mdeaths, wtd_yexp,
                       unwtd_deaths, unwtd_yexp), sum), .groups = "drop")

  ## --- get_age_distributions -----------------------------------------------
  ir <- read_dta(ir.file, col_select = any_of(c("v005","v013","awfactt","V005","V013","AWFACTT")),
                 encoding = encoding)
  names(ir) <- tolower(names(ir))
  ## :551 -- the all-women factor is required; it is 100 for all-women samples
  awf <- if ("awfactt" %in% names(ir)) as.numeric(ir$awfactt) / 100 else 1
  age_f <- data.frame(age_grp = as.numeric(ir$v013), p = (as.numeric(ir$v005)/1e6) * awf) %>%
    filter(age_grp >= 1, age_grp <= 7) %>%
    group_by(age_grp) %>% summarise(age_prop = sum(p), .groups = "drop") %>%
    mutate(age_prop = age_prop / sum(age_prop), sex = 2)

  age_m <- NULL
  if (!is.null(mr.file)) {
    mr <- read_dta(mr.file, col_select = any_of(c("mv005","mv012","mv013")))
    names(mr) <- tolower(names(mr))
    keep <- mr$mv012 >= 15 & mr$mv012 <= 49
    age_m <- data.frame(age_grp = as.numeric(mr$mv013)[keep],
                        p = as.numeric(mr$mv005)[keep] / 1e6) %>%
      filter(age_grp >= 1, age_grp <= 7) %>%
      group_by(age_grp) %>% summarise(age_prop = sum(p), .groups = "drop") %>%
      mutate(age_prop = age_prop / sum(age_prop), sex = 1)
  }
  age_dist <- bind_rows(age_f, age_m)

  ## --- merge_files ----------------------------------------------------------
  tab <- cells %>%
    left_join(age_dist, by = c("sex", "age_grp")) %>%
    mutate(mx   = wtd_deaths   / wtd_yexp,
           prmx = wtd_prdeaths / wtd_yexp,
           mmx  = wtd_mdeaths  / wtd_yexp,
           q5   = 5 * mx / (1 + nax_coef * mx))

  summ <- tab %>%
    group_by(sex) %>%
    summarise(deaths     = sum(wtd_deaths),
              prdeaths   = sum(wtd_prdeaths),
              mdeaths    = sum(wtd_mdeaths),
              yexp       = sum(wtd_yexp),
              unwtd_deaths = sum(unwtd_deaths),
              mx_adj     = if (all(!is.na(age_prop))) sum(mx   * age_prop) else NA_real_,
              prmx_adj   = if (all(!is.na(age_prop))) sum(prmx * age_prop) else NA_real_,
              mmx_adj    = if (all(!is.na(age_prop))) sum(mmx  * age_prop) else NA_real_,
              q_15_to_50 = 1 - exp(sum(log(1 - q5))),
              .groups = "drop") %>%
    mutate(prpmdf = 100 * prdeaths / deaths,
           mpmdf  = 100 * mdeaths  / deaths)

  list(by_age = tab, totals = summ, has.mm16 = has.mm16,
       micro = micro, window = c(start = unique(start_month)[1], end = unique(end_month)[1]))
}

report <- function(nm, r) {
  cat("\n===============", nm, "===============\n")
  cat("mm16 present:", r$has.mm16, "\n")
  for (i in seq_len(nrow(r$totals))) {
    x <- r$totals[i, ]
    lab <- if (x$sex == 1) "men  " else "women"
    cat(sprintf("%s  exposure %10.0f  deaths %8.1f  rate/1000 %6.3f  35q15 %6.1f\n",
                lab, x$yexp, x$deaths, 1000 * x$mx_adj, 1000 * x$q_15_to_50))
    if (x$sex == 2) {
      cat(sprintf("       preg-related deaths %6.1f  rate/1000 %6.3f  PRMDF %5.1f%%\n",
                  x$prdeaths, 1000 * x$prmx_adj, x$prpmdf))
      if (r$has.mm16)
        cat(sprintf("       maternal deaths     %6.1f  rate/1000 %6.3f  MPMDF %5.1f%%\n",
                    x$mdeaths, 1000 * x$mmx_adj, x$mpmdf))
    }
  }
}
