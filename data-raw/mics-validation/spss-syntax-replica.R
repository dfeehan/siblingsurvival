## A literal R transcription of UNICEF's own MICS6 tabulation syntax
##   "MICS6 - 06 - TM.9.1&TM.9.2&TM.9.3&DQ.7.1&DQ7.2.sps"  (v02, 2020-04-14)
## from the MICS6 "42 Syntax Files" distribution.
##
## This is NOT part of the package and is not what the package does. It exists so
## that the published tables can be reproduced independently of the package, and
## so that any gap between the two can be attributed to a specific convention
## rather than guessed at. See ../../vignettes/mics-data.Rmd.
##
## Usage:
##   source("data-raw/mics-validation/spss-syntax-replica.R")
##   r <- spss_tm9("data-raw/mics-data/IQ2018"); report("IQ2018", r)
##   f <- spss_fert("data-raw/mics-data/IQ2018")   # GFR and TFR, for the MMR
##   100 * r$totals$mrate / f$gfr                  # maternal mortality ratio
##
## Requires wm.sav, mm.sav and (for spss_fert) bh.sav. MICS microdata is
## registration-gated and is not redistributed with this package.
##
## Conventions this encodes, each of which is undocumented in the MICS reports:
##   * the seven-year window is [wdoi - 84, wdoi - 1] -- the interview month is
##     excluded
##   * a decedent's exposure ends at MM18C - 1, the month *before* death
##   * a death is assigned to the age group holding at that same month, not to
##     the age at death
##   * siblings whose survival status is unknown are kept, with full exposure
##   * age standardisation uses the weighted age distribution of women with a
##     completed interview (WM17 = 1)
##   * a "maternal" death is MM22 = 1 | MM23 = 1 | (MM24 = 1 & MM25 < 42), with
##     no reference to MM26 (violence) or MM27 (accident) at all
##   * 35q15 uses nax = 2.6, not the 2.5 the syntax's own header documents

suppressMessages({ library(haven); library(dplyr) })

## SPSS trunc() truncates toward zero
tr <- function(x) trunc(x)

spss_tm9 <- function(dir, nax_coef = 2.4) {

  ## ---- women's file: age distribution for standardisation -----------------
  wm <- read_sav(file.path(dir, "wm.sav"))
  names(wm) <- tolower(names(wm))
  wm <- wm[wm$wm17 %in% 1, ]                       # select if (WM17 = 1)

  agedis <- wm %>%
    group_by(wage) %>%
    summarise(Nwomen = sum(wmweight), .groups = "drop") %>%   # weighted N
    mutate(propagegrp = Nwomen / sum(Nwomen)) %>%
    filter(wage >= 1, wage <= 7) %>%
    rename(zage = wage)

  ## ---- maternal mortality module ------------------------------------------
  mm <- read_sav(file.path(dir, "mm.sav"))
  names(mm) <- tolower(names(mm))

  kmax <- 1; kmin <- 84

  d <- mm %>%
    filter(mm15 %in% c(1, 2)) %>%                  # select if (MM15 = 1 or 2)
    mutate(w = wmweight)

  ## exposure window
  d <- d %>% mutate(
    higcm = wdoi - kmax,
    higcm = ifelse(mm16 %in% 2 & !is.na(mm18c) & (mm18c - 1) < higcm, mm18c - 1, higcm),
    lowcm = wdoi - kmin,
    lowcm = ifelse(!is.na(mm17c) & lowcm < mm17c, mm17c, lowcm),
    totexp = higcm - lowcm + 1,
    totexp = ifelse(!is.na(totexp) & totexp < 0, 0, totexp),
    higage = tr((higcm - mm17c) / 60)
  )

  ## death flags
  in_win <- with(d, mm16 %in% 2 & !is.na(higage) & higage >= 3 & higage <= 9 &
                     !is.na(mm18c) & mm18c >= (wdoi - kmin) & mm18c <= (wdoi - kmax))
  d <- d %>% mutate(
    adm = ifelse(in_win & mm15 %in% 1, 1, 0),
    adf = ifelse(in_win & mm15 %in% 2, 1, 0),
    md  = ifelse(in_win & mm15 %in% 2 &
                   (mm22 %in% 1 | mm23 %in% 1 |
                      (mm24 %in% 1 & !is.na(mm25) & mm25 < 42)), 1, 0)
  )

  ## exposure split across up to three five-year age groups
  clamp <- function(x, tot) { x <- pmin(x, 60); pmin(x, tot) }
  d <- d %>% mutate(
    higexp = higcm - mm17c - higage * 60 + 1,
    higexp = ifelse(!is.na(higexp) & higexp < 0, 0, higexp),
    higexp = clamp(higexp, totexp),
    midage = higage - 1,
    midexp = clamp(totexp - higexp, totexp),
    lowage = midage - 1,
    lowexp = totexp - higexp - midexp,
    mhigexp = ifelse(mm15 %in% 1, higexp, 0),
    mmidexp = ifelse(mm15 %in% 1, midexp, 0),
    mlowexp = ifelse(mm15 %in% 1, lowexp, 0),
    higexp  = ifelse(mm15 %in% 1, 0, higexp),
    midexp  = ifelse(mm15 %in% 1, 0, midexp),
    lowexp  = ifelse(mm15 %in% 1, 0, lowexp)
  )

  agg <- function(agevar, cols) {
    z <- d[[agevar]] - 2
    keep <- !is.na(z) & z >= 1 & z <= 7
    dd <- d[keep, ]; zz <- z[keep]
    out <- data.frame(zage = zz)
    for (cn in cols) out[[cn]] <- dd[[cn]] * dd$w
    out %>% group_by(zage) %>% summarise(across(everything(), ~sum(.x, na.rm = TRUE)),
                                         .groups = "drop")
  }

  deaths <- agg("higage", c("adm", "adf", "md"))
  e1 <- agg("higage", c("higexp", "mhigexp"))
  e2 <- agg("midage", c("midexp", "mmidexp"))
  e3 <- agg("lowage", c("lowexp", "mlowexp"))

  tab <- deaths %>%
    left_join(e1, by = "zage") %>% left_join(e2, by = "zage") %>%
    left_join(e3, by = "zage") %>% left_join(agedis, by = "zage") %>%
    mutate(across(everything(), ~ifelse(is.na(.x), 0, .x))) %>%
    mutate(
      totexp  = (higexp + midexp + lowexp) / 12,
      mtotexp = (mhigexp + mmidexp + mlowexp) / 12,
      adfmr = 1000 * adf / totexp,
      admmr = 1000 * adm / mtotexp,
      mrate = 1000 * md  / totexp
    )

  totals <- list(
    adf = sum(tab$adf), adm = sum(tab$adm), md = sum(tab$md),
    totexp = sum(tab$totexp), mtotexp = sum(tab$mtotexp),
    adfmr = sum(tab$adfmr * tab$propagegrp),
    admmr = sum(tab$admmr * tab$propagegrp),
    mrate = sum(tab$mrate * tab$propagegrp)
  )

  ## 35q15 -- note the syntax uses 2.4, not the 2.5 its own header documents
  fq <- 1 - (5 * (tab$adfmr / 1000) / (1 + nax_coef * (tab$adfmr / 1000)))
  mq <- 1 - (5 * (tab$admmr / 1000) / (1 + nax_coef * (tab$admmr / 1000)))
  totals$f35q15 <- 1000 * (1 - prod(fq))
  totals$m35q15 <- 1000 * (1 - prod(mq))
  totals$pmdf   <- 100 * totals$md / totals$adf

  list(by_age = tab, totals = totals, mm = d)
}

fmt <- function(x, k = 3) formatC(x, format = "f", digits = k, big.mark = ",")

report <- function(nm, r) {
  t <- r$totals
  cat("\n===============", nm, "===============\n")
  cat(sprintf("female exposure  %s   deaths %s   rate %s\n",
              fmt(t$totexp,0), fmt(t$adf,1), fmt(t$adfmr,3)))
  cat(sprintf("male   exposure  %s   deaths %s   rate %s\n",
              fmt(t$mtotexp,0), fmt(t$adm,1), fmt(t$admmr,3)))
  cat(sprintf("maternal deaths  %s   rate %s   pct of female deaths %s\n",
              fmt(t$md,1), fmt(t$mrate,3), fmt(t$pmdf,1)))
  cat(sprintf("35q15  women %s   men %s\n", fmt(t$f35q15,1), fmt(t$m35q15,1)))
}

## ---- fertility half of the syntax: GFR, TFR, MMR, lifetime risk ------------
spss_fert <- function(dir) {
  wm <- read_sav(file.path(dir, "wm.sav")); names(wm) <- tolower(names(wm))
  wm <- wm[wm$wm17 %in% 1, ]
  kmax <- 1; kmin <- 84; totexp0 <- kmin - kmax + 1

  w <- wm %>% mutate(
    higcm  = wdoi - kmax,
    higage = tr((higcm - wdob) / 60),
    higexp = pmin(pmin(higcm - wdob - higage * 60 + 1, 60), totexp0),
    midage = higage - 1,
    midexp = pmin(pmin(totexp0 - higexp, 60), totexp0),
    lowage = midage - 1,
    lowexp = totexp0 - higexp - midexp)

  agg <- function(av, cn) {
    z <- w[[av]] - 2; k <- !is.na(z) & z >= 1 & z <= 7
    data.frame(zage = z[k], v = w[[cn]][k] * w$wmweight[k]) %>%
      group_by(zage) %>% summarise(!!cn := sum(v), .groups = "drop")
  }
  exposure <- agg("higage","higexp") %>%
    full_join(agg("midage","midexp"), by="zage") %>%
    full_join(agg("lowage","lowexp"), by="zage")

  agedis <- wm %>% group_by(wage) %>%
    summarise(Nwomen = sum(wmweight), .groups="drop") %>%
    mutate(propagegrp = Nwomen/sum(Nwomen)) %>%
    filter(wage>=1, wage<=7) %>% rename(zage = wage)

  bh <- read_sav(file.path(dir, "bh.sav")); names(bh) <- tolower(names(bh))
  bz <- tr((bh$bh4c - bh$wdob)/60) - 2
  keep <- !is.na(bz) & bh$bh4c >= bh$wdoi - kmin & bh$bh4c <= bh$wdoi - kmax &
          bz >= 1 & bz <= 7
  births <- data.frame(zage = bz[keep], b = bh$wmweight[keep]) %>%
    group_by(zage) %>% summarise(births = sum(b), .groups="drop")

  f <- births %>% left_join(exposure, by="zage") %>% left_join(agedis, by="zage") %>%
    mutate(across(c(lowexp, midexp), ~ifelse(is.na(.x), 0, .x)),
           texp = (higexp + midexp + lowexp)/12,
           asfr = births/texp,
           asfr_adj = asfr * propagegrp)
  list(gfr = sum(f$asfr_adj), tfr = 5 * sum(f$asfr))
}
