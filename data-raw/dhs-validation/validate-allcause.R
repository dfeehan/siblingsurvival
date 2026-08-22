## Validate ALL-CAUSE adult mortality against published DHS tables.
##
## The maternal work validated the pregnancy-related and maternal columns. This
## checks the estimator underneath them, on a quantity that involves no cause
## recode at all: deaths and exposure by age and sex, the age-adjusted rates
## built from them, and 35q15.
##
##   source("data-raw/dhs-validation/validate-allcause.R")
##
## Targets are in published-targets.csv. Each report chooses its own window, and
## they differ -- Malawi 2000 and Gambia 2019-20 publish 0-6 years, the three
## Rwanda surveys publish 0-4 -- so the window is part of the target, not a
## constant. Getting this wrong is the easiest way to "fail" a validation that
## would otherwise pass exactly.
##
## Standardisation: published DHS reports standardise **both** sexes by the age
## distribution of the survey respondents -- the women. That is what
## get_ego_age_distn(only_females = TRUE) returns, so no external file is needed.
##
## This is worth stating because Chap16_AM/AM_rates.do does something else: its
## get_age_distributions takes the men's distribution from the men's MR file.
## Standardising that way moves the male rate *away* from every published figure
## checked here (Malawi 2000: 11.162 against a published 11.1, where the
## respondents' distribution gives 11.064; Rwanda 2005: 7.285 against 7.39,
## where it gives 7.393). The MR variant is computed alongside where an MR file
## is available, so the comparison stays visible.

suppressMessages({ library(dplyr); library(haven) })
devtools::load_all(here::here())
source(here::here("data-raw", "dhs-validation", "stata-reference-replica.R"))

REPRO <- c("[15,20)","[20,25)","[25,30)","[30,35)","[35,40)","[40,45)","[45,50)")

IR_DIR <- path.expand("~/Dropbox/maternal-mortality/maternal-mortality/data/dhs")
MR_DIR <- here::here("data-raw", "dhs-data")

SURVEYS <- list(
  list(id = "MWIR41FL", varmap = "dhs4", lw = -6, label = "Malawi 2000"),
  list(id = "RWIR53FL", varmap = "dhs5", lw = -4, label = "Rwanda 2005"),
  list(id = "RWIR61FL", varmap = "dhs6", lw = -4, label = "Rwanda 2010"),
  list(id = "RWIR70FL", varmap = "dhs7", lw = -4, label = "Rwanda 2014-15"),
  list(id = "GMIR81FL", varmap = "dhs8", lw = -6, label = "Gambia 2019-20"))

targets <- read.csv(here::here("data-raw", "dhs-validation", "published-targets.csv"),
                    stringsAsFactors = FALSE)

pub <- function(sv, quantity, sex, age) {
  v <- targets$value[targets$survey == sv & targets$quantity == quantity &
                       targets$sex == sex & targets$age_group == age]
  if (length(v) == 0) NA_real_ else as.numeric(v[1])
}

##' Package estimates of all-cause deaths and exposure by age and sex.
package_allcause <- function(id, varmap, lw) {

  ir <- file.path(IR_DIR, paste0(id, ".DTA"))
  raw <- tryCatch(read_dta(ir),
                  error = function(e) read_dta(ir, encoding = "latin1"))

  prep <- prep_dhs_sib_histories(raw, varmap = get(paste0("sibhist_varmap_", varmap)),
                                 verbose = FALSE)
  sib <- prep$sib.dat
  sib$in.F <- ifelse(is.na((sib$sib.alive == 1) & (sib$sib.sex == "f") &
                             (sib$sib.age >= 15) & (sib$sib.age <= 49)), 0,
                     as.numeric((sib$sib.alive == 1) & (sib$sib.sex == "f") &
                                  (sib$sib.age >= 15) & (sib$sib.age <= 49)))

  ## the window each report used; -lw years back, ending the month before interview
  months <- 12 * (-lw + 1)
  tp <- make.time.periods(start = -months, durations = months,
                          names = paste0(months / 12, "yr"))

  cc <- cell_config(age.groups = "5yr", time.periods = tp,
                    start.obs = "sib.dob", end.obs = "sib.endobs",
                    event = "sib.death.date", age.offset = "sib.dob",
                    time.offset = "doi", exp.scale = 1/12)

  est <- sibling_estimator(sib.dat = sib, ego.id = "caseid",
                           sib.frame.indicator = "in.F", sib.sex = "sib.sex",
                           cell.config = cc, weights = "wwgt")$asdr.agg %>%
    filter(sib.age %in% REPRO) %>% arrange(sib.sex, match(sib.age, REPRO))

  ## female age distribution from the respondents, as DHS does
  fprop <- get_ego_age_distn(prep$ego.dat, only_females = TRUE) %>%
    filter(age.cat %in% REPRO) %>% arrange(match(age.cat, REPRO))

  ## male age distribution has to come from the men's file; NULL if absent
  mr <- file.path(MR_DIR, paste0(sub("IR", "MR", id), ".DTA"))
  mprop <- NULL
  if (file.exists(mr)) {
    m <- read_dta(mr, col_select = any_of(c("mv005", "mv012", "mv013")))
    names(m) <- tolower(names(m))
    keep <- m$mv012 >= 15 & m$mv012 <= 49
    mprop <- data.frame(age_grp = as.numeric(m$mv013)[keep],
                        p = as.numeric(m$mv005)[keep] / 1e6) %>%
      filter(age_grp >= 1, age_grp <= 7) %>%
      group_by(age_grp) %>% summarise(p = sum(p), .groups = "drop") %>%
      mutate(p = p / sum(p)) %>% pull(p)
  }

  list(f = est %>% filter(sib.sex == "f"),
       m = est %>% filter(sib.sex == "m"),
       fprop = fprop$agegrp_prop / sum(fprop$agegrp_prop),
       mprop = mprop)
}

cat("\nAll-cause adult mortality against published DHS tables\n")
cat("=====================================================\n")

out <- list()
for (s in SURVEYS) {

  r <- package_allcause(s$id, s$varmap, s$lw)

  cat(sprintf("\n--- %s (%s), window 0-%d years ---\n", s$label, s$id, -s$lw))
  cat(sprintf("%-9s | %8s %8s %6s | %9s %9s %6s\n",
              "age", "deaths", "pub", "ratio", "exposure", "pub", "ratio"))

  for (sex in c("f", "m")) {
    d <- r[[sex]]
    cat(sprintf("        %s\n", if (sex == "f") "WOMEN" else "MEN"))
    for (i in seq_along(REPRO)) {
      pd <- pub(s$id, "allcause_deaths", sex, REPRO[i])
      pe <- pub(s$id, "exposure",        sex, REPRO[i])
      cat(sprintf("%-9s | %8.1f %8.0f %6.4f | %9.0f %9.0f %6.4f\n",
                  REPRO[i], d$num.hat[i], pd, d$num.hat[i]/pd,
                  d$denom.hat[i], pe, d$denom.hat[i]/pe))
    }
    pd <- pub(s$id, "allcause_deaths", sex, "total")
    pe <- pub(s$id, "exposure",        sex, "total")
    prate <- pub(s$id, "allcause_rate", sex, "age_adjusted")
    ## the respondents' age distribution, for both sexes -- what reports use
    adj <- 1000 * sum(d$asdr.hat * r$fprop)
    ## the AM_rates.do variant, men only, where an MR file is to hand
    adj.mr <- if (sex == "m" && !is.null(r$mprop)) 1000 * sum(d$asdr.hat * r$mprop) else NA_real_
    q <- q15_to_50(d$asdr.hat)
    cat(sprintf("%-9s | %8.1f %8.0f %6.4f | %9.0f %9.0f %6.4f\n",
                "TOTAL", sum(d$num.hat), pd, sum(d$num.hat)/pd,
                sum(d$denom.hat), pe, sum(d$denom.hat)/pe))
    cat(sprintf("%-9s   age-adjusted rate %6.3f  (published %s)   35q15 %6.1f%s\n",
                "", adj, format(prate), q,
                if (is.na(adj.mr)) "" else sprintf("   [MR-standardised: %6.3f]", adj.mr)))

    out[[length(out) + 1]] <- data.frame(
      survey = s$id, label = s$label, sex = sex,
      deaths = sum(d$num.hat), deaths_pub = pd,
      exposure = sum(d$denom.hat), exposure_pub = pe,
      rate_adj = adj, rate_adj_mr = adj.mr, rate_pub = prate, q15_50 = q,
      stringsAsFactors = FALSE)
  }
}

res <- bind_rows(out)
res$deaths_ratio   <- res$deaths / res$deaths_pub
res$exposure_ratio <- res$exposure / res$exposure_pub
res$rate_ratio     <- res$rate_adj / res$rate_pub

cat("\n\nSummary\n=======\n")
print(res[, c("label","sex","deaths_ratio","exposure_ratio","rate_ratio")],
      row.names = FALSE, digits = 6)

write.csv(res, here::here("data-raw", "dhs-validation", "allcause-results.csv"),
          row.names = FALSE)
cat("\nwrote data-raw/dhs-validation/allcause-results.csv\n")
