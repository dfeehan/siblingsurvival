## D5: package vs the Stata reference replica, Rwanda 2010, 0-4 year window.
suppressMessages({library(dplyr); library(haven)})
suppressMessages(devtools::load_all("/Users/dennis/dev/siblingsurvival"))
source("/Users/dennis/dev/siblingsurvival/data-raw/dhs-validation/stata-reference-replica.R")
REPRO <- c("[15,20)","[20,25)","[25,30)","[30,35)","[35,40)","[40,45)","[45,50)")
IR <- path.expand("~/Dropbox/maternal-mortality/maternal-mortality/data/dhs/RWIR61FL.DTA")

ref <- dhs_am(IR, lw = -4, uw = 0)

raw  <- read_dta(IR)
prep <- prep_dhs_sib_histories(raw, varmap = sibhist_varmap_dhs6,
                               add_maternal = TRUE, na.action = "include",
                               verbose = FALSE)
sib <- prep$sib.dat
sib$in.F <- ifelse(is.na((sib$sib.alive==1)&(sib$sib.sex=="f")&(sib$sib.age>=15)&(sib$sib.age<=49)),0,
             as.numeric((sib$sib.alive==1)&(sib$sib.sex=="f")&(sib$sib.age>=15)&(sib$sib.age<=49)))

est <- function(event, tp) {
  cc <- cell_config(age.groups="5yr", time.periods=tp, start.obs="sib.dob",
        end.obs="sib.endobs", event=event, age.offset="sib.dob",
        time.offset="doi", exp.scale=1/12)
  sibling_estimator(sib.dat=sib, ego.id="caseid", sib.frame.indicator="in.F",
        sib.sex="sib.sex", cell.config=cc, weights="wwgt")$asdr.agg %>%
    filter(sib.age %in% REPRO) %>% arrange(sib.sex, match(sib.age, REPRO))
}
tp <- make.time.periods(start = -60, durations = 60, names = "0-4yr")
ac <- est("sib.death.date", tp)
pr <- est("sib.preg_related.death.date", tp)

pub_fd <- c(29,49,69,84,61,58,23); pub_fe <- c(21511,26065,24195,18732,13943,9888,6566)
pub_pr <- c(4,16,20,23,17,8,3)
rw <- subset(ref$by_age, sex==2)
pw <- subset(ac,  sib.sex=="f"); pp <- subset(pr, sib.sex=="f")

cat("Rwanda 2010, women, 0-4 years before survey\n")
cat(sprintf("%-7s | %7s %7s %7s | %8s %8s %8s | %6s %6s %6s\n",
  "age","exp pkg","ref","pub","dth pkg","ref","pub","PR pkg","ref","pub"))
for (i in 1:7) cat(sprintf("%-7s | %7.0f %7.0f %7d | %8.1f %8.1f %8d | %6.1f %6.1f %6d\n",
  REPRO[i], pw$denom.hat[i], rw$wtd_yexp[i], pub_fe[i],
  pw$num.hat[i], rw$wtd_deaths[i], pub_fd[i],
  pp$num.hat[i], rw$wtd_prdeaths[i], pub_pr[i]))
cat(sprintf("%-7s | %7.0f %7.0f %7d | %8.1f %8.1f %8d | %6.1f %6.1f %6d\n", "TOTAL",
  sum(pw$denom.hat), sum(rw$wtd_yexp), 120900,
  sum(pw$num.hat), sum(rw$wtd_deaths), 373,
  sum(pp$num.hat), sum(rw$wtd_prdeaths), 91))
