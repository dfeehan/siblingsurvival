## Build the MICS varmaps shipped with the package.
##
## Variable names verified 2026-08-21 against real mm.sav files:
##   MICS6  -- ZWE_2019 (48 vars), IRQ_2018 (28), MDG_2018 (35), COM_2022 (47)
##   MICS4/5 -- ZWE_2014 (27), MWI_2013 (32), BEN_2014 (33), and six others
## and against the World Bank microdata catalog dictionaries (catalog 4180).
##
## Names are LOWERCASE: MICS .sav files are mixed case, and
## prep_mics_sib_histories(lowercase = TRUE) normalizes them before matching.
##
## Run with: source("data-raw/make-mics-varmaps.R")

library(tibble)
library(usethis)

## ---- ego (respondent) variables, common to both schemes -------------------
## psu and stratum are absent from most MICS mm.sav files; the prep constructs
## psu from the cluster when it is missing. wdoi/wdob are deliberately NOT
## mapped: prep_mics_sib_histories() reads them under their own names to build
## `doi` and respondent `age`.
mics_ego <- tribble(
  ~orig.varname, ~new.varname,  ~sibvar, ~description,                        ~comments,
  "hh1",         "cluster",           0, "cluster number",                    NA,
  "hh2",         "hhnum",             0, "household number",                  NA,
  "ln",          "hrnum",             0, "respondent's line number",          NA,
  "wmweight",    "wwgt",              0, "women's sample weight",             "already normalized to mean 1; do NOT divide by 1e6",
  "psu",         "psu",               0, "primary sampling unit",             "absent from most MICS mm.sav files; constructed from hh1 when missing",
  "stratum",     "stratum",           0, "sampling stratum",                  "absent from most MICS mm.sav files",
  "hh6",         "ruralurban",        0, "area (urban/rural)",                NA,
  "hh7",         "region",            0, "region or province",                NA,
  "welevel",     "educ",              0, "respondent education",              NA,
  "windex5",     "wealth",            0, "wealth index quintile",             NA
)

## ---- MICS6 / MICS7 sibling roster: MM15-MM27 ------------------------------
sibhist_varmap_mics6 <- rbind(
  mics_ego,
  tribble(
    ~orig.varname, ~new.varname,               ~sibvar, ~description,                                   ~comments,
    "mmln",        "sibindex",                       1, "sibling's roster position",                    NA,
    "mm15",        "sib.sex",                        1, "sex of sibling",                               "1 male, 2 female",
    "mm16",        "sib.alive",                      1, "whether sibling is alive",                     "1 yes, 2 no, 8 DK -- recoded to the package's 1 alive / 0 dead / NA",
    "mm17",        "sib.age",                        1, "current age of sibling",                       "living siblings only",
    "mm17c",       "sib.dob",                        1, "imputed CMC date of birth",                    "constructed by MICS; absent in some countries, then approximated",
    "mm18",        "sib.death.yrsago",               1, "years ago the sibling died",                   "dead siblings only",
    "mm18c",       "sib.death.date",                 1, "imputed CMC date of death",                    "constructed by MICS; absent in some countries, then approximated",
    "mm19",        "sib.death.age",                  1, "sibling's age at death",                       NA,
    "mm22",        "sib.preg.at.death",              1, "was she pregnant when she died",               NA,
    "mm23",        "sib.died.childbirth",            1, "did she die during childbirth",                "MM23=1 skips MM26/MM27, so those are NA by design",
    "mm24",        "sib.died.postpartum",            1, "died within two months of end of pregnancy",   NA,
    "mm25",        "sib.days.postpartum.death",      1, "days after end of pregnancy she died",         "asked only when MM24=1; the 42-day maternal cut applies here",
    "mm26",        "sib.died.violence",              1, "death due to an act of violence",              "MICS6+ only",
    "mm27",        "sib.died.accident",              1, "death due to an accident",                     "MICS6+ only; presence enables maternal (not just pregnancy-related) deaths"
  )
)

## ---- MICS4 / MICS5 sibling roster: MM5-MM13 -------------------------------
## No violence/accident items, so MICS4/5 supports pregnancy-related mortality
## only -- the same limitation as DHS phases 2-6.
sibhist_varmap_mics4 <- rbind(
  mics_ego,
  tribble(
    ~orig.varname, ~new.varname,          ~sibvar, ~description,                                 ~comments,
    "mmln",        "sibindex",                  1, "sibling's roster position",                  NA,
    "mm5",         "sib.sex",                   1, "sex of sibling",                             "1 male, 2 female",
    "mm6",         "sib.alive",                 1, "whether sibling is alive",                   "1 yes, 2 no, 8 DK",
    "mm7",         "sib.age",                   1, "current age of sibling",                     "living siblings only",
    "mm7c",        "sib.dob",                   1, "imputed CMC date of birth",                  "absent in some countries (eg BTN_2010), then approximated",
    "mm8",         "sib.death.yrsago",          1, "years ago the sibling died",                 "dead siblings only",
    "mm8c",        "sib.death.date",            1, "imputed CMC date of death",                  "absent in some countries (eg BTN_2010), then approximated",
    "mm9",         "sib.death.age",             1, "sibling's age at death",                     NA,
    "mm10",        "sib.preg.at.death",         1, "was she pregnant when she died",             NA,
    "mm11",        "sib.died.childbirth",       1, "did she die during childbirth",              NA,
    "mm12",        "sib.died.postpartum",       1, "died within two months of end of pregnancy", NA,
    "mm13",        "sib.num.children",          1, "live births during her lifetime",            "dropped in MICS6"
  )
)

sibhist_varmap_mics5 <- sibhist_varmap_mics4
sibhist_varmap_mics7 <- sibhist_varmap_mics6

usethis::use_data(sibhist_varmap_mics4, overwrite = TRUE)
usethis::use_data(sibhist_varmap_mics5, overwrite = TRUE)
usethis::use_data(sibhist_varmap_mics6, overwrite = TRUE)
usethis::use_data(sibhist_varmap_mics7, overwrite = TRUE)
