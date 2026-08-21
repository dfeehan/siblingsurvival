# Synthetic MICS-shaped data for testing the MICS path.
#
# Shaped after the real MICS6 mm.sav (verified against Zimbabwe 2019 and Iraq
# 2018): one row per reported sibling, uppercase questionnaire items, lowercase
# derived/design variables, cluster+household+line as the respondent key.
#
# This is a FABRICATION with known answers, not a sample of real data. Real MICS
# microdata is registration-gated and cannot ship with the package.
#
# Design (all dates CMC; interview at 1400 = July 2016):
#
#   Respondent 1 (cluster 1, hh 1, line 2), 4 siblings:
#     S1 female, alive, 30
#     S2 female, dead 3 yrs ago at 20, pregnant when she died   -> MATERNAL + PR
#     S3 male,   dead 2 yrs ago at 41, died in an accident      -> neither
#     S4 female, dead 1 yr  ago at 8                            -> under 12, neither
#   Respondent 2 (cluster 1, hh 3, line 1), 3 siblings:
#     S1 female, dead 4 yrs ago at 33, died during childbirth,
#                MM26/MM27 NOT ASKED (skip pattern)             -> MATERNAL + PR
#     S2 female, dead 5 yrs ago at 27, postpartum at 55 days    -> PR ONLY (>42d)
#     S3 female, alive, 22
#   Respondent 3 (cluster 2, hh 1, line 1), 2 siblings:
#     S1 female, survival status DK                             -> dropped
#     S2 male,   alive, 35
#
# So: 2 maternal deaths, 3 pregnancy-related deaths, 1 sibling dropped for DK.

make_mics6_mm <- function() {
  tibble::tribble(
    ~HH1, ~HH2, ~LN, ~MMLN, ~MM15, ~MM16, ~MM17, ~MM18, ~MM19, ~MM22, ~MM23, ~MM24, ~MM25, ~MM26, ~MM27,
    # respondent 1
       1L,   1L,  2L,    1L,    2L,    1L,   30L,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,
       1L,   1L,  2L,    2L,    2L,    2L,    NA,    3L,   20L,    1L,    NA,    NA,    NA,    2L,    2L,
       1L,   1L,  2L,    3L,    1L,    2L,    NA,    2L,   41L,    NA,    NA,    NA,    NA,    2L,    1L,
       1L,   1L,  2L,    4L,    2L,    2L,    NA,    1L,    8L,    NA,    NA,    NA,    NA,    2L,    2L,
    # respondent 2
       1L,   3L,  1L,    1L,    2L,    2L,    NA,    4L,   33L,    2L,    1L,    NA,    NA,    NA,    NA,
       1L,   3L,  1L,    2L,    2L,    2L,    NA,    5L,   27L,    2L,    2L,    1L,   55L,    2L,    2L,
       1L,   3L,  1L,    3L,    2L,    1L,   22L,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,
    # respondent 3
       2L,   1L,  1L,    1L,    2L,    8L,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA,
       2L,   1L,  1L,    2L,    1L,    1L,   35L,    NA,    NA,    NA,    NA,    NA,    NA,    NA,    NA
  ) %>%
    dplyr::mutate(
      WM1 = HH1, WM2 = HH2, WM3 = LN,
      WDOI = 1400L,
      # respondents aged 28, 35, 41 at interview
      WDOB = WDOI - 12L * c(28L, 28L, 28L, 28L, 35L, 35L, 35L, 41L, 41L),
      wmweight = c(1.0, 1.0, 1.0, 1.0, 1.2, 1.2, 1.2, 0.8, 0.8),
      psu = HH1,
      stratum = 1L,
      welevel = 2L
    )
}

# Variant with the constructed CMC columns MICS6 usually ships (MM17C/MM18C).
# Values are consistent with the ages and years-ago above, so a prep that reads
# them should agree with one that derives them.
make_mics6_mm_with_cmc <- function() {
  d <- make_mics6_mm()
  d %>%
    dplyr::mutate(
      MM18C = dplyr::if_else(is.na(MM18), NA_real_, WDOI - (12 * MM18 + 6)),
      MM17C = dplyr::case_when(
        MM16 == 1 & !is.na(MM17) ~ WDOI - (12 * MM17 + 6),
        !is.na(MM18C) & !is.na(MM19) ~ MM18C - 12 * MM19,
        TRUE ~ NA_real_)
    )
}

# The MICS6 varmap, lowercase, matching the real mm.sav names.
make_mics6_varmap <- function() {
  tibble::tribble(
    ~orig.varname,  ~new.varname,        ~sibvar,
    "mm15",         "sib.sex",                 1,
    "mm16",         "sib.alive",               1,
    "mm17",         "sib.age",                 1,
    "mm18",         "sib.death.yrsago",        1,
    "mm19",         "sib.death.age",           1,
    "mm22",         "sib.preg.at.death",       1,
    "mm23",         "sib.died.childbirth",     1,
    "mm24",         "sib.died.postpartum",     1,
    "mm25",         "sib.days.postpartum.death", 1,
    "mm26",         "sib.died.violence",       1,
    "mm27",         "sib.died.accident",       1,
    "mmln",         "sibindex",                1,
    "wdoi",         "doi",                     0,
    "wmweight",     "wwgt",                    0,
    "psu",          "psu",                     0
  )
}

# Same, plus the constructed CMC columns.
make_mics6_varmap_cmc <- function() {
  dplyr::bind_rows(
    make_mics6_varmap(),
    tibble::tibble(orig.varname = c("mm17c", "mm18c"),
                   new.varname  = c("sib.dob", "sib.death.date"),
                   sibvar       = c(1, 1))
  )
}
