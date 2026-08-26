# helper to prep the sib dataset

Shared by
[prep_dhs_sib_histories](http://dennisfeehan.org/siblingsurvival/reference/prep_dhs_sib_histories.md),
[prep_nrsim_sib_histories](http://dennisfeehan.org/siblingsurvival/reference/prep_nrsim_sib_histories.md)
and
[prep_mics_sib_histories](http://dennisfeehan.org/siblingsurvival/reference/prep_mics_sib_histories.md).

## Usage

``` r
get_sib_df(
  ego.dat,
  sib.attrib,
  verbose = FALSE,
  reshape = TRUE,
  max.plausible.age = 110,
  death.exposure = c("dhs", "mics")
)
```

## Arguments

- ego.dat:

  the prepped ego dataset

- sib.attrib:

  vector with sibling attribute columns (see
  [prep_dhs_sib_histories](http://dennisfeehan.org/siblingsurvival/reference/prep_dhs_sib_histories.md))

- verbose:

  see
  [prep_dhs_sib_histories](http://dennisfeehan.org/siblingsurvival/reference/prep_dhs_sib_histories.md)

- reshape:

  is `ego.dat` wide, with one column per sibling attribute per sibling
  (TRUE, the DHS layout), or already one row per reported sibling
  (FALSE, the MICS `mm.sav` layout)?

- max.plausible.age:

  warn about siblings whose derived date of birth implies they would be
  older than this at the date of interview. Such rows mean the reported
  years-since-death and age at death are jointly inconsistent. Note this
  is *not* a check on whether a sibling died before the respondent was
  born, which is perfectly possible

- death.exposure:

  whether a sibling who died contributes the month of death as exposure.
  `"dhs"` (the default) counts it, matching `Chap16_AM/AM_rates.do`;
  `"mics"` stops the month before, matching the MICS6 tabulation syntax.
  The two references genuinely disagree here, so it cannot be settled by
  getting it "right"

## Value

a prepped sibling dataset, used by the `prep_*_sib_histories()`
functions
