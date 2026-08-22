# add pregnancy-related and maternal death info to a sibling dataset

add pregnancy-related and maternal death info to a sibling dataset

## Usage

``` r
add_maternal_deaths(
  sib_df,
  style = c("dhs", "mics6", "mics4"),
  na.action = NULL,
  preg.window = c("2months", "42days"),
  keep_missing = FALSE,
  verbose = TRUE
)
```

## Arguments

- sib_df:

  the prepped sibling dataset (probably from
  [prep_dhs_sib_histories](http://dennisfeehan.org/siblingsurvival/reference/prep_dhs_sib_histories.md)
  or
  [prep_mics_sib_histories](http://dennisfeehan.org/siblingsurvival/reference/prep_mics_sib_histories.md))

- style:

  which questionnaire the coding follows: `"dhs"` (the default),
  `"mics6"` for MICS6/MICS7, or `"mics4"` for MICS4/MICS5

- na.action:

  how to treat a death that falls in the right window but whose timing
  detail is missing – `"include"` counts it, `"exclude"` does not.
  **Required for the MICS styles**; see Details

- preg.window:

  width of the postpartum window used for the *pregnancy-related* column
  under the MICS styles: `"2months"` (the default, and what this package
  has always done) or `"42days"`, which is what published MICS tables
  report. Ignored for `style = "dhs"`, which already applies a 42-day
  cut. See Details

- keep_missing:

  not currently used

- verbose:

  report detailed summaries?

## Value

`sib_df` with columns `sib.preg_related.death.date` and
`sib.maternal.death.date` added

## Details

Two quantities are computed, and they are not the same thing:

- **pregnancy-related** – died while pregnant, during childbirth, or
  within the postpartum window, *whatever the cause*

- **maternal** – as above but within 42 days, and excluding deaths due
  to violence or an accident

`sib.maternal.death.date` is only computable when the data identify
accidental deaths, which means DHS phase 7 and later, or MICS6 and
later. Otherwise it is `NA` and only the pregnancy-related column is
usable – the same limitation applies to DHS phases 2–6 and to
MICS4/MICS5.

Siblings who did not die of the relevant cause get a death date of `-1`
rather than `NA`, so that they still contribute exposure. Both columns
are set to `NA` for male siblings.

**Note for MICS users:** the tables published in MICS survey reports
under the heading "Maternal mortality", with a column labelled "Maternal
Deaths", do *not* report maternal deaths as defined above. UNICEF's own
tabulation syntax counts `MM22 = 1 | MM23 = 1 | (MM24 = 1 & MM25 < 42)`
and never reads the violence (`MM26`) or accident (`MM27`) items at all,
despite the footnote in the reports saying those causes are excluded. So
the published column is a **pregnancy-related** count on a 42-day
window. To reproduce it, use `sib.preg_related.death.date` with
`preg.window = "42days"`. See the vignette "Working with MICS sibling
history data".

### Choosing `na.action`

`na.action` has **no default for the MICS styles**, because it is a
substantive choice about the estimand rather than a coding detail, and
it should be made deliberately and reported.

In MICS, the number of days after the end of a pregnancy (`MM25`) is
asked only of sisters who died within two months (`MM24 = 1`). When that
day count is missing, `na.action` decides whether she falls inside the
42-day maternal window. In three MICS6 surveys examined this affected 5
of 38 such deaths in Iraq 2018 and none at all in Zimbabwe 2019 or
Madagascar 2018, so the choice is usually immaterial – but not always,
and it only ever moves the maternal column, never the pregnancy-related
one.

For `style = "dhs"`, `na.action` defaults to `"include"`, which is what
this package has always done: a missing `sib.time.delivery.death` was
treated as falling in the window. That default is kept so existing
results do not move.

## Examples

``` r
  # TODO - write example code
```
