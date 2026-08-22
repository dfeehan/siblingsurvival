# is each sibling's death pregnancy-related, by MICS coding?

MICS asks three separate binaries where the DHS uses one coded item:
`MM22` pregnant when she died, `MM23` died during childbirth, and `MM24`
died within two months of the end of a pregnancy (`MM10`, `MM11`, `MM12`
in MICS4/5). Any of the three makes the death pregnancy-related, with no
cause exclusion.

## Usage

``` r
is_preg_related_mics(sib_df, preg.window = c("2months", "42days"))
```

## Arguments

- sib_df:

  the prepped sibling dataset

- preg.window:

  width of the postpartum window: `"2months"` (default) or `"42days"`.
  See Details

## Value

a logical vector, one entry per row of `sib_df`

## Details

`preg.window` chooses how wide the postpartum window is:

- `"2months"` (the default) takes `MM24 = 1` at face value, so the
  window is however long the respondent understood "two months" to be.
  This is the widest reading and the one this package has always used
  for MICS.

- `"42days"` additionally requires `MM25 < 42`. This is the WHO
  definition of a pregnancy-related death, it is what UNICEF's own
  tabulation syntax computes, and it is therefore what published MICS
  tables report. It is also consistent with
  [is_preg_related_dhs](http://dennisfeehan.org/siblingsurvival/reference/is_preg_related_dhs.md),
  which already applies a 42-day cut through the `mm12` band
  `100`–`141`.

Only `"42days"` reproduces published MICS figures. On Iraq 2018 it gives
64.4 pregnancy-related deaths against a published 64, and on Madagascar
2018 136.6 against a published 137; `"2months"` gives 67.7 and 140.4.

See the "Working with MICS sibling history data" vignette.
