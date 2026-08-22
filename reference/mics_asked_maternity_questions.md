# were the MICS maternity questions asked of this sibling?

MICS routes male siblings, and sisters who died before age 12, past the
maternity items – so `MM22` through `MM25` are `NA` *by design* for
them, not missing data.

## Usage

``` r
mics_asked_maternity_questions(sib_df)
```

## Arguments

- sib_df:

  the prepped sibling dataset

## Value

a logical vector, one entry per row of `sib_df`

## Details

The age test is deliberately "not *known* to have died under 12" rather
than "known to have died at 12 or over". A sister whose age at death was
reported as don't-know (`MM19 = 98`) is set to `NA` by the prep, and the
stricter test silently dropped her even when she had answered
`MM22`–`MM24` affirmatively – which is itself proof she was asked. That
cost 7 pregnancy-related deaths in Iraq 2018 and 5 in Zimbabwe 2019.
