# is each sibling's death maternal, by DHS coding?

As
[is_preg_related_dhs](http://dennisfeehan.org/siblingsurvival/reference/is_preg_related_dhs.md),
but stopping at `mm9 = 5` — the 42-day window — and excluding deaths
reported as due to violence or an accident (`mm16`). Only computable
when `sib.died.accident` is present, which is DHS phase 7 and later, and
in practice only 5 of the 43 surveys with a sibling roster carry it.

## Usage

``` r
is_maternal_dhs(sib_df, na.action = NULL)
```

## Arguments

- sib_df:

  the prepped sibling dataset

- na.action:

  retained for symmetry and ignored. It used to decide how a missing
  `mm12` was treated; `mm12` is no longer consulted

## Value

a logical vector, one entry per row of `sib_df`

## Details

The rule is the reference implementation's, verbatim
(`DHS-Indicators-Stata`, `Chap16_AM/AM_rates.do:728`):

    mm9 >= 2 & mm9 <= 5 & mm16 != 1 & mm16 != 2

Two details that look like edge cases but are not:

- **A missing `mm16` counts as "not an accident".** `mm16` is *not
  asked* when the death occurred during delivery, so for `mm9 = 3` it is
  missing by design — in The Gambia 2019-20 all 40 such in-window deaths
  have `mm16` missing and none of the `mm9 = 2` or `mm9 = 5` deaths do.
  Requiring `mm16 == 0` would silently drop every delivery death.

- **`mm9 = 4` is included** even though it is never assigned in
  practice. That matches the reference, which tests a range rather than
  a set.

## Changed in this version

This function previously applied the accident exclusion to codes 2 and 5
only, took code 3 unconditionally, and additionally required `mm12` to
fall in the band `100`–`141`. Given the skip pattern above, the first
two are *equivalent* to the reference wherever a survey respects it —
The Gambia 2019-20 reproduces its published Table 14.3 exactly either
way.

They part company where a survey does not. **South Africa 2016 has 3
deaths coded `mm9 = 3` with `mm16` reported as violence or an
accident**, which the old rule counted as maternal and the reference
does not. The `mm12` condition is dropped for the same reason it was
dropped from
[is_preg_related_dhs](http://dennisfeehan.org/siblingsurvival/reference/is_preg_related_dhs.md):
the reference does not use it.
