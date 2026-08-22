# is each sibling's death maternal, by MICS coding?

As
[is_preg_related_mics](http://dennisfeehan.org/siblingsurvival/reference/is_preg_related_mics.md),
but restricted to deaths within 42 days of the end of a pregnancy and
excluding deaths due to violence (`MM26`) or an accident (`MM27`). Only
computable for MICS6 and MICS7; MICS4 and MICS5 ask no cause-of-death
questions.

## Usage

``` r
is_maternal_mics(sib_df, na.action)
```

## Arguments

- sib_df:

  the prepped sibling dataset

- na.action:

  how to treat a sibling who died within two months of the end of a
  pregnancy (`MM24 = 1`) but whose day count (`MM25`) is missing. See
  [add_maternal_deaths](http://dennisfeehan.org/siblingsurvival/reference/add_maternal_deaths.md)

## Value

a logical vector, one entry per row of `sib_df`
