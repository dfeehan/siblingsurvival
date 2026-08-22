# is each sibling's death maternal, by DHS coding?

As
[is_preg_related_dhs](http://dennisfeehan.org/siblingsurvival/reference/is_preg_related_dhs.md),
but additionally excluding deaths reported as due to violence or an
accident. Only computable when `sib.died.accident` is present, which is
DHS phase 7 and later.

## Usage

``` r
is_maternal_dhs(sib_df, na.action)
```

## Arguments

- sib_df:

  the prepped sibling dataset

- na.action:

  see
  [add_maternal_deaths](http://dennisfeehan.org/siblingsurvival/reference/add_maternal_deaths.md)

## Value

a logical vector, one entry per row of `sib_df`
