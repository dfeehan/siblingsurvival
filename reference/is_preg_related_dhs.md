# is each sibling's death pregnancy-related, by DHS coding?

The DHS records one coded item, `sib.died.pregnant` (`mm9`), plus a
time-since-delivery band, `sib.time.delivery.death` (`mm12`).

## Usage

``` r
is_preg_related_dhs(sib_df, na.action)
```

## Arguments

- sib_df:

  the prepped sibling dataset

- na.action:

  how to treat a missing `sib.time.delivery.death` when the sibling died
  after a delivery: `"include"` counts her, `"exclude"` does not. See
  [add_maternal_deaths](http://dennisfeehan.org/siblingsurvival/reference/add_maternal_deaths.md)

## Value

a logical vector, one entry per row of `sib_df`
