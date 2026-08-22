# shared tail of the sibling-history prep functions

Summarises a prepped ego/sibling pair, optionally drops sibling reports
that are missing survival status or sex, and assembles the returned
list. Shared by
[prep_dhs_sib_histories](http://dennisfeehan.org/siblingsurvival/reference/prep_dhs_sib_histories.md),
[prep_nrsim_sib_histories](http://dennisfeehan.org/siblingsurvival/reference/prep_nrsim_sib_histories.md)
and
[prep_mics_sib_histories](http://dennisfeehan.org/siblingsurvival/reference/prep_mics_sib_histories.md)
so that the three preps cannot drift apart.

## Usage

``` r
finalize_sib_prep(
  ego.dat,
  sib.dat,
  cur.survey,
  miss_col,
  resp.attrib,
  keep_missing = FALSE,
  keep_varmap_only = FALSE,
  verbose = TRUE
)
```

## Arguments

- ego.dat:

  the prepped ego dataset

- sib.dat:

  the prepped sibling dataset

- cur.survey:

  the survey id

- miss_col:

  the list returned by
  [`check_varmap_cols()`](http://dennisfeehan.org/siblingsurvival/reference/check_varmap_cols.md)

- resp.attrib:

  named vector of ego variables from the varmap, used by
  `keep_varmap_only`

- keep_missing:

  see
  [prep_dhs_sib_histories](http://dennisfeehan.org/siblingsurvival/reference/prep_dhs_sib_histories.md)

- keep_varmap_only:

  see
  [prep_dhs_sib_histories](http://dennisfeehan.org/siblingsurvival/reference/prep_dhs_sib_histories.md)

- verbose:

  see
  [prep_dhs_sib_histories](http://dennisfeehan.org/siblingsurvival/reference/prep_dhs_sib_histories.md)

## Value

a list with entries `survey`, `ego.dat`, `sib.dat` and `summ`

## Details

Unless `keep_missing = TRUE`, three kinds of sibling report are dropped:
those with unknown survival status, those with unknown sex, and those
with no usable date of birth or no sampling weight. The last cannot be
placed in an age group, so they contribute neither exposure nor events –
and left in, a single one turns an entire exposure cell into `NA`, since
the estimator sums over the cell.
