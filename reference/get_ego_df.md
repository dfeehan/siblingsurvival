# helper to prep the ego dataset

Shared by
[prep_dhs_sib_histories](http://dennisfeehan.org/siblingsurvival/reference/prep_dhs_sib_histories.md),
[prep_nrsim_sib_histories](http://dennisfeehan.org/siblingsurvival/reference/prep_nrsim_sib_histories.md)
and
[prep_mics_sib_histories](http://dennisfeehan.org/siblingsurvival/reference/prep_mics_sib_histories.md).

## Usage

``` r
get_ego_df(df, resp.attrib, verbose = FALSE, weight.scale = 1e+06)
```

## Arguments

- df:

  the survey dataset

- resp.attrib:

  vector with respondent attribute columns (see
  [prep_dhs_sib_histories](http://dennisfeehan.org/siblingsurvival/reference/prep_dhs_sib_histories.md))

- verbose:

  see
  [prep_dhs_sib_histories](http://dennisfeehan.org/siblingsurvival/reference/prep_dhs_sib_histories.md)

- weight.scale:

  divide the `wwgt` column by this number. DHS weights are published
  multiplied by 1,000,000, so `1e6` recovers weights that average 1;
  surveys whose weights are already normalized (MICS, and simulated
  data) should pass `1`. See
  [prep_dhs_sib_histories](http://dennisfeehan.org/siblingsurvival/reference/prep_dhs_sib_histories.md)

## Value

a prepped ego dataset, used by the `prep_*_sib_histories()` functions
