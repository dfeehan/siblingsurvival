# calculate total rate based on point estimates

calculate total rate based on point estimates

## Usage

``` r
aggregate_maternal_estimates(estimates, ego.dat, sib.dat, only_females = TRUE)
```

## Arguments

- estimates:

  the output of
  [sibling_estimator](http://dennisfeehan.org/siblingsurvival/reference/sibling_estimator.md)

- ego.dat:

  the prepped ego data

- sib.dat:

  the prepped sibling data

- only_females:

  only keep female estimates? Defaults to TRUE; see Details, below

## Value

either a tibble with the estimates aggregated across age groups OR, if
there are bootstrap results, then a list with three entries:

- `point` - the point estimates (should be the same as running w/out
  bootstraps)

- `boot_summ` - estimates w/ confidence intervals calculated from
  bootstrap reps

- `boot` - results for each bootstrap rep

## Details

This function aggregates age-specific maternal mortality quantities like
the maternal death rate or the pregnancy-related death rate. It uses the
age distribution of the survey respondents as the reference population.

Note that, by default, this will only consider estimates for females
aged 15-49, as this is what makes sense for aggregating maternal
estimates. It might occasionally be useful to run this function for
all-cause mortality; in that case, setting the parameter
`only_female=FALSE` will include male estimates, too (but will still
restrict to ages 15-49)
