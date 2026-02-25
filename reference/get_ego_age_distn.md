# get_ego_age_distribution

get_ego_age_distribution

## Usage

``` r
get_ego_age_distn(ego.dat, only_females = TRUE)
```

## Arguments

- ego.dat:

  the ego dataset (probably from
  [prep_dhs_sib_histories](http://dennisfeehan.org/siblingsurvival/reference/prep_dhs_sib_histories.md))

- only_females:

  should only females be used to calculate age distribution? (default:
  True)

## Value

dataframe with distribution of respondent ages by 5-year category,
typically used in calculating the maternal or pregnancy-related
mortality

## Details

`ego_dat` is assumed to have two columns: `wwgt` and `age.cat`
