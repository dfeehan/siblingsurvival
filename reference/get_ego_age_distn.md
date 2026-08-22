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

dataframe with distribution of respondent ages by 5-year category.
Columns `age.cat`, `total` and `agegrp_prop`, plus `sex` when
`only_females = FALSE`; see Details

## Details

`ego_dat` is assumed to have the columns `wwgt`, `age.cat` and `sex`

The age groups used are those returned by
[reproductive_age_groups](http://dennisfeehan.org/siblingsurvival/reference/reproductive_age_groups.md).

When `only_females = TRUE` (the default), respondents are restricted to
females and a single age distribution is returned, with `agegrp_prop`
summing to 1 across age groups.

When `only_females = FALSE`, a **separate** age distribution is returned
for each respondent sex: the result gains a `sex` column, and
`agegrp_prop` sums to 1 *within* each sex. This is what
[aggregate_maternal_estimates](http://dennisfeehan.org/siblingsurvival/reference/aggregate_maternal_estimates.md)
needs in order to weight each sex's age-specific rates by its own
respondents' age structure.
