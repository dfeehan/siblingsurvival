# get_ego_age_distribution

get_ego_age_distribution

## Usage

``` r
get_ego_age_distn(ego.dat, only_females = TRUE, warn.single.sex = TRUE)
```

## Arguments

- ego.dat:

  the ego dataset (probably from
  [prep_dhs_sib_histories](http://dennisfeehan.org/siblingsurvival/reference/prep_dhs_sib_histories.md))

- only_females:

  should only females be used to calculate age distribution? (default:
  True)

- warn.single.sex:

  when `only_females = FALSE`, warn if `ego.dat` holds only one
  respondent sex, since the result then cannot standardise rates for the
  other. Set `FALSE` only when the caller reports the problem itself, as
  [aggregate_maternal_estimates](http://dennisfeehan.org/siblingsurvival/reference/aggregate_maternal_estimates.md)
  does

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

### There is no male age distribution in DHS data, and you do not need one

`only_females = FALSE` splits whatever respondents are in `ego.dat`; it
does not conjure a sex that was never interviewed. DHS sibling histories
come from the women's file, so a DHS `ego.dat` is entirely female and
`only_females = FALSE` returns the female distribution with a `sex`
column attached, and warns.

It would be reasonable to conclude from `Chap16_AM/AM_rates.do` that
this blocks reproducing a published male rate, since its
`get_age_distributions` takes the men's age distribution from the men's
recode (`MR`) file. **In practice it does not.** Published DHS reports
standardise *both* sexes by the age distribution of the survey
respondents — the women — which is exactly what `only_females = TRUE`
returns here.

Checked against four published tables spanning DHS phases 4 to 8:

|                |                     |           |
|----------------|---------------------|-----------|
| Survey         | male rate, this way | published |
| Malawi 2000    | 11.064              | 11.1      |
| Rwanda 2005    | 7.393               | 7.39      |
| Rwanda 2014-15 | 2.961               | 2.96      |
| Gambia 2019-20 | 3.133               | 3.13      |

Standardising men by a male distribution built from the `MR` file
instead gives 11.162, 7.285 and 2.881 for the first three — further from
the published figures in every case.
