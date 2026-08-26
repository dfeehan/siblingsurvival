# the reproductive age groups used for maternal mortality estimates

Maternal and pregnancy-related mortality quantities are conventionally
defined over women of reproductive age, 15 to 49. This function is the
single definition of those age groups, so that the respondent age
distribution
([get_ego_age_distn](http://dennisfeehan.org/siblingsurvival/reference/get_ego_age_distn.md))
and the age-specific estimates being aggregated
([aggregate_maternal_estimates](http://dennisfeehan.org/siblingsurvival/reference/aggregate_maternal_estimates.md))
cannot drift apart.

## Usage

``` r
reproductive_age_groups()
```

## Value

a character vector with the seven 5-year age group labels covering ages
15 through 49

## Details

The labels match the ones produced by `cut(..., right=FALSE)` and by
[make.even.age.groups](http://dennisfeehan.org/networkreporting/reference/make.even.age.groups.md),
which is what both the ego age categories (`age.cat`) and the estimate
age groups (`sib.age`) use.

## Examples

``` r
  reproductive_age_groups()
#> [1] "[15,20)" "[20,25)" "[25,30)" "[30,35)" "[35,40)" "[40,45)" "[45,50)"
```
