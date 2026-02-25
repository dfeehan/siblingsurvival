# calculate visibility for each sibship

calculate visibility for each sibship

## Usage

``` r
get_sibship_visibility(sib.dat, ego.id, sib.frame.indicator)
```

## Arguments

- sib.dat:

  The long-form sibling dataset (likely produced by
  [prep_dhs_sib_histories](http://dennisfeehan.org/siblingsurvival/reference/prep_dhs_sib_histories.md))

- ego.id:

  String with the name of the column in `sib.dat` containing the survey
  respondent ID

- sib.frame.indicator:

  String with the name of the column in `sib.dat` containing a 0/1 coded
  variable indicating whether or not each sib is in the frame population

## Value

A tibble with a row for each survey respondent (each unique value of
`ego.id`), and the number of sibs the respondent reported on the frame,
including and not including herself

## Examples

``` r
  # TODO write example code
```
