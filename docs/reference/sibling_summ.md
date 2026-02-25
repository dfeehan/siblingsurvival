# calculate summary statistics for siblings in a given time window

calculate summary statistics for siblings in a given time window

## Usage

``` r
sibling_summ(sib.dat, ego.id, sib.id, sib.frame.indicator, cell.config)
```

## Arguments

- sib.dat:

  The long-form sibling dataset (likely produced by
  `prep_dhs_sib_histories`)

- ego.id:

  String with the name of the column in `sib.dat` containing the survey
  respondent ID

- sib.id:

  String with the name of the column of `sib.dat` that has the sibling
  ID

- sib.frame.indicator:

  String with the name of the column in `sib.dat` containing a 0/1 coded
  variable indicating whether or not each sib is in the frame population

## Value

A one-row tibble with two columns: `num_sibs` has the number of sibs who
contribute exposure in the time window; and `num_deaths`, the number of
reported sibling deaths in the time window
