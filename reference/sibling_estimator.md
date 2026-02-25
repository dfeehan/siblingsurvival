# Estimate death rates from sibling history data

Estimate death rates from sibling history data

## Usage

``` r
sibling_estimator(
  sib.dat,
  ego.id,
  sib.id,
  sib.frame.indicator,
  sib.sex = "sex",
  cell.config,
  weights,
  boot.weights = NULL,
  return.boot = FALSE,
  discretize.exp = FALSE
)
```

## Arguments

- sib.dat:

  The long-form sibling history dataset (likely produced by
  [prep_dhs_sib_histories](http://dennisfeehan.org/siblingsurvival/reference/prep_dhs_sib_histories.md))

- ego.id:

  String with the name of the column of `sib.dat` that has the ID of the
  survey respondent

- sib.id:

  String with the name of the column of `sib.dat` that has the sibling
  ID

- sib.frame.indicator:

  String with the name of the column in `sib.dat` containing a 0/1 coded
  variable indicating whether or not each sib is in the frame population

- sib.sex:

  String with the name fo the column of `sib.dat` that has the sibling's
  sex

- cell.config:

  An object containing the configuration of cells; see TODO for more
  information

- weights:

  String with the name of the column of `sib.dat` that has the sampling
  weight

- boot.weights:

  Optional dataframe with bootstrap resampled weights. See Details for
  more info.

- return.boot:

  If TRUE, and if `boot.weights` is specified, then return each
  bootstrap estimate

- discretize.exp:

  Boolean for whether or not expsoure should be discretized. Not yet
  implemented.

## Value

a list with two entries: `asdr.ind`, individual visibility asdr
estimates; and `asdr.agg`, aggregate visibility asdr estimates

## Details

If you want estimated sampling variances, you can pass in a data frame
`boot.weights`. `boot.weights` is assumed to have a column that is named
whatever the `ego.id` is, and then a series of columns named
`boot_weight_1`, ..., `boot_weight_M`.
