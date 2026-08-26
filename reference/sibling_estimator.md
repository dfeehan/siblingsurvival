# Estimate death rates from sibling history data

Estimate death rates from sibling history data

## Usage

``` r
sibling_estimator(
  sib.dat,
  ego.id,
  sib.id = "sibid",
  sib.frame.indicator,
  sib.sex = "sex",
  cell.config,
  weights,
  boot.weights = NULL,
  return.boot = FALSE,
  visibility = networkreporting::vis_from_clique(),
  tie = networkreporting::tie_config("clique", name = "siblings"),
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
  ID. Defaults to `'sibid'`, which is the column created by
  [`prep_dhs_sib_histories`](http://dennisfeehan.org/siblingsurvival/reference/prep_dhs_sib_histories.md)
  and
  [`prep_nrsim_sib_histories`](http://dennisfeehan.org/siblingsurvival/reference/prep_nrsim_sib_histories.md).

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

- visibility:

  A visibility rule saying how each reported sibling's visibility is
  derived. Defaults to
  [`networkreporting::vis_from_clique()`](http://dennisfeehan.org/networkreporting/reference/vis_from_clique.md),
  the exact rule this function has always applied, so the default
  changes nothing. See
  [`networkreporting::vis_from_donor()`](http://dennisfeehan.org/networkreporting/reference/vis_from_donor.md)
  and
  [`networkreporting::vis_coalesce()`](http://dennisfeehan.org/networkreporting/reference/vis_coalesce.md)
  for the approximating rules that non-clique ties need.

- tie:

  What kind of tie the reports are about, as a
  [`networkreporting::tie_config()`](http://dennisfeehan.org/networkreporting/reference/tie_config.md).
  Defaults to `tie_config("clique", name = "siblings")`, which is what
  siblings are, so the default changes nothing.

         **Set this if you are using this function for a tie that is not a
         clique.** Whether a roster is a clique is a fact about how it was
         built, not something the data reveals: applied to a roster that is
         not one, the clique rule still returns a plausible number. On a
         socsim roster pooling maternal and paternal cousins it overstates
         visibility by 1.089x for off-frame alters against 1.061x for
         on-frame ones, and since a death is always off-frame while exposure
         is a mixture, that differential biases the rate rather than
         cancelling out of it.

         Worth knowing which way the surprise runs: maternal cousins *alone*
         are a clique, since everyone sharing a maternal grandmother forms an
         equivalence class, and the rule is exact for them. It is the union of
         the two lines that is not.

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

## See also

[`networkreporting::network_survival_estimator()`](http://dennisfeehan.org/networkreporting/reference/network_survival_estimator.md),
which this wraps
