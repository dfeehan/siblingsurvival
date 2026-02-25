# take a prepared DHS sibling dataset and add maternal death info

take a prepared DHS sibling dataset and add maternal death info

## Usage

``` r
add_maternal_deaths(sib_df, keep_missing = FALSE, verbose = TRUE)
```

## Arguments

- sib_df:

  the prepped DHS dataset (probably from
  [prep_dhs_sib_histories](http://dennisfeehan.org/siblingsurvival/reference/prep_dhs_sib_histories.md))

- keep_missing:

  should we keep reported sibs that are missing sex or survival status?

- verbose:

  report detailed summaries?

## Value

a dataframe with columns `sib.preg_related.death.date` and
`sib.maternal.death.date` added

## Details

This function checks to see if there is a column called
`sib.died.accident`. If so, then it is possible to estimate whether or
not each death is maternal; the resulting death dates are in the column
`sib.maternal.death.date`.

For siblings, you should be sure to include

- `sib.death.date` (the date of the sibling's death)

- `sib.alive` (whether or not the sib is alive)

- `sib.sex` (the sex of the sibling).

Returns a dataframe TODO

## Examples

``` r
  # TODO - write example code
```
