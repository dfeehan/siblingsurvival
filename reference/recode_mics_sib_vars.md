# recode MICS sibling variables to the conventions the package expects

MICS and the DHS code survival status differently, and the difference is
silent: MICS `MM16` is 1 yes / 2 no / 8 don't know, while the DHS `mm2`
is 1 alive / 0 dead. Everything downstream filters on
`sib.alive %in% c(0,1)`, so passing MICS codes through unchanged drops
**every dead sibling** as though its survival status were missing –
which silently drives every mortality estimate to zero.

## Usage

``` r
recode_mics_sib_vars(df, verbose = TRUE)
```

## Arguments

- df:

  the renamed MICS sibling data

- verbose:

  report the recode?

## Value

`df` with `sib.alive` recoded, and `sib.sex` validated
