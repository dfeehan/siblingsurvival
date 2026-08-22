# guard against the MM16 collision between MICS and the DHS

In MICS6 `MM16` is "Is (name) still alive?"; in DHS-VII and later `mm16`
is "died of violence or an accident". Mapping one onto the other would
silently reclassify survival status as cause of death, so refuse to run.

## Usage

``` r
check_mics_varmap(varmap)
```

## Arguments

- varmap:

  the varmap to check

## Value

`varmap`, invisibly; called for the error
