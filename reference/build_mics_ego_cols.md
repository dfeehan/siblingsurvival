# construct the respondent-level columns MICS does not supply

construct the respondent-level columns MICS does not supply

## Usage

``` r
build_mics_ego_cols(
  df,
  survey,
  id.vars = c("hh1", "hh2", "ln"),
  doi.var = "wdoi",
  doi.ym = c("wm6y", "wm6m"),
  dob.var = "wdob",
  verbose = TRUE
)
```

## Arguments

- df:

  the (renamed) MICS sibling file

- survey:

  the survey id

- id.vars:

  columns identifying a respondent

- doi.var:

  CMC date-of-interview column, used when present

- doi.ym:

  year and month columns, used when `doi.var` is absent

- dob.var:

  CMC respondent date-of-birth column

- verbose:

  report what was constructed?

## Value

`df` with `caseid`, `survey`, `doi`, `age`, `psu` and `sex` added
