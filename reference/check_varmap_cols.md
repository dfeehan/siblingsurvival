# report varmap columns that are missing from a dataset

In some cases, variables in the varmap will not be in the specific
dataset being prepared (for example, some DHS surveys don't have the
'literacy' variable, v155). In those cases we report the missing columns
and proceed.

## Usage

``` r
check_varmap_cols(df, resp.attrib, sib.attrib, sep = "\\.|_", verbose = TRUE)
```

## Arguments

- df:

  the survey dataset

- resp.attrib:

  named vector of ego (respondent) variables from the varmap

- sib.attrib:

  named vector of sibling variables from the varmap

- sep:

  regular expression separating the sibling variable prefix from the
  sibling number; must match the `sep` passed to
  [attributes.to.long](http://dennisfeehan.org/siblingsurvival/reference/attributes.to.long.md)

- verbose:

  print a message describing the missing columns?

## Value

a list with entries `ego` and `sib`, each a (possibly empty) named
vector of varmap columns that were not found in `df`

## Details

Ego (respondent) variables are matched by exact name. Sibling variables
are matched as *prefixes*, since the wide-form data has one column per
reported sibling: the varmap entry `mm1` corresponds to columns `mm1_1`,
`mm1_2`, and so on. The regular expression used here has to match the
one used by
[attributes.to.long](http://dennisfeehan.org/siblingsurvival/reference/attributes.to.long.md),
which is what actually reshapes those columns.
