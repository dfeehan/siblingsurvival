# warn when a sibling sex has no matching respondents

Called from
[aggregate_maternal_estimates](http://dennisfeehan.org/siblingsurvival/reference/aggregate_maternal_estimates.md)
when `only_females = FALSE`. In most sibling history surveys only women
are interviewed, so there is no respondent age distribution and no
visibility adjustment for male siblings, and their estimates come out
`NA`. That is the honest answer – you cannot estimate a visibility
adjustment for a sex that was never interviewed – but it should not be
silent.

## Usage

``` r
warn_uninterviewed_sex(res, ego.dat)
```

## Arguments

- res:

  the joined estimate dataframe

- ego.dat:

  the prepped ego data, used to report which sexes were interviewed

## Value

`res`, unchanged; called for the warning
