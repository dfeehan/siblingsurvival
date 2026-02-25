# prepare a dataset from nrsimulatr for sibling analysis

prepare a dataset from nrsimulatr for sibling analysis

## Usage

``` r
prep_nrsim_sib_histories(
  df,
  varmap,
  keep_missing = FALSE,
  keep_varmap_only = FALSE,
  verbose = TRUE
)
```

## Arguments

- df:

  the raw dataset (each row is a survey response)

- varmap:

  see Details; defaults to NULL

- keep_missing:

  should we keep reported sibs that are missing sex or survival status?

- keep_varmap_only:

  should we only keep ego variables mentioned in the varmap? (Default:
  FALSE)

- verbose:

  report detailed summaries?

## Value

a list; see Details

## Details

This function is similar to
[prep_dhs_sib_histories](http://dennisfeehan.org/siblingsurvival/reference/prep_dhs_sib_histories.md),
but it is not customized to work with DHS survey data.

Note that if the dataframe does not have a column called 'sex', then one
will be added, and we will assume respondents are all female (sex='f').
If you want to avoid this, only pass in a dataframe after adding the
'sex' column.

`varmap` should be a dataframe with columns

- `orig.varname` (the raw variable name)

- `new.varname` (the new variable name)

- `sibvar` (a 0/1 column, with 1 meaning this is a sibling variable and
  0 meaning an ego variable)

Each row of `varmap` describes a variable to rename from the original
dataset. Note that you MUST include `varmap`; at a minimum, it is needed
to show which columns are siblings...

For respondents, you should be sure to include

- `survey` (the survey id, usually a country code plus one digit)

- `caseid` (the respondent id)

- `wwgt` (the sampling weight for women)

- `psu` (the primary sampling unit)

- `doi` (the date of the interview)

For siblings, you should be sure to include

- `sib.death.date` (the date of the sibling's death)

- `sib.alive` (whether or not the sib is alive)

- `sib.sex` (the sex of the sibling, coded 'f' or 'm').

Returns a list whose entries include

- `ego.dat` - dataset with information about the survey respondents

- `sib.dat` - dataset with information about the reported siblings

- `summ` - a one-row tibble with a summary of the data

## Examples

``` r
  # TODO - write example code
```
