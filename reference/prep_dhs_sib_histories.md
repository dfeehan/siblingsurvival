# prepare a DHS dataset for analysis

prepare a DHS dataset for analysis

## Usage

``` r
prep_dhs_sib_histories(
  df,
  varmap = sibhist_varmap_dhs6,
  add_maternal = FALSE,
  keep_missing = FALSE,
  keep_varmap_only = FALSE,
  verbose = TRUE
)
```

## Arguments

- df:

  the raw DHS dataset (indvidual recode)

- varmap:

  see Details

- add_maternal:

  should maternal/pregnancy-related death info be added? (Default:
  FALSE)

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
dataset.

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

The default varmap is `sibhist_varmap_dhs6`, which is included with the
package.

Returns a list whose entries include

- `ego.dat` - dataset with information about the survey respondents

- `sib.dat` - dataset with information about the reported siblings

- `summ` - a one-row tibble with a summary of the data

## Examples

``` r
  # TODO - write example code
```
