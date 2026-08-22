# prepare a MICS dataset for analysis

prepare a MICS dataset for analysis

## Usage

``` r
prep_mics_sib_histories(
  mm.df,
  survey,
  wm.df = NULL,
  varmap = sibhist_varmap_mics6,
  id.vars = c("hh1", "hh2", "ln"),
  doi.var = "wdoi",
  doi.ym = c("wm6y", "wm6m"),
  dob.var = "wdob",
  lowercase = TRUE,
  weight.scale = 1,
  add_maternal = FALSE,
  style = c("mics6", "mics4"),
  na.action = NULL,
  preg.window = c("2months", "42days"),
  keep_missing = FALSE,
  keep_varmap_only = FALSE,
  verbose = TRUE
)
```

## Arguments

- mm.df:

  the MICS maternal mortality file (`mm.sav`), one row per reported
  sibling

- survey:

  string identifying the survey, eg `"ZW2019"`. Required: MICS has no
  `v000` equivalent, so this cannot be derived; see Details

- wm.df:

  optional women's file (`wm.sav`), joined on `id.vars` to bring across
  respondent attributes that `mm.df` does not carry

- varmap:

  see Details; defaults to `sibhist_varmap_mics6`

- id.vars:

  columns of `mm.df` that together identify a respondent. Combined to
  form `caseid`

- doi.var:

  column holding the date of interview as a CMC. Used when present;
  otherwise `doi.ym` is used

- doi.ym:

  year and month columns from which to construct a CMC date of
  interview, when `doi.var` is absent

- dob.var:

  column holding the respondent's date of birth as a CMC, used to derive
  respondent age

- lowercase:

  lowercase all variable names before matching the varmap? See Details

- weight.scale:

  divide the weight by this number. Defaults to `1`, since MICS weights
  are already normalized; see Details

- add_maternal:

  should maternal/pregnancy-related death info be added?

- style:

  which MICS roster coding the data use: `"mics6"` (the default, also
  correct for MICS7) or `"mics4"` (also correct for MICS5). Only used
  when `add_maternal = TRUE`

- na.action:

  required when `add_maternal = TRUE`; see
  [add_maternal_deaths](http://dennisfeehan.org/siblingsurvival/reference/add_maternal_deaths.md)

- preg.window:

  width of the postpartum window for the pregnancy-related column;
  `"42days"` reproduces published MICS tables. See
  [add_maternal_deaths](http://dennisfeehan.org/siblingsurvival/reference/add_maternal_deaths.md)

- keep_missing:

  should we keep reported sibs that are missing sex or survival status?

- keep_varmap_only:

  should we only keep ego variables mentioned in the varmap?

- verbose:

  report detailed summaries?

## Value

a list; see Details

## Details

MICS publishes the sibling history as a dedicated file with **one row
per reported sibling**, unlike the DHS, which publishes a wide women's
file with one column per sibling attribute per sibling. So no reshape is
needed, and this function does not call
[attributes.to.long](http://dennisfeehan.org/siblingsurvival/reference/attributes.to.long.md).

`survey` has no default because MICS has no `v000` equivalent. Supplying
it explicitly keeps MICS survey ids comparable with the DHS codes.

`weight.scale` defaults to `1` because MICS weights (`wmweight`) are
already normalized to average 1. Do not pass `1e6` here: that is a DHS
convention.

MICS `.sav` files use **mixed** case – questionnaire items and link keys
are uppercase (`MM15`, `HH1`, `WDOI`) while derived and design variables
are lowercase (`wmweight`, `psu`, `welevel`) – so `lowercase = TRUE`
normalizes them and every shipped MICS varmap is written in lowercase.

Several columns the rest of the pipeline requires are not in every MICS
file and are constructed here when absent:

- `caseid` – from `id.vars`; MICS has no single respondent id

- `doi` – from `doi.var`, else `doi.ym`. Must end up a CMC

- `age` – respondent age, from `(doi - dob.var) / 12`

- `psu` – from the first of `id.vars` (the cluster) when no `psu` column
  exists, which is the case in most MICS surveys

- `sex` – constant `'f'`; MICS interviews only women

`sib.dob` and `sib.death.date` are used when the varmap supplies them
(`MM17C`/`MM18C` in MICS6, `MM7C`/`MM8C` in MICS4/5) and approximated
from reported ages and years-since-death otherwise.

`varmap` has the same format as the DHS varmaps: columns `orig.varname`,
`new.varname` and `sibvar`.

Returns a list whose entries include

- `ego.dat` - dataset with information about the survey respondents

- `sib.dat` - dataset with information about the reported siblings

- `summ` - a one-row tibble with a summary of the data

## Examples

``` r
  # TODO - write example code
```
