# attributes.to.long

Start with a wide-form dataframe reported about alters using network
method questions and convert it into a long-form dataset. For example,
after a network survey of out-migrants, there might be variables about
sex and age of each emigre reported to be connected to each respondent.
In a study that encountered a maximum of 3 reported emigres across all
respondents, this wide-form dataframe might look like:  

|         |            |         |         |         |         |         |         |     |
|---------|------------|---------|---------|---------|---------|---------|---------|-----|
| resp.id | resp.d.hat | emage.1 | emage.2 | emage.3 | emsex.1 | emsex.2 | emsex.3 | 1   |
| 100     | 24         | NA      | NA      | M       | NA      | NA      | 2       | 110 |
| NA      | NA         | NA      | NA      | NA      | NA      | 3       | 140     | 33  |

The `attributes.to.long` function could convert that into a long-form
dataframe that looks like this:  

|        |     |     |
|--------|-----|-----|
| degree | age | sex |
| 100    | 24  | M   |
| 140    | 33  | F   |
| 140    | 23  | M   |
| 140    | 53  | F   |
|        | ... |     |

(Note that we make no guarantees about the order in which the reshaped
data will be returned.)  

## Usage

``` r
attributes.to.long(
  df,
  attribute.prefix,
  ego.vars = NULL,
  keep.na = FALSE,
  idvar = NULL,
  sep = "\\.|_",
  varname.first = TRUE
)
```

## Arguments

- df:

  the wide-form dataset to convert

- attribute.prefix:

  a vector whose entries have the prefixes of the names of variables in
  the dataframe `data` that pertain to each alter. if you'd like these
  to be re-named in the long form data, then the variable names you'd
  like to use in the long form should be the names of each entry in this
  vector. in the example above, we would use
  `attribute.prefix=c("age"="emage", "sex"="emsex")`. see `regexp`,
  below, to understand how these prefixes are used to match columns of
  the dataset; by default, we assume that the variables match \<either
  '.' or '\_'\>.

- ego.vars:

  if not NULL, the names of columns in the dataset that refer to the
  egos and so should not be converted to long-form. you can specify that
  they should be renamed in the same way as with `attribute.prefix`. in
  the example above, we would use `ego.vars=c("degree"="resp.d.hat")`.

- keep.na:

  if FALSE, remove columns in the resulting dataset that are all NAs

- idvar:

  the index or name of the variable in the data that has the respondent
  id. if NULL, then new ids which correspond to the rows in data are
  created.

- sep:

  a regular expression that the wide-form variable names are split
  around. (eg, for "var_01", sep="\_"; for "var.01" is is "\\")

- varname.first:

  TRUE if the text before the separator is the variable name (eg
  var_01), and FALSE otherwise

## Value

a long-form dataframe with the attributes reported for all of the
alters. the dataframe will include an alter.id variable which is formed
using .

## TODO

- should follow the se / nse pattern in, eg, the kp functions; interim
  workaround – eg `as.data.frame(df)[,idvar]` – for now

- for now, this converts any factors into characters. this is obviously
  not ideal. eventually, data type should be preserved...

- handle the case of "" more effectively. Right now, we *assume* that
  all structural missings (eg, I only report one alter, though there are
  three columns for me to do so) are NA

- look at the code in the middle of the function that's commented out
  and be sure we know that the order of the rows will be the same, to
  that we can cbind them together.

## Examples

``` r
if (FALSE) { # \dontrun{
   ## TODO add example
} # }
```
