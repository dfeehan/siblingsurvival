# Fast bootstrap estimation using matrix multiplication

Replaces the wide-dataframe summarize_at + gather/spread approach with
direct matrix multiplication. For each cell, computes weighted sums
across all M bootstrap replicates simultaneously using BLAS routines,
avoiding the creation of 10k-column intermediate dataframes.

## Usage

``` r
get_boot_ests_matrix(
  ec_dat,
  boot_weights_df,
  ego_id_col,
  cell_vars,
  estimator_type
)
```

## Arguments

- ec_dat:

  ego X cell data from get_ec_reports()

- boot_weights_df:

  dataframe with .ego.id column and boot_weight_1..M columns

- ego_id_col:

  name of the ego id column in ec_dat and boot_weights_df

- cell_vars:

  vector of column names defining cells (age, sex, time period, etc)

- estimator_type:

  either 'ind' (individual visibility) or 'agg' (aggregate visibility)

## Value

long-form data frame with one row per cell per bootstrap replicate
