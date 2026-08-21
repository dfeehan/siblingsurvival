# siblingsurvival 0.3.0.9000 (development)

## New features

* `get_ego_age_distn()` is now exported. It was already documented and used by
  `aggregate_maternal_estimates()`, but callers who wanted the respondent age
  distribution for their own age-specific output had to reach for `:::`.
* Added `reproductive_age_groups()`, an exported accessor giving the seven
  5-year age groups covering ages 15-49. This is now the single definition used
  by both `get_ego_age_distn()` and `aggregate_maternal_estimates()`, which
  previously each carried their own copy of the age-group list.
* `sibling_estimator()` now defaults `sib.id` to `'sibid'`, which is the sibling
  id column that `prep_dhs_sib_histories()` and `prep_nrsim_sib_histories()`
  create. Callers that pass `sib.id` explicitly are unaffected.
* `sibling_estimator()` now checks up front that the columns named by `ego.id`,
  `sib.id`, `sib.frame.indicator`, `sib.sex` and `weights` exist in `sib.dat`,
  and errors with a message naming both the arguments at fault and the columns
  that are actually present. Previously a mismatched name produced an opaque
  tidyselect error deep in the call stack.
* `prep_dhs_sib_histories()` and `prep_nrsim_sib_histories()` now report
  *sibling* variables from the varmap that are missing from the dataset, not
  just ego variables. Sibling variables are matched as prefixes (`mm3` matches
  `mm3_01`, `mm3_02`, ...), using the same regular expression as
  `attributes.to.long()`. The `summ` tibble gained a `sib.cols.notfound` column
  alongside the existing `ego.cols.notfound`.

## Bug fixes

* Fixed the derivation of `sib.dob` from a sibling's age at death in
  `get_sib_df()`. The condition was guarded on `sib.death.yrsago` but the
  approximation is computed from `sib.death.age`, so a sibling with a known age
  at death but no years-since-death got a silent `NA` birth date. Now guarded on
  `sib.death.age`.
* Fixed a misplaced parenthesis in `attributes.to.long()`
  (`length(intersect(...) > 0)` rather than `length(intersect(...)) > 0`) in the
  check for overlapping ego and alter variable names. The check happened to
  behave correctly, but only by accident.

## Tests

* Added `tests/testthat/test_prep_cleanup.R` covering the export of
  `get_ego_age_distn()`, `reproductive_age_groups()` (including that it is
  equivalent to the exclusion filter it replaced for the standard `'5yr'` age
  groups), the missing-sibling-variable reporting, the `sib.dob` derivation
  regression, and the `sibling_estimator()` `sib.id` default and error message.

# siblingsurvival 0.3.0

## New features

* Added `add_maternal_deaths()` to classify sibling deaths as pregnancy-related
  or maternal based on DHS coding variables (`sib.died.pregnant`,
  `sib.time.delivery.death`, `sib.died.accident`).
* Added `aggregate_maternal_estimates()` to aggregate age-specific maternal
  mortality estimates across age groups using the respondent age distribution as
  weights. Supports both point estimates and bootstrap confidence intervals.
* Added `get_ego_age_distn()` (internal) to compute the weighted age distribution
  of survey respondents, used as the reference population for aggregation.
* Added `prep_nrsim_sib_histories()` to prepare non-DHS (NR-SIM style) sibling
  history data, alongside the existing `prep_dhs_sib_histories()`.
* Improved bootstrap performance in `sibling_estimator()` via matrix
  multiplication (`get_boot_ests_matrix()`), replacing a wide-dataframe approach
  and achieving substantial speed-ups for large bootstrap replicate counts.

## Bug fixes

* Fixed a grouping bug in `sibling_estimator()` that caused an error
  (`Column '.ego.id' doesn't exist`) when using dplyr < 1.1.0. The
  `summarise(across(...))` calls introduced in the dplyr 1.0 migration were
  missing `.groups = "drop"`, so the result remained grouped by `.ego.id`.
  This residual grouping propagated through `pivot_wider` and `purrr::map_dfr`
  into `get_ec_reports()`, where a subsequent `group_by(across(all_of(...)))`
  failed because dplyr 1.0.x evaluates `across()` in a mutate context that
  cannot select already-active grouping variables. Fixed by adding
  `.groups = "drop"` to the `summarise` in `occ.exp()` and `get_ec_reports()`,
  and adding a defensive `ungroup()` before the `group_by` in
  `get_ec_reports()`. The bug was most visible when calling
  `sibling_estimator()` inside `purrr::imap_dfr()`.
* Fixed `aggregate_maternal_estimates()`, which was accidentally referencing
  package-level example objects (`ex.ego`, `ex.sib`) instead of the `ego.dat`
  and `sib.dat` arguments passed by the caller.
* Fixed `attributes.to.long()`: replaced defunct `dplyr::select_()`,
  `mutate_()`, and `filter_()` (deprecated in dplyr 0.7, now defunct) with
  modern equivalents (`select(all_of(...))`, `mutate(across(...))`,
  `filter()`). Removed `lazyeval` from package `Imports`. This unblocked
  `devtools::build()`, which was failing when building vignettes.

## Tests

* Added regression tests for the grouping bug: `sibling_estimator()` returns
  ungrouped data frames, works correctly when called via `purrr::imap_dfr()`,
  and handles column names containing dots (e.g. `ego.id`, `sib.id`).
* Added `tests/testthat/test_maternal.R` with tests covering
  `add_maternal_deaths()`, `get_ego_age_distn()`, and
  `aggregate_maternal_estimates()` (including bootstrap paths).
* Added `tests/testthat/test_prep_dhs_sib_histories.R` with smoke tests for
  `prep_dhs_sib_histories()`.

## Repository

* Renamed default branch from `master` to `main`.

# siblingsurvival 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Created website for package using [`pkgdown`](https://pkgdown.r-lib.org/)
