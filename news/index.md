# Changelog

## siblingsurvival 0.3.0

### New features

- Added
  [`add_maternal_deaths()`](http://dennisfeehan.org/siblingsurvival/reference/add_maternal_deaths.md)
  to classify sibling deaths as pregnancy-related or maternal based on
  DHS coding variables (`sib.died.pregnant`, `sib.time.delivery.death`,
  `sib.died.accident`).
- Added
  [`aggregate_maternal_estimates()`](http://dennisfeehan.org/siblingsurvival/reference/aggregate_maternal_estimates.md)
  to aggregate age-specific maternal mortality estimates across age
  groups using the respondent age distribution as weights. Supports both
  point estimates and bootstrap confidence intervals.
- Added
  [`get_ego_age_distn()`](http://dennisfeehan.org/siblingsurvival/reference/get_ego_age_distn.md)
  (internal) to compute the weighted age distribution of survey
  respondents, used as the reference population for aggregation.
- Added
  [`prep_nrsim_sib_histories()`](http://dennisfeehan.org/siblingsurvival/reference/prep_nrsim_sib_histories.md)
  to prepare non-DHS (NR-SIM style) sibling history data, alongside the
  existing
  [`prep_dhs_sib_histories()`](http://dennisfeehan.org/siblingsurvival/reference/prep_dhs_sib_histories.md).
- Improved bootstrap performance in
  [`sibling_estimator()`](http://dennisfeehan.org/siblingsurvival/reference/sibling_estimator.md)
  via matrix multiplication
  ([`get_boot_ests_matrix()`](http://dennisfeehan.org/siblingsurvival/reference/get_boot_ests_matrix.md)),
  replacing a wide-dataframe approach and achieving substantial
  speed-ups for large bootstrap replicate counts.

### Bug fixes

- Fixed a grouping bug in
  [`sibling_estimator()`](http://dennisfeehan.org/siblingsurvival/reference/sibling_estimator.md)
  that caused an error (`Column '.ego.id' doesn't exist`) when using
  dplyr \< 1.1.0. The `summarise(across(...))` calls introduced in the
  dplyr 1.0 migration were missing `.groups = "drop"`, so the result
  remained grouped by `.ego.id`. This residual grouping propagated
  through `pivot_wider` and
  [`purrr::map_dfr`](https://purrr.tidyverse.org/reference/map_dfr.html)
  into
  [`get_ec_reports()`](http://dennisfeehan.org/siblingsurvival/reference/get_ec_reports.md),
  where a subsequent `group_by(across(all_of(...)))` failed because
  dplyr 1.0.x evaluates
  [`across()`](https://dplyr.tidyverse.org/reference/across.html) in a
  mutate context that cannot select already-active grouping variables.
  Fixed by adding `.groups = "drop"` to the `summarise` in
  [`occ.exp()`](http://dennisfeehan.org/siblingsurvival/reference/occ.exp.md)
  and
  [`get_ec_reports()`](http://dennisfeehan.org/siblingsurvival/reference/get_ec_reports.md),
  and adding a defensive
  [`ungroup()`](https://dplyr.tidyverse.org/reference/group_by.html)
  before the `group_by` in
  [`get_ec_reports()`](http://dennisfeehan.org/siblingsurvival/reference/get_ec_reports.md).
  The bug was most visible when calling
  [`sibling_estimator()`](http://dennisfeehan.org/siblingsurvival/reference/sibling_estimator.md)
  inside
  [`purrr::imap_dfr()`](https://purrr.tidyverse.org/reference/map_dfr.html).
- Fixed
  [`aggregate_maternal_estimates()`](http://dennisfeehan.org/siblingsurvival/reference/aggregate_maternal_estimates.md),
  which was accidentally referencing package-level example objects
  (`ex.ego`, `ex.sib`) instead of the `ego.dat` and `sib.dat` arguments
  passed by the caller.
- Fixed
  [`attributes.to.long()`](http://dennisfeehan.org/siblingsurvival/reference/attributes.to.long.md):
  replaced defunct
  [`dplyr::select_()`](https://dplyr.tidyverse.org/reference/defunct-lazyeval.html),
  [`mutate_()`](https://dplyr.tidyverse.org/reference/defunct-lazyeval.html),
  and
  [`filter_()`](https://dplyr.tidyverse.org/reference/defunct-lazyeval.html)
  (deprecated in dplyr 0.7, now defunct) with modern equivalents
  (`select(all_of(...))`, `mutate(across(...))`,
  [`filter()`](https://dplyr.tidyverse.org/reference/filter.html)).
  Removed `lazyeval` from package `Imports`. This unblocked
  `devtools::build()`, which was failing when building vignettes.

### Tests

- Added regression tests for the grouping bug:
  [`sibling_estimator()`](http://dennisfeehan.org/siblingsurvival/reference/sibling_estimator.md)
  returns ungrouped data frames, works correctly when called via
  [`purrr::imap_dfr()`](https://purrr.tidyverse.org/reference/map_dfr.html),
  and handles column names containing dots (e.g. `ego.id`, `sib.id`).
- Added `tests/testthat/test_maternal.R` with tests covering
  [`add_maternal_deaths()`](http://dennisfeehan.org/siblingsurvival/reference/add_maternal_deaths.md),
  [`get_ego_age_distn()`](http://dennisfeehan.org/siblingsurvival/reference/get_ego_age_distn.md),
  and
  [`aggregate_maternal_estimates()`](http://dennisfeehan.org/siblingsurvival/reference/aggregate_maternal_estimates.md)
  (including bootstrap paths).
- Added `tests/testthat/test_prep_dhs_sib_histories.R` with smoke tests
  for
  [`prep_dhs_sib_histories()`](http://dennisfeehan.org/siblingsurvival/reference/prep_dhs_sib_histories.md).

### Repository

- Renamed default branch from `master` to `main`.

## siblingsurvival 0.1.0

- Added a `NEWS.md` file to track changes to the package.
- Created website for package using
  [`pkgdown`](https://pkgdown.r-lib.org/)
