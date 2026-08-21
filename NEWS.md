# siblingsurvival 0.3.0.9000 (development)

## MICS support

* `add_maternal_deaths()` gained `style` and `na.action`. `style` selects the
  questionnaire coding -- `"dhs"` (the default), `"mics6"` (also MICS7) or
  `"mics4"` (also MICS5) -- and the classification rules are factored into
  `is_preg_related_dhs()`, `is_maternal_dhs()`, `is_preg_related_mics()` and
  `is_maternal_mics()`. Everything else, including the `-1` sentinel, the `NA`
  fill and the male blanking, is shared. Existing DHS call sites are unaffected.
* **`na.action` has no default for the MICS styles.** It decides whether a
  sister who died within two months of the end of a pregnancy, but whose day
  count is missing, falls inside the 42-day maternal window -- a choice about
  the estimand rather than a coding detail, so `add_maternal_deaths()` errors
  with an explanation instead of picking one. It moves only
  `sib.maternal.death.date`, never `sib.preg_related.death.date`. For
  `style = "dhs"` it defaults to `"include"`, which is what this package has
  always done.
* The MICS classification respects three questionnaire skip patterns that are
  easy to get wrong: a childbirth death (`MM23 = 1`) is unconditionally
  maternal even though `MM26`/`MM27` are `NA` by design; sisters who died before
  age 12 are routed past the maternity items and must not be swept in; and male
  siblings get `NA` rather than `FALSE`.

* Added `prep_mics_sib_histories()`, which prepares a MICS maternal mortality
  file (`mm.sav`) for analysis. MICS publishes the sibling history with **one
  row per reported sibling**, unlike the DHS wide women's file, so no reshape is
  needed. The function constructs the columns MICS does not supply -- `caseid`
  from cluster/household/line, `doi` from `WDOI` or from year and month,
  respondent `age` from `(WDOI - WDOB)/12`, `psu` from the cluster, and `sex`
  -- then delegates to the same internals the DHS path uses.
* Added `sibhist_varmap_mics4`, `sibhist_varmap_mics5`, `sibhist_varmap_mics6`
  and `sibhist_varmap_mics7`. MICS4/5 use a roster numbering of `MM5`--`MM13`;
  MICS6 renumbered to `MM15`--`MM27` and added the violence and accident items,
  so MICS6/7 support maternal mortality while MICS4/5 support pregnancy-related
  mortality only -- the same split as DHS phases 2--6 versus 7+.
* MICS codes survival status as 1 yes / 2 no / 8 don't know, while this package
  (following the DHS) expects 1 alive / 0 dead. `prep_mics_sib_histories()`
  recodes it. Passing the MICS codes through unchanged would make every dead
  sibling look like missing survival status and silently drive every mortality
  estimate to zero.
* `prep_mics_sib_histories()` refuses to run a varmap that maps `mm16` to
  `sib.died.accident`. `MM16` means opposite things in the two systems: "Is
  (name) still alive?" in MICS6, "died of violence or an accident" in DHS
  phase 7 and later.
* `get_sib_df()` gained a `reshape` argument, so the same function serves the
  wide DHS layout and the long MICS layout.
* `get_sib_df()` now treats `sib.dob` and `sib.death.date` as **derived** rather
  than required. They are used when the varmap supplies them (`MM17C`/`MM18C` in
  MICS6, `MM7C`/`MM8C` in MICS4/5, `mm4`/`mm8` in the DHS) and approximated from
  reported ages and years-since-death otherwise. Some surveys ship no CMC
  columns at all.

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

* `prep_dhs_sib_histories()` and `prep_nrsim_sib_histories()` gained a
  `weight.scale` argument. `get_ego_df()` used to divide any column named
  `wwgt` by `1e6` whenever it was present, announcing "assuming we have a DHS
  survey". That is right for the DHS, which publishes women's weights multiplied
  by 1,000,000, and wrong by six orders of magnitude for anything else.
  `prep_dhs_sib_histories()` keeps `1e6` as its default; see Bug fixes for
  `prep_nrsim_sib_histories()`.
* `get_ego_df()` and `get_sib_df()` now check for the columns they require and
  error with a message naming the missing ones, rather than failing inside a
  `mutate()` or `case_when()` with (for example) `object 'age' not found`.
  `get_ego_df()` requires `age` and `survey`; `get_sib_df()` requires `caseid`,
  `wwgt`, `psu`, `doi` and `sex` on the ego data, and `sib.sex`, `sib.alive`,
  `sib.age`, `sib.dob`, `sib.death.date`, `sib.death.yrsago` and
  `sib.death.age` on the siblings. The `doi` message notes that it has to be a
  CMC (century month code), since the date derivations are arithmetic in months.
* `aggregate_maternal_estimates()` gained optional `age_prop` and `vis_res`
  arguments. Both are computed internally when not supplied, as before. Callers
  that need the respondent age distribution or the visibility results for their
  own output, or that call this function more than once per survey, can now
  compute them once and pass them in. Results are unchanged either way, and
  there is a test asserting that.

* `get_ego_age_distn(only_females = FALSE)` now returns a **separate** age
  distribution for each respondent sex: the result gains a `sex` column and
  `agegrp_prop` sums to 1 *within* each sex. Previously it pooled the sexes into
  a single distribution with no `sex` column, which
  `aggregate_maternal_estimates()` could not join against.
  `only_females = TRUE`, the default and by far the common case, is unchanged.

## Documentation

* Added the vignette "Working with MICS sibling history data", covering which
  MICS rounds carry a usable sibling history, what the MICS prep does that the
  DHS path does not, and the results of validating the package against the
  published tables of three MICS6 surveys. It records two conventions that MICS
  documents leave unstated -- the seven-year reference window is
  `[doi - 84, doi)`, and age standardisation uses the interviewed women -- and
  one that is actively mislabelled: the column headed "Maternal Deaths" in table
  TM.9.3 of MICS reports contains the **pregnancy-related** count.

## Bug fixes

* Sibling reports with no usable date of birth are now dropped along with those
  missing sex or survival status, and counted in `summ$miss.dob`. They cannot be
  placed in an age group, so they contribute neither exposure nor events -- but
  left in, a single one turned an entire exposure cell into `NA`, since the
  estimator sums over the cell. Found on Madagascar 2018, where 13 living
  siblings have neither a reported age nor an imputed date of birth.
* `prep_mics_sib_histories()` now recodes MICS's 98 (don't know) and 99 (no
  response) values to `NA` on the numeric sibling items -- `sib.age`,
  `sib.death.yrsago`, `sib.death.age`, `sib.days.postpartum.death` and
  `sib.num.children`. Passed through as real values these are silently
  catastrophic: a sibling with `sib.death.age = 98` gets a date of birth 98
  years before her death. The damage is masked whenever MICS supplies its own
  imputed CMC dates, so it only bites on surveys that ship none.
* Fixed `cell_config()`, which rejected a custom `time.periods` object built by
  `make.time.periods()` -- the documented usage, and how `age.groups` already
  behaved. The non-character branch called `stop("No time periods specified.")`
  unconditionally, so only the three built-in strings worked. This blocked
  testing alternative reference windows.
* Fixed `aggregate_maternal_estimates()`, whose bootstrap branch joined the
  visibility results on age alone while the point-estimate branch joined on age
  *and* sex. When the respondents include both sexes, every bootstrap row
  matched twice, so the bootstrap estimates -- and therefore the confidence
  intervals -- came out inflated by exactly the number of sexes present. With
  bootstrap weights set equal to the real weights, which must reproduce the
  point estimate exactly, the bootstrap mean was 2x the point estimate. This was
  masked in practice because DHS respondents are all female, so
  `ego_vis_agg` has a single sex and the duplication does not fire. Callers doing
  this join themselves should check for the same missing key.
* Fixed `aggregate_maternal_estimates(only_females = FALSE)`, which errored
  outright with `Join columns in 'y' must be present in the data`. Three defects
  were stacked in that one branch: it joined `age_prop` on a `sex` column that
  `get_ego_age_distn()` never produced; it then grouped by `sex`, which the join
  consumes into `sib.sex`; and it removed a `dummy` column that its grouping
  never created. It now groups by `sib.sex`, and results are reported per
  sibling sex.
* `aggregate_maternal_estimates(only_females = FALSE)` warns when a sibling sex
  has no respondents of that sex, naming the sexes involved, instead of silently
  returning `NA`. A reference age distribution and a visibility adjustment can
  only come from respondents of the same sex, so for the usual survey that
  interviews only women, male sibling estimates are `NA` -- which is the honest
  answer, but should not be silent.
* `prep_nrsim_sib_histories()` no longer divides weights by `1e6`. Its
  `weight.scale` defaults to `1`, on the grounds that weights outside the DHS
  are typically already normalized to average 1. Previously any varmap mapping a
  weight to `wwgt` -- which every varmap must, since everything downstream
  expects that name -- had its weights silently divided by a million.
  **This changes results** for existing uses of `prep_nrsim_sib_histories()`;
  pass `weight.scale = 1e6` to restore the old behaviour.
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
* Added `tests/testthat/test_weight_scale.R` covering weight scaling in both
  prep functions, the new required-column guards, and that
  `aggregate_maternal_estimates()` returns identical results whether `age_prop`
  and `vis_res` are computed internally or supplied.
* Added `tests/testthat/test_maternal_aggregation.R` covering the bootstrap join
  (with bootstrap weights equal to the real weights, so the replicate mean must
  reproduce the point estimate), the `only_females = FALSE` path, the warning for
  an uninterviewed sex, and the per-sex `get_ego_age_distn()` output.

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
