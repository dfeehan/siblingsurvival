Changes needed in `maternal-mortality` after the `siblingsurvival` MICS work
====

For a session working in **`~/Dropbox/maternal-mortality/maternal-mortality`**,
not in the package repo.

Written 2026-08-21 against `siblingsurvival` branch `mics`. Everything described
here is already done on the package side; this file is the list of things the
analysis repo has to change to keep working, plus the things the package work
learned that change the analysis plan.

Install the branch first:

    devtools::install_github("dfeehan/siblingsurvival", ref = "mics")

Read `MICS-PLAN.md` and `vignettes/mics-data.Rmd` in the package repo for the
background. This file only lists actions.


A. Breaking changes — these will error or silently change results
----

### A1. `get_ego_age_distn(only_females = FALSE)` returns a different shape

**Where:** `code/R/estimate.R:129`, and the join at `:135`.

    respondent_age <- siblingsurvival:::get_ego_age_distn(ego, only_females = FALSE)
    ...
      dplyr::left_join(vis_df$ego_vis_agg, by = c("sib.age" = "age.cat")) %>%
      dplyr::left_join(respondent_age,     by = c("sib.age" = "age.cat")) %>%

`get_ego_age_distn(only_females = FALSE)` now returns a **`sex` column**, with
`agegrp_prop` summing to 1 *within* each sex rather than across the whole table.
`only_females = TRUE` is unchanged.

Two consequences at this call site:

1. The `left_join` on `age.cat` alone will now duplicate rows, once per sex.
2. It will also pull in a `sex` column that collides with the `sex` already
   arriving from `vis_df$ego_vis_agg` one line above, giving `sex.x` / `sex.y`.

**Fix:** since the code filters to `sib.sex == "f"` two lines later anyway, the
simplest correction is to ask for females only:

    respondent_age <- siblingsurvival::get_ego_age_distn(ego, only_females = TRUE)

If the two-sex distribution really is wanted, join on both keys instead:

    dplyr::left_join(respondent_age, by = c("sib.age" = "age.cat", "sib.sex" = "sex"))

### A2. `get_ego_age_distn()` and `get_visibility()` are exported now

**Where:** `code/R/estimate.R:128-129`.

Drop the `:::`:

    siblingsurvival:::get_visibility(...)      ->  siblingsurvival::get_visibility(...)
    siblingsurvival:::get_ego_age_distn(...)   ->  siblingsurvival::get_ego_age_distn(...)

### A3. `add_maternal_deaths()` gained `style` and `na.action`

DHS call sites are **unaffected** — `style` defaults to `"dhs"` and `na.action`
defaults to `"include"`, which is what the package has always done. No change
needed for the existing DHS pipeline.

For MICS data `na.action` is **required** and has no default; see C3.

### A4. Expect the numbers to move slightly

Several fixes on the package side change DHS results, all small but real:

* `aggregate_maternal_estimates()` had a bootstrap branch that joined visibility
  on age alone while the point branch joined on age *and* sex. With single-sex
  respondents — which is every DHS — this made no difference, so **DHS point
  estimates and CIs should not move**. Mentioned only so it is not a surprise if
  something does.
* `get_ego_age_distn()` and `aggregate_maternal_estimates()` now use a shared
  `reproductive_age_groups()`, converting an *exclusion* of the three
  post-reproductive groups into an *inclusion* of the seven reproductive ones.
  For every built-in `cell_config()` age grouping these are identical, and there
  is a test pinning that, so **no result should change**.
* A `sib.dob` derivation was guarded on the wrong variable and produced silent
  `NA` birth dates for siblings with a known age at death but no
  years-since-death. **This can change DHS results**, in the direction of
  recovering exposure that was previously lost.

If the individual/aggregate comparison has reversed sign since June 2025 — the
open question in `STATUS.md` — re-running now is the way to test it.


B. Cleanups
----

### B1. Use `reproductive_age_groups()` instead of the third hard-coded copy

**Where:** `code/R/estimate.R:137`.

    dplyr::filter(!sib.age %in% c("[50,55)", "[55,60)", "[60,65)"))

becomes

    dplyr::filter(sib.age %in% siblingsurvival::reproductive_age_groups())

This was one of three copies of the same demographic definition across two
repositories; the two in the package are now a single exported accessor.

### B2. The visibility join is missing a sex key

**Where:** `code/R/estimate.R:134`.

    dplyr::left_join(vis_df$ego_vis_agg, by = c("sib.age" = "age.cat"))

`ego_vis_agg` is keyed on `(sex, age.cat)`. Joining on age alone duplicates every
row once per sex present. It is benign today because DHS respondents are all
female, so there is only one sex — but it is the exact bug that was found and
fixed inside the package, where it silently doubled every bootstrap estimate.

**Fix:**

    dplyr::left_join(vis_df$ego_vis_agg, by = c("sib.age" = "age.cat", "sib.sex" = "sex"))

### B3. `sib.id` no longer needs to be passed

`sibling_estimator()` now defaults `sib.id = "sibid"`, which is what the prep
functions create, and errors with a message naming the available columns if a
column is not found. The workaround noted in `STATUS.md` can be simplified or
left as-is; it is no longer load-bearing.


C. The MICS plan needs revising
----

This is the substantive part. The package work invalidated several assumptions
the MICS audit was built on.

### C1. ⚠ The survey shortlist does not survive

**None of Benin 2021, Gambia 2018, Malawi 2019 or Sierra Leone 2017 fielded the
maternal mortality module.** The shortlist was built from a survey *catalogue*,
on country and round, without checking whether MM was actually collected — and
MM is an optional module. Verified directly: Gambia 2018's `wm.sav` has 409
variables and zero `MM`-prefixed ones.

Of **229** MICS surveys examined, only **13** have a usable sibling roster:

| Round | Surveys |
|---|---|
| MICS4 | BTN_2010, MRT_2011 |
| MICS5 | BEN_2014, COG_2014, GIN_2016, GNB_2014, MWI_2013, STP_2014, ZWE_2014 |
| MICS6 | COM_2022, IRQ_2018, MDG_2018, ZWE_2019 |

The full inventory, with per-survey detail, is `data-raw/mics-inventory.csv` in
the package repo.

**But the design is recoverable.** Benin and Malawi both appear one round
earlier, as `BEN_2014` and `MWI_2013`, so the within-country DHS↔MICS comparison
survives by moving to MICS5. Rebuild the shortlist from the inventory rather
than from the catalogue.

Two traps if rebuilding an inventory independently:

* **MICS3's `mm.sav` is the *men's* file**, not maternal mortality — it contains
  `MM4` "Man's Line number" and `mmweight` "men's sample weight". Globbing for
  `mm.sav` picks up three false positives.
* Surveys that did not field the module do **not** hide the roster in `wm.sav`.
  `mm.sav` presence is the definitive test.

### C2. ⚠ The draft varmap has the wrong numbering

**Where:** `mics_varmaps/sibhist_varmap_mics6_DRAFT.csv`, and the generator at
`code/mics-audit/02-build-item-crosswalk.R`.

The draft maps `mm5 -> sib.sex`, `mm6 -> sib.alive`, `mm7 -> sib.age`,
`mm8 -> sib.death.yrsago`, `mm9 -> sib.death.age`. That is **MICS4/5**
numbering. MICS6 renumbered the roster to `MM15`–`MM19`. The draft's
`mm12/13/14` for the pregnancy items match neither scheme: MICS4/5 uses
`MM10/11/12`, MICS6 uses `MM22/23/24`.

The `MM16` note in the crosswalk is also wrong. It records `MM16` as MICS loop
control; in MICS6 loop control is `MM28`, and **`MM16` is "Is (name) still
alive?"**. Mapping it to the DHS meaning would turn survival status into cause
of death.

**Action:** the package now ships `sibhist_varmap_mics4`, `_mics5`, `_mics6` and
`_mics7`, with names verified against real `mm.sav` files and the World Bank
catalog dictionaries. **Use those and delete the draft**, or regenerate the
crosswalk to agree with them.

### C3. ⚠ MICS6 supports maternal mortality after all

The audit concluded MICS has no violence/accident item and so supports only
pregnancy-related mortality. That is true of MICS4/5 but **false for MICS6/7**,
which ask `MM26` (violence) and `MM27` (accident). MICS6 can produce both
estimands, and because `MM25` records days postpartum, the older two-month
pregnancy-related series can be reconstructed for comparability with DHS.

So the round-to-estimand mapping is:

| | Estimand | DHS analogue |
|---|---|---|
| MICS4/5 | Pregnancy-related only | DHS 2–6 |
| MICS6/7 | Maternal *and* pregnancy-related | DHS 7+ |

**Decision needed for the paper:** using pregnancy-related throughout maximises
comparability across DHS phases and MICS rounds and is now clearly achievable,
but discards MICS6's better-defined maternal measure. Worth stating explicitly
either way.

`na.action` is required when calling `add_maternal_deaths()` on MICS data. It
decides whether a sister who died within two months but whose day count is
missing falls inside the 42-day maternal window. It affects only the maternal
column, never the pregnancy-related one, and across three MICS6 surveys it moved
5 of 38 such deaths in Iraq and none in Zimbabwe or Madagascar.

### C4. ⚠ Published MICS tables report a 42-day pregnancy-related count

**This matters for any comparison against published figures.** MICS reports print
a table headed *Maternal mortality* with a column labelled "Maternal Deaths" and
a footnote defining maternal as excluding accidents and violence. UNICEF's own
tabulation syntax says otherwise. It flags a death with

    if (MM15 = 2 & (MM22 = 1 or MM23 = 1 or (MM24 = 1 and MM25 < 42))) md = 1.

and never reads `MM26` (violence) or `MM27` (accident) anywhere in the file. So
the published column is a **pregnancy-related** count on a **42-day** window ---
which is the WHO definition of a pregnancy-related death.

To reproduce it:

    add_maternal_deaths(..., style = "mics6", preg.window = "42days")

and compare `sib.preg_related.death.date`. On Iraq 2018 that gives 64.4 against a
published 64; the two-month default gives 67.7 and the maternal column 57.0.

Note the default is still `preg.window = "2months"`, so nothing moves unless you
ask for it. But be aware that the DHS side of this package has *always* applied a
42-day cut (via the `mm12` band `100`--`141`), so for a DHS↔MICS comparison
`"42days"` is the consistent choice and `"2months"` is not. **This is a decision
to make deliberately for the paper.**

### C4b. A don't-know age at death no longer drops a sister

A guard in the MICS classification required a *known* age at death of 12 or over
before the maternity items were read, to respect the `MM21` skip that routes
sisters who died under 12 past `MM22`--`MM25`. But `MM19 = 98` ("don't know")
becomes `NA` in the prep, so the guard also dropped sisters who had answered the
maternity questions affirmatively --- which is itself proof they were asked. It
now excludes only sisters *known* to have died under 12.

This affected 7 pregnancy-related deaths in Iraq 2018 and 5 in Zimbabwe 2019.
MICS only; no DHS result moves.

### C5. Data access is no longer a blocker

A local archive of 229 MICS surveys is at
`~/Google Drive/My Drive/_2023_maternal/data/mics/MICS_Datasets.zip`, as nested
per-survey zips. Nothing in the MICS plan is now waiting on registration.

Also useful: the World Bank microdata catalog publishes full variable
dictionaries **without registration**, so a survey's `mm.sav` contents can be
checked before downloading anything.


D. Two conventions the validation settled
----

Both were open questions in the audit; both are now answered empirically against
published tables, and both are already implemented in the package.

* **The seven-year window is `[doi - 84, doi)`** — years since death 0 to 6, not
  1 to 7. This is what `cell_config(time.periods = '7yr_beforeinterview')`
  already does.
* **Age standardisation uses the age distribution of the interviewed women**,
  which is what `get_ego_age_distn()` computes — not household women, as IUSSP
  and MMEIG describe.

The package reproduces published MICS estimates closely on four surveys across
both roster schemes and three countries.

A literal R transcription of the official syntax now lives at
`data-raw/mics-validation/spss-syntax-replica.R` in the package repo. It
reproduces Iraq 2018 and Madagascar 2018 **to the person-year**, including the
general fertility rate and the maternal mortality ratio.

Zimbabwe 2019 is the exception. Running the official syntax on its own public
microdata reproduces its **male** columns exactly and misses its **female** ones
(exposure 0.997, all-cause deaths 0.988, pregnancy-related deaths 0.896). Since
both sexes go through identical code, that pattern cannot come from the
calculation. The report also contradicts itself: its sampling-error appendix
gives a maternal mortality ratio of 413.64 against 462 in TM.9.3, and it is the
appendix that we reproduce (409.9). **Treat Zimbabwe 2019's published female
mortality figures as unreliable, and do not use them to validate anything.**
Zimbabwe *2014* reproduces closely, so it is not a country-level problem.


E. Suggested order
----

1. **A1, A2, B1, B2** — small, mechanical, and A1 will otherwise error. Do these
   first and re-run the DHS pipeline.
2. **A4** — compare the re-run against the cached results and record what moved.
   This is also the test of the open individual/aggregate sign question.
3. **C1** — rebuild the survey shortlist from `data-raw/mics-inventory.csv`.
   Decide which of the 13 surveys enter the analysis, and which have a DHS
   counterpart in the same country.
4. **C2** — delete or regenerate the draft varmap.
5. **C3** — decide the estimand for the paper.
6. Then the MICS analysis proper, using `prep_mics_sib_histories()`.
