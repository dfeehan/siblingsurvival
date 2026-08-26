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

### A0. ⚠⚠ The DHS pregnancy-related count changes, by a lot

**Regenerate every cached DHS result.** This is not a small correction.

`is_preg_related_dhs()` was requiring `mm9` in 2--5 **and** `mm12` in the band
`100`--`141`. The DHS Program's own tabulation code
(`DHS-Indicators-Stata`, `Chap16_AM/AM_rates.do:725`) counts `mm9 >= 2 & mm9 <= 6`
and states that "mm12 is not needed". Code 6 is "between six weeks and two
months of a delivery" --- inside the two-month window, and the reason this is the
*pregnancy-related* rather than the *maternal* quantity.

Validated against Rwanda 2010 (`FR259` Table 16.4): the package gave 51.2
pregnancy-related deaths against a published 91, and now gives 90.7, matching a
literal replica of the DHS reference in every age group. Exposure and all-cause
deaths were already exact and do not move.

**The correction is not uniform across surveys**, which matters because the paper
compares across them. Whether a survey codes postpartum deaths as 5 or 6 is a
property of its questionnaire, so the old loss ranged from nothing to about 44%:

| Survey | old | corrected | lost before |
|---|---|---|---|
| MWIR22FL 1992 | 67.7 | 67.7 | none |
| BJIR31FL 1996 | 31.7 | 59.3 | 47% |
| MWIR41FL 2000 | 237.7 | 344.3 | 31% |
| RWIR53FL 2005 | 109.9 | 180.3 | 39% |
| RWIR61FL 2010 | 67.3 | 129.3 | 48% |
| RWIR70FL 2014 | 31.9 | 56.4 | 43% |
| GMIR81FL 2019 | 68.5 | 71.7 | 4% |

(seven-year window, weighted, all ages 15--49)

`sib.maternal.death.date` does **not** change. Note also that maternal is only
computable for 5 of the 43 surveys and is itself distorted where a questionnaire
used only code 6 --- see H1b in `DHS-VALIDATION-PLAN.md`. Pregnancy-related is
computable for all 43 and is insensitive to that, which is a further reason to
make it the paper's estimand.

### A0b. Every estimate moves slightly: the event boundary convention changed

`window_intersect()` treated observation windows as `(start, end]`; it now treats
them as `[start, end)`. The old form disagreed with the exposure calculation at
the first month of a window --- a death there contributed exposure but could not
be counted as an event.

Effect is small but systematic and in one direction (a few more deaths counted).
Three of seven validation surveys were each missing exactly one death, always in
month `doi - 84`. With this fixed, **all seven reproduce the DHS reference
exactly**.

Nothing to change in the analysis code, but it is another reason to regenerate
cached results alongside A0.

If any analysis code builds `sib.dat` by hand rather than via the prep
functions, note that `end.obs` for a sibling who died must be `death + 1`, not
`death`. The prep functions already do this.

### A0c. Male rates change slightly in 13 surveys; two data caveats

Three further findings from running all 43 surveys against the DHS reference.

**Male exposure was inflated in 13 of 43 surveys.** `get_sib_df()` turned every
sex code that was not 2 into male, so "don't know" (8) and an unlabelled 9 both
became men --- Gabon 2000 has 163 such siblings. Up to 0.7% of male exposure.
**Female results do not move**, so nothing about pregnancy-related or maternal
mortality changes; but any adult-male mortality figures should be regenerated.

**Burkina Faso 2003 cannot contribute pregnancy-related estimates at all.** Its
`mm9` column exists but all 249,540 values are missing, so the count is exactly
zero. It is the only such survey. The package now warns; previously the zero was
silent, and a zero rate reads as a finding. **Decide explicitly whether BFIR43FL
stays in the sample.**

**Gabon 2000 needs `encoding = "latin1"`** --- see B4.

### A0d. Four DHS conventions are now explicit options

All default to what The DHS Program does, so **nothing moves unless you ask**
--- except the first, which is a correction.

* **Maternal deaths.** `is_maternal_dhs()` now follows the reference exactly
  (`mm9` 2--5, `mm16` not 1 or 2). Only affects the 5 surveys with `mm16`, and
  only South Africa 2016 actually changes, by 3 deaths. Not relevant if the
  paper uses pregnancy-related.
* **`prmr.accident.recode`** on `add_maternal_deaths()`: applies the 2016 PRMR
  redefinition (a death during pregnancy reported as violence or accident stops
  counting). Default `FALSE` = what published tables reflect. **Worth an
  explicit decision if the paper says anything about post-2016 PRMR
  comparability.**
* **`death.exposure = c("dhs", "mics")`** on all three prep functions: whether a
  sibling who died contributes the month of death. Default `"dhs"`, unchanged
  behaviour. Relevant if you ever want DHS and MICS on a single convention ---
  they genuinely differ, and this is the knob.
* **`nmx_to_nqx()` and `q15_to_50()`** are new exported helpers with `nax`
  defaulting to 2.6, which is what both DHS and MICS use. **If the analysis
  repo computes 35q15 anywhere with 2.5, it disagrees with both published
  sources**; switch to these. Validated against Gambia 2019-20.

### A0e. All-cause adult mortality validates too --- and male rates are reproducible

Independent of the maternal work: the package reproduces published DHS *all-cause*
adult mortality on five surveys spanning phases 4 to 8. **Exposure matches to the
person-year in every cell, both sexes** (70 cells); deaths match to rounding.

The useful practical finding is about standardisation. **Published DHS reports
standardise both sexes by the age distribution of the survey respondents** ---
i.e. `get_ego_age_distn(only_females = TRUE)` --- not by a sex-specific
distribution. Male rates come out right this way:

| Survey | ours | published |
|---|---|---|
| Malawi 2000 | 11.064 | 11.1 |
| Rwanda 2005 | 7.393 | 7.39 |
| Rwanda 2014-15 | 2.961 | 2.96 |
| Gambia 2019-20 | 3.133 | 3.13 |

So if the analysis reports adult male mortality anywhere, use the respondents'
age distribution for it. Using a male distribution from the men's `MR` file ---
which is what the current `AM_rates.do` does --- moves *away* from the published
figures.

Also note **Rwanda 2010's published summary rows are unreliable**: its
age-adjusted rates match the crude rates rather than the standardised ones,
despite the footnote, while its age-specific cells reproduce exactly. Third
independent sign of trouble in that report. Do not use its totals as a target.

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


B4. Gabon 2000 cannot be read with `read_dta()` defaults
----

`data/dhs/GAIR41FL.DTA` fails with *"Unable to convert string to the requested
encoding (invalid byte sequence)"*. It is one of the 43 surveys in
`out/survey-index.rds`, so whatever the pipeline currently does with it, it is
not estimating from it.

`haven::read_dta(path, encoding = "latin1")` reads it fine (3,361 variables).
`"ISO-8859-1"` also works; `"windows-1252"` does not.

Worth checking whether the pipeline is silently skipping it, or erroring, or
whether the 43 has quietly been 42 all along.


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
