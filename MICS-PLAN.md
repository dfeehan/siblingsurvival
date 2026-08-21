Plan: MICS support in `siblingsurvival`
====

Working plan for adding MICS surveys alongside DHS. Split out of
`PACKAGE-HANDOFF.md` (section B) once sections A, C, D and E were done, so that
this file is the single place the MICS work is tracked.

Revised 2026-08-21 against `notes/mics-maternal-mortality-reference.md`, which is
far better sourced than the earlier feasibility audit — verbatim questionnaires,
the actual `mm.sav` data dictionaries, and published tables verified
arithmetically. **Where the reference and the audit crosswalk disagree, the
reference wins**, and several things below reverse what the first draft of this
plan said. Those reversals are marked ⚠.

Every code claim was checked against the source or demonstrated by running it.


What changed from the first draft
----

Four corrections, all from the methods reference. They are listed up front
because each invalidates something the earlier plan asserted.

| ⚠ | First draft said | Actually |
|---|---|---|
| 1 | MICS supports pregnancy-related mortality only, having no violence/accident item | **MICS6/7 have `MM26` (violence) and `MM27` (accident) and report *true maternal* deaths.** MICS6 can produce *both* estimands |
| 2 | The roster layout is unknown; write the wide path first | **Answered: long.** A dedicated `mm.sav`, one row per sibling. `attributes.to.long()` is not needed for MICS at all |
| 3 | MICS has no CMC dates, so `sib.dob` / `sib.death.date` must be derived | **`mm.sav` ships `MM17C` and `MM18C`, imputed CMC birth and death dates, plus `WDOI`.** Map them directly; derive only as a fallback, since some countries omit them |
| 4 | Only MICS6 is in scope | **MICS4 and MICS5 are also usable**, with different variable numbering. MICS2/3 are not usable at all |

And one correction to the draft varmap itself, item M5 below: its variable
numbers are wrong for MICS6.


Which MICS rounds are usable
----

MICS has run **two structurally different** mortality modules under the same
"MM" label. Only one of them is a sibling survival history.

| | MICS2 / MICS3 | MICS4 / MICS5 | MICS6 / MICS7 |
|---|---|---|---|
| Method | Indirect (summary) sisterhood | **Direct sibling history** | **Direct sibling history** |
| **Usable here?** | **No** | **Yes** | **Yes** |
| Questionnaire | Household | Individual Women | Individual Women |
| Respondents | All adults 15+, both sexes, proxy allowed | Women 15–49 | Women 15–49 |
| Brothers enumerated? | No — sisters only | Yes | Yes |
| Roster numbering | — | `MM4`–`MM14` | `MM14`–`MM28` |
| Data file | inside `hl.sav` | women's file | dedicated **`mm.sav`** |
| Reference period | ~10–14 yrs | 7 years | 7 years |
| Estimand | — | Pregnancy-related, 2 months, **no** cause exclusion | **Maternal**, 42 days, **excl.** violence/accidents |

**MICS2/3 are out.** Summary sisterhood counts, asked in the household
questionnaire, sisters only, no per-sibling roster, no ages at death. There is
nothing for this estimator to consume — supporting them would mean implementing
a different method, not a different varmap.

**MICS4/5 and MICS6/7 map cleanly onto the DHS phases the package already
handles**, which is the useful framing:

| | Estimand | Package analogue |
|---|---|---|
| DHS 2–6 | Pregnancy-related | `sib.died.accident` absent → fallback branch |
| DHS 7+ | Maternal | `sib.died.accident` present |
| **MICS4/5** | Pregnancy-related | like DHS 2–6 |
| **MICS6/7** | Maternal | like DHS 7+ |

So the package should ship `sibhist_varmap_mics4`, `_mics5`, `_mics6` and
`_mics7`, mirroring the seven DHS varmaps, and `add_maternal_deaths()` needs a
MICS branch that keys off which items are present rather than off the round
number.

**For a DHS↔MICS comparison, use pregnancy-related throughout.** MICS6 collects
`MM25` (days after end of pregnancy) and asks `MM24` on a two-month window, so
the older pregnancy-related series is fully reconstructible from MICS6
microdata — which is what makes the two instruments comparable. See M6.


State of play
----

**The estimator applies.** MICS4+ collects a full both-sex per-sibling roster —
`[S1]` oldest through `[S8]`, with `MM28` as loop control in MICS6. Verified
against the Zimbabwe MICS 2019 women's questionnaire (Appendix E, pp. 495–500),
the IPUMS transcription of the MICS6 model questionnaire, and the MICS7
standalone form.

**The module is optional, not core.** MM appears in MICS7's *Complementary
Questionnaire Topics*, not the Base Questionnaire, and only a minority of
countries fielded it. Presence of `mm.sav` is the practical test.

**The blocker is data access.** Registration is per-survey at mics.unicef.org.
The four shortlisted MICS6 surveys, chosen because they are in countries already
in the DHS sample and so support a within-country comparison of the two
instruments:

| Survey | |
|---|---|
| Benin | 2021–22 |
| Gambia | 2018 |
| Malawi | 2019–20 |
| Sierra Leone | 2017 |

For *validation* (see below) a different set matters more — surveys whose
published tables are already transcribed in the methods reference:
**Zimbabwe 2019**, **Iraq 2018**, **Pakistan Punjab 2017-18** and **Pakistan
Sindh 2018-19**.

**Everything except a real-data validation can be built before a file arrives**,
against a synthetic fixture (M8). That is the point of the sequencing.


The deliverable: `prep_mics_sib_histories()`
----

**Add a real `prep_mics_sib_histories()`, exported, alongside
`prep_dhs_sib_histories()` and `prep_nrsim_sib_histories()`.**

Even though the layout question resolved to "long" — which removes the reshape,
and with it some of the anticipated work — the function still has real
substance. MICS splits the module across two files (`wm.sav` woman-level,
`mm.sav` sibling-level), needs `caseid` constructed and the two joined, and has
to cope with country files that omit the constructed CMC columns. None of that
belongs in `get_ego_df()`, which the DHS path shares, or in
`prep_nrsim_sib_histories()`, which is deliberately generic.

It is also the discoverable name. Someone holding a MICS file will look for
`prep_mics_sib_histories()`.

### Division of labour

The rule: **survey-family-specific work goes in the prep; cross-cutting fixes go
in the shared internals.** Three prep functions, one set of internals.

| Item | Lives in |
|---|---|
| M1 optional `sib.dob` / `sib.death.date` | `get_sib_df()` — shared; DHS benefits too |
| M2 join `wm.sav`+`mm.sav`, construct `caseid` | `prep_mics_sib_histories()` |
| M3 lowercase names | `prep_mics_sib_histories()` |
| M4 long layout — skip the reshape | `get_sib_df()` switch, set by the prep |
| M5 varmaps (MICS4/5/6/7) + `MM16` guard | data objects + guard in the prep |
| M6 `add_maternal_deaths(style = "mics6")` | `add_maternal_deaths()` — shared, switched |
| M7 sex-recode and DK guards | `get_sib_df()` — shared |
| M8 fixture | tests |
| V1–V7 validation | `data-raw/`, not the package |

What stays shared and must **not** be forked: varmap handling,
`check_varmap_cols()`, `get_ego_df()`, `get_sib_df()`, the summary block.

### Proposed signature

    prep_mics_sib_histories(mm.df,
                            survey,
                            wm.df        = NULL,
                            varmap       = sibhist_varmap_mics6,
                            id.vars      = c("hh1", "hh2", "ln"),
                            doi.var      = "wdoi",
                            doi.ym       = c("wm6y", "wm6m"),
                            lowercase    = TRUE,
                            weight.scale = 1,
                            add_maternal = FALSE,
                            style        = c("mics6", "mics4"),
                            na.action    = c("include", "exclude"),
                            keep_missing = FALSE,
                            keep_varmap_only = FALSE,
                            verbose      = TRUE)

Notes on the choices that are not obvious:

- **`mm.df` is the only required data argument; `wm.df` is optional.** Confirmed
  2026-08-21 from the public data dictionaries (below): `mm.sav` already carries
  the weights, `psu`, `stratum`, `WDOI`, `WDOB` and a set of respondent
  background variables. Everything the pipeline *requires* can be built from
  `mm.sav` alone. Pass `wm.df` only to bring across further respondent
  covariates.
- **`survey` is required, no default.** MICS has no `v000` equivalent, so the
  package cannot derive one and should not invent one. Making the caller name it
  (`"ZW2019"`) keeps MICS ids comparable with the DHS codes. Open decision 2.
- **`weight.scale = 1`** — `wmweight` is already normalized to mean 1. The single
  most consequential default in the signature.
- **`doi.var = "wdoi"` with `doi.ym` as fallback.** `WDOI` is a ready-made CMC
  interview date, present in both files inspected. Keep `doi.ym` because country
  files vary.
- **No `age` argument.** Respondent age comes from `(WDOI - WDOB) / 12`, both of
  which are CMCs in `mm.sav`. No need to reach into `wm.sav` for it.
- **`id.vars` / `doi.ym` are arguments, not constants**, because MICS6 country
  customisation is real: Sindh adds `MM22A` and drops background variables.
- **`style`** selects the maternal recode, defaulting to the MICS6/7 form. See M6.


What the package requires today
----

Established by feeding minimal varmaps through `prep_nrsim_sib_histories()` and
reading the failures. This is the contract the MICS prep has to satisfy.

`get_ego_df()` requires, after varmap renaming: `age`, `survey`.

`get_sib_df()` requires on the ego data, because they are carried onto every
sibling row (`ego.vars` is hardcoded):

    caseid, wwgt, psu, doi, sex

and on the sibling data, because the derivation `case_when`s reference them
unconditionally:

    sib.sex, sib.alive, sib.age, sib.dob,
    sib.death.date, sib.death.yrsago, sib.death.age

Also hardcoded, and not covered by any guard:

- `sib.sex = ifelse(sib.sex == 2, 'f', 'm')` — anything not literally `2`
  silently becomes `'m'`
- `doi` is treated as a **CMC**. Every date derivation is integer arithmetic in
  months off it.
- `aggregate_maternal_estimates()` hardcodes `ego.id = 'caseid'` and
  `sib.frame.indicator = 'in.F'`.

**What `mm.sav` supplies directly**, which is more than the first draft assumed:

| Package name | MICS6 `mm.sav` |
|---|---|
| `psu`, `stratum` | `psu`, `stratum` — present, no construction needed |
| `wwgt` | `wmweight` — already mean-1 |
| `doi` | `WDOI` — already a CMC |
| `sib.dob` | `MM17C` — imputed CMC, **when present** |
| `sib.death.date` | `MM18C` — imputed CMC, **when present** |
| `sibindex` | `MMLN` — sibling roster position |
| `caseid` | — must be built from `HH1`+`HH2`+`LN` |

`WM1`, `WM2`, `WM3`, `WMINT` are the link keys to `wm.sav`.


The work
----

### M1. `sib.dob` and `sib.death.date` must become *optional* inputs

Still needed, but the reasoning changed. The first draft said MICS never supplies
these. In fact MICS6 usually does, via `MM17C`/`MM18C` — but **Pakistan Sindh's
file does not carry them**, and MICS4/5 predate them. So the package must accept
a varmap that omits them and fall back to deriving.

Today `get_sib_df()`'s required-column guard lists both, so it rejects such a
varmap outright — demonstrated:

    The sibling data is missing required column(s): sib.dob, sib.death.date.

The guard conflates *inputs the varmap must supply* with *columns that must exist
by the time the derivations run*.

**Change:** split the two lists. `sib.sex`, `sib.alive`, `sib.age`,
`sib.death.yrsago` and `sib.death.age` stay required. `sib.dob` and
`sib.death.date` become *derived*: if absent, initialize to `NA_real_` before the
`case_when` block and let the existing derivations fill them.

The derivations that then have to close:

| Target | Derivation | MICS6 input |
|---|---|---|
| `sib.death.date` | `doi - (12*yrsago + 6)` | `MM18` years ago |
| `sib.dob`, living | `doi - (12*age + 6)` | `MM17` current age |
| `sib.dob`, dead | `death.date - 12*death.age` | `MM19` age at death |

The third works only because of the D4 fix in `bc0c38b`. **Validation V2 tests
exactly this**: running Zimbabwe twice, once using `MM17C`/`MM18C` and once
forcing the derivation, checks the supplied and derived paths against each other
on the same file.

### M2. Join the two files and construct `caseid`

The core of `prep_mics_sib_histories()`, and **much** smaller than the first
draft thought. Having read the actual dictionaries, almost everything is already
in `mm.sav`; only `caseid` genuinely has to be constructed.

- **`caseid`** — build from `HH1` + `HH2` + `LN` (cluster, household,
  respondent's line number). Must be unique per respondent;
  `aggregate_maternal_estimates()` hardcodes the name. This is the only real
  construction.
- **`age`** — respondent age in single years as `(WDOI - WDOB) / 12`, both CMCs
  in `mm.sav`. Decide whether to floor or round; floor matches "age last
  birthday".
- **`sex`** — constant `'f'`. MICS interviews women only, and `get_ego_df()`
  already assumes this when the column is absent.
- **`survey`** — from the required argument.
- **`doi`** — `WDOI`, or CMC from `WM6Y`/`WM6M` as a fallback:
  `cmc = (year - 1900) * 12 + month`.
- **`psu`, `stratum`, `wwgt`** — map straight from `psu`, `stratum`, `wmweight`.
- **Joining `wm.sav` is optional**, for covariates beyond the background
  variables `mm.sav` already carries (`welevel`, `MSTATUS`, `CEB`, `religion`,
  `windex5`, …). Join on `WM1`/`WM2`/`WM3` when needed.

### M3. Lowercase the variable names

**Confirmed necessary, and for a better reason than expected: MICS files are
*mixed* case.** In both dictionaries inspected, the questionnaire items and link
keys are uppercase (`MM15`, `HH1`, `WM3`, `WDOI`, `WDOB`, `MSTATUS`, `CEB`) while
the derived and design variables are lowercase (`wmweight`, `psu`, `stratum`,
`welevel`, `religion`, `windex5`). So neither "assume uppercase" nor "assume
lowercase" is safe, and the varmap match is by exact name.

Without normalisation an unmatched varmap produces an empty roster — no error,
just nothing.

**Change:** a `lowercase = TRUE` argument that lowercases `names()` of the input
frames up front, and write every MICS varmap in lowercase.

### M4. Long layout — skip the reshape ⚠

**Resolved: MICS6 ships `mm.sav` with one row per sibling.** Zimbabwe 2019 is
47,835 rows × 48 variables. Verified against the World Bank microdata
dictionaries for Zimbabwe 2019 (catalog 4180) and Pakistan Sindh 2018-19
(catalog 4181).

So the MICS path does **not** call `attributes.to.long()`. It renames per the
varmap, joins the respondent attributes, and hands a long frame to the rest of
the pipeline. The `layout` argument proposed in the first draft is unnecessary.

Consequence: `get_sib_df()` currently *always* reshapes. Give it a
`reshape = TRUE` argument rather than writing a MICS-specific variant — one
shared function, one switch.

### M5. The varmaps, and the `MM16` collision ⚠

> **No longer provisional.** The first draft said the real `.sav` variable names
> were unknown until a survey was downloaded. They are not: the **World Bank
> microdata catalog publishes full data dictionaries without registration**.
> Confirmed 2026-08-21 against Zimbabwe 2019 (catalog 4180, `mm.sav`, 47,835
> records × 48 variables — matching the reference exactly) and Pakistan Sindh
> 2018-19 (catalog 4181, 147,316 × 42). **The MICS6 varmap can be written now,
> with confirmed spellings, before any data arrives.**

**The draft varmap in the analysis repo has the wrong numbering for MICS6.**

It maps `mm5→sib.sex, mm6→sib.alive, mm7→sib.age, mm8→sib.death.yrsago,
mm9→sib.death.age` — that is **MICS4/5** numbering. In MICS6 those are `MM15`
through `MM19`. And its `mm12/13/14` for the pregnancy items match neither
scheme: MICS4/5 uses `MM10/11/12`, MICS6 uses `MM22/23/24`.

Correct MICS6 sibling mappings:

| MICS6 | Package name |
|---|---|
| `MMLN` | `sibindex` |
| `MM15` | `sib.sex` (1 male, 2 female — same coding as DHS) |
| `MM16` | `sib.alive` (1 yes, 2 no, 8 DK) |
| `MM17` | `sib.age` |
| `MM17C` | `sib.dob` *(when present)* |
| `MM18` | `sib.death.yrsago` |
| `MM18C` | `sib.death.date` *(when present)* |
| `MM19` | `sib.death.age` |
| `MM22` | `sib.preg.at.death` |
| `MM23` | `sib.died.childbirth` |
| `MM24` | `sib.died.postpartum` (two-month window) |
| `MM25` | `sib.days.postpartum.death` |
| `MM26` | `sib.died.violence` |
| `MM27` | `sib.died.accident` |

`MM20` and `MM21` are interviewer check items and **are not in the data** —
confirmed absent from both dictionaries. `MM28` is loop control and is absent
too. Nothing between `MM5` and `MM14` appears: the MICS6 roster genuinely starts
at `MM15`.

**Ego columns available directly in `mm.sav`** (so the varmap covers both halves
and the prep constructs only `caseid`):

| MICS6 | Package name |
|---|---|
| `HH1`, `HH2`, `LN` | → build `caseid` |
| `WDOI` | `doi` |
| `WDOB` | → `age` as `(WDOI - WDOB)/12` |
| `wmweight` | `wwgt` |
| `psu` | `psu` |
| `stratum` | `stratum` |
| `MMLN` | `sibindex` |

**Country customisation is real and must not break the prep.** Sindh adds
`MM22A` ("Was deceased sister ever married"), drops `MM17C`/`MM18C`, and carries
42 variables against Zimbabwe's 48. `check_varmap_cols()` already reports
varmap entries missing from a file, which is exactly the right behaviour here.

> ⚠ **The `MM16` collision is worse than documented.** The earlier plan said MICS
> `MM16` is loop control. It is not — in MICS6 loop control is **`MM28`**, and
> **`MM16` is "Is (name) still alive?"**, i.e. `sib.alive`. Mapping MICS6 `MM16`
> onto the DHS meaning of `mm16` would turn *survival status* into *died of
> violence or accident*. The prep should therefore reject
> `mm16 -> sib.died.accident` in any MICS varmap, and the crosswalk generator in
> the analysis repo should be re-run, since it produced the wrong numbering.

`sib.num.children` (MICS4/5 `MM13`) **was dropped in MICS6**. The draft maps
`mm15` to it, which is wrong twice over. Harmless in practice: nothing in the
package reads that column.

MICS4/5 sibling mappings, for the second varmap: `MM5` sex, `MM6` alive, `MM7`
age, `MM8` years since death, `MM9` age at death, `MM10` pregnant, `MM11`
childbirth, `MM12` within two months, `MM13` live births. No violence/accident
items, no CMC columns.

### M6. `add_maternal_deaths()` needs a MICS branch ⚠

**MICS6 supports true maternal mortality, contrary to the first draft.** Both
estimands are computable, which is what makes the DHS comparison work.

    maternal     = female & MM19 >= 12 &
                   (MM22 == 1 | MM23 == 1 | (MM24 == 1 & MM25 <= 42)) &
                   !(MM26 %in% 1 | MM27 %in% 1)

    preg_related = female & MM19 >= 12 &
                   (MM22 == 1 | MM23 == 1 | MM24 == 1)

The second reconstructs the MICS5/DHS-style two-month, no-cause-exclusion series
for backward comparability. These map exactly onto the package's existing
`sib.maternal.death.date` and `sib.preg_related.death.date` outputs, so MICS6
behaves like DHS 7+ and MICS4/5 like DHS 2–6.

**Change:**

    add_maternal_deaths(sib_df,
                        style = c("dhs", "mics6", "mics4"),
                        na.action = c("include", "exclude"),
                        keep_missing = FALSE, verbose = TRUE)

Factor the existing condition into `is_preg_related_dhs()` and add
`is_preg_related_mics()` / `is_maternal_mics()`. Everything else — the `-1`
sentinel, the `NA` fill, the male-blanking, the `sib.died.accident` guard — is
unchanged and shared. `style` defaults to `"dhs"`, so every existing call site is
untouched.

Three skip-pattern traps, all from the questionnaire routing, all of which must
be in the recode:

1. **`MM23 = 1` skips `MM26`/`MM27` entirely.** A childbirth death is
   unconditionally maternal; `NA` on the cause items must not exclude it. Use
   `%in%`, not `==`, which would propagate `NA`.
2. **`MM21` routes sisters who died before age 12 past `MM22`–`MM25`.** Those
   items are `NA` *by design*. The explicit `MM19 >= 12` guard is therefore not
   optional — without it, and with `na.action = "include"`, every under-12 female
   death becomes maternal.
3. **`MM16 = DK` jumps to the next sibling** — no age, no dates, nothing. Those
   rows carry no information; the existing `filter(sib.alive %in% c(0,1))`
   should already drop them. Confirm it does.

> **`na.action` is a substantive statistical choice.** The DHS branch treats a
> missing `sib.time.delivery.death` as **include** — the `is.na(...)` sits inside
> the OR. Trap 2 is a concrete argument that "include" is the wrong default for
> MICS. Note also that MICS has **no DK code on `MM17`, `MM18`, `MM19` or
> `MM25`**, so missing really is missing rather than an explicit refusal.
> **Decide this before writing the branch, and document the reasoning.**

### M7. Sex-recode and DK guards

`sib.sex = ifelse(sib.sex == 2, 'f', 'm')` is safe for MICS6 as documented —
`MM15` is 1 male / 2 female, the same as DHS, with no DK code listed. But the
idiom silently maps anything unexpected to `'m'`, so guard rather than assume:
error on values outside the expected set instead of coercing.

### M8. The synthetic fixture and the contract tests

Built **first**, before M1–M7, so the rest is test-driven. A small MICS6-shaped
pair of frames in code — say 10 respondents × ≤5 siblings, **long** layout,
uppercase names, known answers — as `tests/testthat/helper-simulate-mics.R`.

**Test-only, not shipped package data.** A fabricated fixture in `data/` looking
like real MICS invites someone to mistake it for one. Add `model_mics_dat` only
if a redistributable extract ever exists.

Assert the contract:

- row counts and `sibid` uniqueness after the join
- **weights not divided by 1e6**
- `sib.dob` / `sib.death.date` taken from `MM17C`/`MM18C` when supplied, and
  derived when omitted, with the two agreeing to within the mid-year rounding
- `doi` a plausible CMC
- uppercase input gives the same row count as its lowercase twin — M3
- a childbirth death with `NA` on `MM26`/`MM27` is still maternal — M6 trap 1
- an under-12 female death is **not** maternal under either `na.action` — trap 2
- an `MM16 = DK` sibling is dropped — trap 3
- `sib.preg_related.death.date` includes a 43–60 day postpartum death that
  `sib.maternal.death.date` excludes — the two-estimand distinction
- a varmap mapping `mm16 -> sib.died.accident` errors — M5
- an out-of-range `sib.sex` errors rather than becoming `'m'` — M7


Validation against published MICS numbers
----

The point of this section: **`asdr.agg` should reproduce published MICS
estimates, and `asdr.ind` should not.**

`sibling_estimator()` computes both. The aggregate-visibility estimator is
literally `Σ(w·deaths) / Σ(w·exposure)` — no visibility weighting at all, only
the respondent's sampling weight (`R/get_ec_reports.R:63-65`). That is exactly
the conventional DHS/MICS calculation, and the methods reference confirms MICS
applies **no sibship-size reweighting**: reported siblings only, respondent
excluded, no Gakidou–King correction. So published numbers are a genuine ground
truth for the aggregate path.

The individual-visibility estimator is the contribution. A gap between `asdr.ind`
and the published figure is the finding, not a bug. **If `asdr.agg` does not
reproduce, something in the pipeline is wrong** — and because the stages below
isolate different components, the stage at which it first diverges says which.

### Where the data lives

MICS microdata is registration-gated and cannot be redistributed, so:

    data-raw/
      mics-data/          <- .sav files. GITIGNORED. never committed.
      mics-validation/    <- scripts + expected values. tracked.
        published-targets.csv
        validate.R

Already set up: `^data-raw$` is in `.Rbuildignore` and `data-raw/mics-data/` is
in `.gitignore`, so neither the data nor the scripts reach the built package and
the microdata cannot be committed by accident.

**`published-targets.csv` is tracked even though the microdata is not.** The
published numbers come from public survey reports, so the expectations are
shareable; anyone who obtains the data can then reproduce the check. Transcribe
them from `notes/mics-maternal-mortality-reference.md`, which already has
Zimbabwe 2019 in full.

Run it as a testthat file that skips unless the data is present:

    skip_if_not(dir.exists(mics_data_dir()), "MICS microdata not available")

so the suite stays green for anyone without the files, and becomes a real
regression test for anyone with them.

### Data dictionaries are public even though the data is not

The World Bank microdata catalog serves full variable dictionaries without
registration — that is where the variable lists above were confirmed. Use them to
check a survey's `mm.sav` **before** requesting it: whether the module was
fielded at all, whether `MM17C`/`MM18C` are present, and what country-specific
items were added. Registration is only needed for the actual `.sav` files.

- Zimbabwe 2019: `catalog/4180/data-dictionary/F6?file_name=mm.sav`
- Pakistan Sindh 2018-19: `catalog/4181/data-dictionary/F6`

### Which surveys, and why each

| Survey | Why |
|---|---|
| **Zimbabwe 2019** | Primary. Fullest published tables — TM.9.3 age-specific, TM.9.1/9.2 adult mortality, DQ.7.1/7.2 data quality. Has `MM17C`/`MM18C` |
| **Iraq 2018** | Second country. Its sampling-error table *agrees* with TM.9.3, unlike Zimbabwe's |
| **Pakistan Sindh 2018-19** | **Confirmed to lack `MM17C`/`MM18C`** while carrying `WDOI`/`WDOB` — exercises the M1 derivation fallback, the code path Zimbabwe never reaches. Also adds `MM22A`, so it tests tolerance of country-specific items. 147,316 sibling rows |
| **Pakistan Punjab 2017-18** | Published point estimates but **no CIs** — confirms the pipeline does not assume uncertainty is available |

### Staged checks

Each stage isolates one component. Run them in order; the first failure localises
the fault.

**V1. File shape and data quality.** Before any estimation. Zimbabwe `mm.sav` =
47,835 rows × 48 vars. DQ.7.1: siblings 82.8% living / 17.1% dead / 0.1%
missing. DQ.7.2: mean sibship size 4.9, sex ratio at birth 0.99. *Tests the join
and the `sib.alive` recode, nothing else.*

**V2. Exposure by age group.** Zimbabwe TM.9.3, woman-years:

    15-19  15,313    30-34  21,434    45-49   6,277
    20-24  18,198    35-39  16,044    total 108,985
    25-29  21,240    40-44  10,479

*Tests the date derivations, the CMC arithmetic and the reference window.* The
highest-value stage: it **resolves the 7-year boundary ambiguity**, which no MICS
document states. Try both 0–6 and 1–7 completed years, and both CMC-based
exposure allocation and integer-year binning — only one combination will match.
Also run it twice on Zimbabwe, once using `MM17C`/`MM18C` and once forcing the M1
derivation, to check the two agree.

**V3. Death counts by age.** Maternal deaths 3, 10, 9, 19, 15, 10, 2; total 68.
*Tests the M6 recode and the three skip-pattern traps.* With only 68 deaths the
age-specific cells are small, so compare counts rather than rates here.

**V4. Age-specific rates per 1,000.** 0.23, 0.56, 0.41, 0.87, 0.91, 0.96, 0.30.
*V2 and V3 combined, via `asdr.agg`.*

**V5. Age-standardised rate.** 0.59 per 1,000. *Tests `get_ego_age_distn()` and
the aggregation in `aggregate_maternal_estimates()`.* This stage **resolves the
age-standard ambiguity** flagged in the reference §6: MICS text says "the age
distribution of the survey respondents" — which is what `get_ego_age_distn()`
does — while IUSSP says "women aged 15–49 in the households surveyed" and MMEIG
says "the female population of respondent households". Try both; only one will
reproduce.

**V6. PM — proportion of female deaths that are maternal.** 10.0% overall.
*Independent check on the all-cause denominator, which V2–V5 never exercise.*

**V7. Adult mortality, both sexes.** TM.9.1 ₅m_x and TM.9.2 ₃₅q₁₅ by sex —
Zimbabwe 224 (W) / 219 (M), Iraq 49 / 86, Punjab 75 / 85. *A completely
independent check that never touches the maternal recode*, and the only one that
exercises the male roster. Note `nax = 2.5` for all groups in the MICS formula.

### Out of scope for the package

**MMR itself.** `MMR = 100,000 × MMRate / GFR`, and the GFR comes from the
women's birth history over the same 7-year window — a different module this
package does not touch. Validation stops at the **rate** (V4/V5) and **PM** (V6),
which are what `siblingsurvival` produces. Reproducing the published MMR of 462
belongs in the analysis repo, where the birth history is available.

**Confidence intervals.** MICS uses jackknife with ±2·se; this package uses a
bootstrap with percentile intervals. They will not match and should not be
expected to.

### Known traps in the published numbers

From the reference, so they are not mistaken for pipeline bugs:

- **Zimbabwe 2019's sampling-error table says MMR 413.637, the headline says
  462.** Validate against TM.9.3, not SE.1. Iraq's tables agree with each other.
- **Units.** A MICS6 footnote says "per 100,000 women 15–49" while the table
  footnote says "per 1,000 woman-years". The printed column is per 1,000.
- **Do not compare Zimbabwe 2014 (614) with 2019 (462)** — different estimand,
  different window, different cause exclusion.
- The narrative text in MICS6 reports still says "pregnancy-related deaths" where
  the table footnote excludes accidents and violence. **The footnote and
  indicator definition are authoritative**; the sentence is un-updated MICS5
  boilerplate.


Sequence
----

1. **Request MICS microdata access now**, in parallel with everything else — it
   is the long pole, and nothing below waits on it except V1–V7. Prioritise
   **Zimbabwe 2019**: it is the validation key, having the fullest published
   tables, even though it is not on the analysis shortlist. Then the four
   shortlisted surveys, then **Pakistan Sindh 2018-19** for the derivation
   fallback. Check each survey's public data dictionary first — it says whether
   the module was fielded before you spend a registration on it.

   Also worth a browser session: the **MICS6 Tabulation Plan** and the
   **Standard SPSS Syntax** are Cloudflare-blocked from a non-browser client and
   absent from the Wayback Machine, but the reference calls the syntax "the only
   authoritative statement of the estimation algorithm". Retrieving it could
   *answer* the two ambiguities that V2 and V5 are otherwise designed to resolve
   empirically — the 7-year boundary and the age standard — and would be worth
   more than either validation stage.
2. **M8** — the fixture and the contract tests. First, so the rest is
   test-driven.
3. **M1** — make `sib.dob` / `sib.death.date` optional in the shared
   `get_sib_df()`. Until this is done no MICS varmap runs at all.
4. **`prep_mics_sib_histories()` skeleton** — the signature above, `lowercase`
   (M3), the `reshape = FALSE` path (M4), and delegation to the shared internals.
5. **M2** — the two-file join and `caseid`.
6. **M5** — the MICS6 varmap first, then MICS4/5, then MICS7. Regenerate from a
   corrected crosswalk rather than hand-editing the draft.
7. **M6, M7** — the maternal recode and the guards. M6 needs the `na.action`
   decision made first.
8. **V1–V7** — validate against Zimbabwe 2019 as soon as the file is in hand,
   then Iraq, then Sindh for the derivation fallback.
9. **Then the analysis**: the four shortlisted surveys, DHS↔MICS within country.


Open decisions
----

1. **`na.action` for MICS missing/DK on `MM22`–`MM25`** (M6). Changes the
   numerator of every MICS estimate. Trap 2 argues against inheriting the DHS
   "include" default. Needs a default and a documented rationale.
2. **How `survey` should be constructed** (M2) so MICS and DHS survey ids are
   comparable in one analysis.
3. **Which estimand the paper reports.** MICS6 offers both; DHS 2–6 offers only
   pregnancy-related. Using pregnancy-related throughout maximises comparability
   and is now clearly achievable — but it discards MICS6's better-defined
   maternal measure. Worth stating explicitly either way.
4. **E4** in `PACKAGE-HANDOFF.md` — whether `adj.factor` is meant to be a global
   scalar. V5 is adjacent: it resolves which reference population MICS uses.


What will legitimately not reproduce
----

Worth writing down so it is not later rediscovered as a defect:

- **`asdr.ind` will not match published MICS numbers, by design.** MICS applies
  no sibship-size correction; the individual-visibility estimator is exactly that
  correction. The gap is the contribution. `asdr.agg` is the one that must match.
- **No male maternal estimates.** MICS interviews women only, so
  `only_females = FALSE` warns and returns `NA` for male siblings — correctly.
  Male *all-cause* estimates are fine, and V7 checks them.
- **`sib.dob` / `sib.death.date` are approximations wherever `MM17C`/`MM18C` are
  absent**, derived from ages and years-ago on a mid-year assumption. Any
  DHS↔MICS comparison should say so, because derived dates carry heaping the DHS
  CMC values do not. Note MICS chose the 7-year window specifically "to reduce
  possible heaping of reported years since death on five-year intervals".
- **CIs will not match** — bootstrap versus jackknife, percentile versus ±2·se.
- **MICS2/MICS3 will never be supported** — different method entirely.
