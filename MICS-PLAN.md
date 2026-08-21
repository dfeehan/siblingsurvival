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
| Data file | inside `hl.sav` | dedicated **`mm.sav`** * | dedicated **`mm.sav`** |
| Reference period | ~10–14 yrs | 7 years | 7 years |
| Estimand | — | Pregnancy-related, 2 months, **no** cause exclusion | **Maternal**, 42 days, **excl.** violence/accidents |

\* The methods reference says MICS4/5 keep the roster in the women's file.
Checking the archive, every MICS4/5 survey that fielded the module ships a
separate `mm.sav`, and every one that lacks `mm.sav` has no `MM` variables in
`wm.sav` either. See the inventory section.

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

**Data access is no longer the blocker.** A local archive of 229 MICS surveys
(MICS2–MICS7) is in hand. See the inventory section below — it changes the
survey plan substantially.

**Everything is now buildable and validatable.** The synthetic fixture (M8) is
still worth building first, because it makes the contract explicit and runs in
CI where the microdata cannot, but validation against real files (V1–V7) no
longer waits on anything.


Inventory: which surveys actually have a sibling history
----

Scanned a local archive of **229 MICS surveys** (MICS2–MICS7) on 2026-08-21,
opening every survey's dataset zip and reading the variable names out of each
`mm.sav`. Full results in `data-raw/mics-inventory.csv`.

**Only 13 of 229 surveys have a usable sibling history.**

| | MICS2 | MICS3 | MICS4 | MICS5 | MICS6 | MICS7 |
|---|---|---|---|---|---|---|
| surveys in archive | 42 | 45 | 42 | 34 | 61 | 5 |
| **with sibling roster** | 0 | 0 | **2** | **7** | **4** | 0 |

The 13:

| Survey | Round | Scheme | Sibling rows | CMC dates | psu | Estimand |
|---|---|---|---|---|---|---|
| BTN_2010 | MICS4 | `MM5`–`MM13` | 60,333 | **none** | yes | pregnancy-related |
| MRT_2011 | MICS4 | `MM5`–`MM13` | 67,412 | `MM7C`/`MM8C` | no | pregnancy-related |
| BEN_2014 | MICS5 | `MM5`–`MM13` | 80,643 | `MM7C`/`MM8C` | no | pregnancy-related |
| COG_2014 | MICS5 | `MM5`–`MM13` | 51,672 | `MM7C`/`MM8C` | no | pregnancy-related |
| GNB_2014 | MICS5 | `MM5`–`MM13` | 46,102 | `MM7C`/`MM8C` | no | pregnancy-related |
| STP_2014 | MICS5 | `MM5`–`MM13` | 15,601 | `MM7C`/`MM8C` | no | pregnancy-related |
| MWI_2013 | MICS5 | `MM5`–`MM13` | 133,054 | `MM7C`/`MM8C` | no | pregnancy-related |
| ZWE_2014 | MICS5 | `MM5`–`MM13` | 71,994 | `MM7C`/`MM8C` | no | pregnancy-related |
| GIN_2016 | MICS5 | `MM5`–`MM13` | 38,132 | `MM7C`/`MM8C` | no | pregnancy-related |
| IRQ_2018 | MICS6 | `MM15`–`MM27` | 186,790 | `MM17C`/`MM18C` | yes | **maternal** + PR |
| MDG_2018 | MICS6 | `MM15`–`MM27` | 89,420 | `MM17C`/`MM18C` | no | **maternal** + PR |
| ZWE_2019 | MICS6 | `MM15`–`MM27` | 47,835 | `MM17C`/`MM18C` | yes | **maternal** + PR |
| COM_2022 | MICS6 | `MM15`–`MM27` | 37,491 | `MM17C`/`MM18C` | yes | **maternal** + PR |

### ⚠ The analysis shortlist does not survive this

**None of Benin 2021, Gambia 2018, Malawi 2019 or Sierra Leone 2017 fielded the
module.** The audit's shortlist was built from a survey *catalogue*, on country
and round, without checking whether MM was actually collected — and MM is an
optional module. Verified directly: Gambia 2018's `wm.sav` has 409 variables and
**zero** `MM`-prefixed ones.

**But Benin and Malawi are both available one round earlier**, as `BEN_2014` and
`MWI_2013`. So the within-country DHS↔MICS design survives — it just moves to
MICS5.

**This makes MICS4/5 support essential rather than optional.** MICS6 alone gives
4 surveys; adding MICS4/5 gives 13. That reverses the priority in M5: write the
**MICS4/5 varmap first**, since it covers nine of the thirteen.

### Two traps the scan turned up

1. **MICS3 `mm.sav` is the *Men's* file, not maternal mortality.** CAF_2006,
   GHA_2006 and MWI_2006 all have an `mm.sav` containing `MM4` "Man's Line
   number", `MM7` "Results of Men's interview", and `mmweight` "men's sample
   weight". Filename collision, nothing more. Any inventory built by globbing for
   `mm.sav` will pick these up.
2. **MICS4/5 surveys without `mm.sav` genuinely did not field the module** — the
   roster is not tucked inside `wm.sav`. Checked NGA_2011, THA_2012, IRQ_2011,
   NGA_2016, SDN_2014 and CMR_2014: all have zero `MM`-prefixed variables in
   `wm.sav`. So **`mm.sav` presence is the definitive test**, contrary to the
   methods reference's suggestion that MICS4/5 keep the roster in the women's
   file.

### What the inventory changes downstream

- **`psu` is usually absent.** Only 4 of 13 carry it in `mm.sav`; `stratum` is
  rarer still. `get_sib_df()` hardcodes `psu` as required, so the prep must
  construct it — `HH1` is the cluster. See M2.
- **`BTN_2010` is the derivation-fallback case.** It is the only survey with no
  CMC columns at all, so it exercises M1 exactly as Pakistan Sindh would have.
  Sindh is not in this archive.
- **`MDG_2018` has no `WDOB`**, so respondent age cannot come from
  `(WDOI - WDOB)/12` there and must be joined from `wm.sav`. This is why `wm.df`
  is optional rather than unused.
- **Zimbabwe appears twice** — `ZWE_2014` (MICS5) and `ZWE_2019` (MICS6) — and
  the methods reference has published numbers for both (614 and 462). That is a
  within-country, across-round, across-estimand validation pair, and the best
  test of the MICS4/5 and MICS6 branches against each other.
- **Variable counts range 23–48**, confirming that country customisation is the
  norm. `check_varmap_cols()` reporting missing varmap entries is the right
  behaviour; the prep must not require the full set.


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
- **`psu`** — map from `psu` **when present, which is only 4 of the 13 usable
  surveys**; otherwise construct it from `HH1` (the cluster). `get_sib_df()`
  hardcodes `psu` as required, so this cannot be skipped. `stratum` is rarer
  still and is not required by the pipeline.
- **`wwgt`** — from `wmweight`.
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

- Zimbabwe 2019: `catalog/4180/data-dictionary/F6?file_name=mm.sav` (matches the real file exactly — 48 variables)
- Pakistan Sindh 2018-19: `catalog/4181/data-dictionary/F6`

### Which surveys, and why each

All four are in the local archive.

| Survey | Why |
|---|---|
| **ZWE_2019** | Primary. Fullest published tables — TM.9.3 age-specific, TM.9.1/9.2 adult mortality, DQ.7.1/7.2 data quality. 47,835 sibling rows |
| **ZWE_2014** | Same country one round earlier, MICS5 scheme, published MMR 614 against 2019's 462. Validates the MICS4/5 branch *and* the estimand difference, in a setting where everything else is held roughly constant |
| **IRQ_2018** | Second country, MICS6. Its sampling-error table *agrees* with TM.9.3, unlike Zimbabwe's. 186,790 rows, and has `psu` but no `stratum` |
| **BTN_2010** | **The only survey in the archive with no CMC columns at all** — exercises the M1 derivation fallback, the code path every other survey skips |

Pakistan Punjab and Sindh, named in the first draft, are **not in the archive**.
Punjab's role (a survey publishing point estimates with no CIs) is worth keeping
in mind but no longer has a test case; Sindh's role is taken by BTN_2010.

### Results (2026-08-21, Zimbabwe 2019)

Run against the survey report itself, not just the transcribed numbers.

**Everything reproduces except the maternal classification.**

| Stage | Table | Observed | Published | |
|---|---|---|---|---|
| V1 sibling rows | — | 47,835 | 47,835 | ✅ |
| V1 survival status | DQ.7.1 | **39,616 / 8,160 / 42** | **39,616 / 8,160 / 42** | ✅ exact |
| V1 mean sibship size | DQ.7.2 | 4.9 | 4.9 | ✅ |
| V1 sex ratio | DQ.7.2 | 1.00 | 0.99 | ✅ |
| V2 exposure, female | TM.9.1/9.3 | 108,614 | 108,985 | ✅ 0.997 |
| V2 exposure, male | TM.9.1 | 109,979 | 110,089 | ✅ 0.999 |
| V5 age-adj. rate, female | TM.9.1 | **6.27** | **6.28** | ✅ |
| V5 age-adj. rate, male | TM.9.1 | 6.16 | 6.09 | ✅ |
| V7 all-cause deaths, female | TM.9.1 | 665 | 674 | ✅ 0.987 |
| V7 all-cause deaths, male | TM.9.1 | 670 | 668 | ✅ 1.002 |
| V7 ₃₅q₁₅, women | TM.9.2 | 222.7 | 224 | ✅ |
| V7 ₃₅q₁₅, men | TM.9.2 | 218.5 | 219 | ✅ |
| **V3 maternal deaths** | **TM.9.3** | **58.9** | **68** | ❌ **0.87** |
| V4/V6 | TM.9.3 | — | — | blocked by V3 |

**V1 matches exactly once computed MICS's way.** The earlier 82.5-versus-82.8
discrepancy was an artefact of the comparison, not the data: DQ.7.1 is
**unweighted** and its denominator is **47,818**, not 47,835, because MICS
excludes the 17 siblings whose sex is missing. Recomputed on that basis every
cell agrees to the unit — 39,616 living, 8,160 dead, 42 missing.

**TM.9.1 and TM.9.2 are the control.** They run through the same prep, the same
exposure calculation, the same window, the same weights and the same death
placement as TM.9.3 — everything except the maternal recode. They reproduce to
within 1–2%. So the machinery is correct and **the entire V3 gap sits in the
classification rule**.

**V5 resolved the age-standard ambiguity.** The reference flagged three
candidate reference populations (MICS text: survey respondents; IUSSP: women
15–49 in surveyed households; MMEIG: female population of respondent
households). Standardising the all-cause female rate by
`get_ego_age_distn()` — the survey respondents — gives **6.27 against a
published 6.28**. MICS uses interviewed women, as its own text says.

**V2 resolved the 7-year boundary.** No MICS document states whether the window
is years-since-death 0–6 or 1–7. Testing all three candidates against the
published exposure column:

| window | ratio to published |
|---|---|
| `[doi-84, doi)` — **0–6 completed years** | **0.9966** |
| `[doi-96, doi-12)` — 1–7 years | 0.9861 |
| shifted one month | 0.9958 |

**0–6 wins**, which is what `cell_config('7yr_beforeinterview')` already
implements. The residual 0.34% is age-patterned — exact at 40–49, worst at
15–19 — and about a quarter of it is siblings dropped for missing sex or
survival status (keeping them gives 0.9974). Not chased further.

> ## ✅ RESOLVED 2026-08-21 by Iraq 2018
>
> **TM.9.3's numerator is *pregnancy-related* deaths, not maternal deaths.**
> Iraq 2018 reproduces to within 1%:
>
> | | ours | published | ratio |
> |---|---|---|---|
> | female exposure | 471,232 | 471,294 | **1.0000** |
> | male exposure | 485,061 | 485,398 | **0.9993** |
> | female all-cause deaths | 574 | 574 | **1.0007** |
> | male all-cause deaths | 1,158 | 1,158 | **0.9996** |
> | ₃₅q₁₅ women / men | 49.5 / 85.8 | 49 / 86 | ✅ |
> | **TM.9.3, pregnancy-related** | **64.9** | **64.4** | **1.008** |
> | TM.9.3, maternal | 54.2 | 64.4 | 0.848 |
>
> Every age group of TM.9.1 matches at ratio 1.00. The PM follows too: ours
> 11.3% against a published 11.2%.
>
> So **the methodology text is authoritative and table footnote A is the stale
> boilerplate** — the reverse of what `notes/mics-maternal-mortality-reference.md`
> §8 concluded. The column is *labelled* "Maternal Deaths" and footnoted as
> excluding accidents and violence, but the number printed is the
> pregnancy-related count, on the two-month window, with no cause exclusion.
>
> **Zimbabwe 2019 is the anomaly, not the package.** It fails to reproduce on
> *every* quantity, including ones with no maternal recode at all — exposure
> 0.997, female all-cause deaths 0.987 — while Iraq is exact on all of them. Its
> published tables are already known to be internally inconsistent: the
> sampling-error appendix gives an MMR of 413.637 against the report's own
> headline of 462, whereas Iraq's SE table agrees with its TM.9.3. Treat
> Zimbabwe's TM.9.1/TM.9.3 as unreliable rather than chasing them further.
>
> ### Why Iraq's exposure is not *exactly* equal
>
> The residual is 0.013% for females and 0.069% for males — small, but the male
> figure is five times the female one and every male age group is short, which is
> the signature of dropped siblings rather than noise.
>
> It is entirely about **siblings whose survival status is unknown**. Iraq has
> **81 male** such siblings against **21 female** — respondents evidently know
> less about brothers — and `prep_*_sib_histories()` drops them by default, so
> their exposure never enters the denominator.
>
> | | female exposure | male exposure |
> |---|---|---|
> | `keep_missing = FALSE` (default) | 471,232 (**−62**) | 485,061 (**−337**) |
> | `keep_missing = TRUE` | 471,342 (**+48**) | 485,495 (**+97**) |
> | **published** | **471,294** | **485,398** |
>
> **The published value sits between the two.** So MICS neither drops these
> siblings nor carries them to the interview date — it censors them somewhere in
> between, or drops the subset whose age is also unknown. Deaths are unaffected
> either way (574 / 1,158 under both settings), since a sibling with unknown
> survival status contributes no death.
>
> The whole spread is under 0.1% of exposure, so this is a rounding-level
> convention rather than a substantive one, and it does not affect any rate to
> two decimal places. Worth knowing rather than fixing: exactly reproducing it
> would mean guessing MICS's censoring rule. Note it is *not* what ails Zimbabwe
> — there `keep_missing = TRUE` still leaves exposure at 0.9974 of published.
>
> **Consequence for the package:** `add_maternal_deaths(style = "mics6")` should
> produce both columns, but any comparison against published MICS figures must
> use `sib.preg_related.death.date`, not `sib.maternal.death.date`. Worth stating
> in the docs, since the published column name says otherwise.

The original analysis, kept because it shows what was ruled out. The report's own
methodology text is internally inconsistent about which estimand TM.9.3 uses:

> "Age-specific mortality rates are calculated by dividing the number of
> **pregnancy-related deaths** by years of exposure" (p. 110)

while footnote A of the table says

> "A maternal death is defined as the death of a woman while pregnant or within
> 42 days of termination of pregnancy, **from any cause except accidents or
> violence**"

Neither reading reproduces. Ruled out:

- *The package.* A direct hand tabulation off the raw `.sav` gives 58.5 weighted
  maternal deaths against the package's 58.9. They agree; both are short of 68.
- *The cause conditions.* Removing the 42-day cut and the violence/accident
  exclusion — that is, the broadest possible pregnancy-related definition —
  reaches only 62.7.
- *The `MM19 >= 12` guard.* Costs nothing in Zimbabwe; every female death aged
  12+ has `MM22` asked (2,678 of 2,678).
- *`na.action`.* The `9 = no response` codes on `MM22`/`MM23`/`MM24` number 6, 7
  and 7 rows. `MM25` is never missing when `MM24 == 1`. Both settings give
  identical results here.
- *Window and age-binning conventions.* All four combinations of
  {integer `MM18` 0–6, CMC `MM18C`} × {reported `MM19`, CMC-derived age} land in
  656–666 female deaths and 58.5–58.9 maternal. A wider 0–7 window overshoots
  female deaths (734 vs an implied 680) while still undershooting maternal (64.9).

With TM.9.1's female-death denominator now known (**674**, not the 680 inferred
from rounded percentages), the discrepancy is sharp:

| | ours | published |
|---|---|---|
| female deaths 15–49 | 665 | 674 |
| pregnancy-related | 62.7 | — |
| maternal | 58.9 | 68 |
| **PM** | **8.9% (mat) / 9.4% (PR)** | **10.1%** |

So MICS classifies proportionally more female deaths as maternal than *any*
reading of the questionnaire items produces — and it does so while agreeing with
us on the denominator to within 1.3%.

**Against the unrounded published counts** — recovered as `PM × all-cause
deaths`, since TM.9.3 prints deaths as integers — the pregnancy-related reading
gets to 92%, and the residual is concentrated rather than uniform:

| age | ours (PR) | published (unrounded) | gap | all-cause gap |
|---|---|---|---|---|
| 15–19 | 1.6 | 3.5 | **−1.9** | **−5** |
| 20–24 | 10.6 | 10.2 | +0.4 | 0 |
| 25–29 | 8.4 | 8.7 | −0.3 | +1 |
| 30–34 | 17.2 | 18.7 | **−1.5** | **−3** |
| 35–39 | 15.1 | 14.7 | +0.4 | 0 |
| 40–44 | 8.0 | 10.0 | **−2.0** | **−4** |
| 45–49 | 2.0 | 2.1 | −0.1 | +2 |
| **total** | **62.7** | **67.9** | **−5.2** | **−9** |

The three age groups with a maternal shortfall are exactly the three with an
all-cause shortfall, which suggests deaths being *missed* rather than
*misclassified*. But the maternal gap is a far larger share of the all-cause gap
(38–50%) than the PM in those groups (8–13%), so the missing deaths would have to
be disproportionately maternal — which no mechanism we can find explains.

**A real bug surfaced on the way, though it does not explain the gap.** MICS
codes don't-know and no-response on the *numeric* sibling items as **98 and 99**,
not as missing:

| item | DK/NR values | matches DQ.7.1 |
|---|---|---|
| `MM17` age of living sibling | 99 × 180 | "Age of living siblings: Missing/DK **180**" |
| `MM18` years since death | 98 × 95, 99 × 57 | |
| `MM19` age at death | 98 × 96, 99 × 67 | both DK = **102**, published **102** |

Passed through as real values these are silently catastrophic: a sibling with
`sib.death.age = 98` gets a date of birth 98 years before her death, and
`sib.age = 99` puts a living sibling outside every age group. **The damage is
masked whenever MICS supplies its own imputed CMC dates**, because then the
derivations never fire — which is why Zimbabwe's numbers do not move when it is
fixed. It would have been severe on `BTN_2010`, the one survey in the archive
that ships no CMC columns at all.

Now fixed in `recode_mics_sib_vars()`. With the CMC columns deliberately dropped
from the varmap, the derived `sib.dob` correlates **0.9999** with MICS's own
`MM17C` — which both validates the derivation and shows how wrong it was before.

Ruled out as the source of the missing deaths:

- *Siblings dropped for missing sex or survival status.* `keep_missing = TRUE`
  gives identical all-cause counts in every age group.
- *Missing date fields.* No female death in the file has a missing `MM18`,
  `MM19` or `MM18C`.
- *Age binning.* CMC-derived age gives 663 female deaths against reported
  `MM19`'s 656; the published figure is 674, so neither reaches it.
- *Weighting.* Weights average 1.0041, so weighted and unweighted counts differ
  by well under the gap: unweighted 660 female / 666 male against weighted
  665.7 / 669.5.
- *The 98/99 codes.* Fixing them leaves Zimbabwe's counts unchanged, since
  `MM17C`/`MM18C` are complete in that file.

**The remaining asymmetry is the odd part.** Male all-cause deaths reproduce
(669.5 against 668, ratio 1.002) while female do not (665.7 against 674, 0.987).
DQ.7.1 reports *more* dead brothers than sisters overall (4,121 against 4,039),
and our counts preserve that ordering inside the reproductive window (666 male
against 660 female unweighted) — while MICS reverses it (668 male, 674 female).
Something in MICS's processing raises female deaths in the 15–49 window relative
to male ones, and we cannot see what.

> **This makes retrieving the MICS6 Standard SPSS Syntax a blocker rather than a
> nice-to-have.** The methods reference calls it "the only authoritative
> statement of the estimation algorithm", and it is Cloudflare-403 from a
> non-browser client. V3 is unlikely to be resolved by further guessing.

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

**V5. Age-standardised rate.** ✅ **Resolved 2026-08-21 on the all-cause rate:
MICS standardises by the age distribution of the *survey respondents*, which is
what `get_ego_age_distn()` computes** (6.27 against a published 6.28). Originally
posed as: *Tests `get_ego_age_distn()` and the aggregation in
`aggregate_maternal_estimates()`*, and resolving the ambiguity flagged in the
reference §6: MICS text says "the age
distribution of the survey respondents" — which is what `get_ego_age_distn()`
does — while IUSSP says "women aged 15–49 in the households surveyed" and MMEIG
says "the female population of respondent households". Try both; only one will
reproduce.

**V6. PM — proportion of female deaths that are maternal.** 10.0% overall.
*Independent check on the all-cause denominator, which V2–V5 never exercise.*

**V7. Adult mortality, both sexes.** ✅ **Passes** — see the results table. This
turned out to be the most useful stage of all, because it is the *control* for
V3: same pipeline, no maternal recode. TM.9.1 ₅m_x and TM.9.2 ₃₅q₁₅ by sex —
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

1. ~~Request MICS microdata access~~ — **done**; a 229-survey archive is in hand
   and the 13 usable surveys are inventoried in `data-raw/mics-inventory.csv`.
   Nothing is now blocked on data.

   Still worth a browser session: the **MICS6 Tabulation Plan** and the
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
6. **M5** — the **MICS4/5 varmap first**, since it covers nine of the thirteen
   usable surveys, then MICS6 for the other four. No MICS7 survey in the archive
   fielded the module, so that varmap can wait. Regenerate from a corrected
   crosswalk rather than hand-editing the draft.
7. **M6, M7** — the maternal recode and the guards. M6 needs the `na.action`
   decision made first.
8. **V1–V7** — validate against Zimbabwe 2019 as soon as the file is in hand,
   then Iraq, then Sindh for the derivation fallback.
9. **Then the analysis**: the four shortlisted surveys, DHS↔MICS within country.


Open decisions
----

1. ⏳ **`na.action` for MICS missing/DK on `MM22`–`MM25`** (M6). **DEFERRED
   2026-08-21 — must be settled before M6 is written; do not let this slip.**
   *Empirical note from V3:* on Zimbabwe 2019 the choice makes **no difference
   at all** — `MM25` is never missing when `MM24 == 1`, and the `9 = no
   response` codes on `MM22`/`MM23`/`MM24` number 6, 7 and 7 rows out of 47,835.
   So this is a low-stakes decision for MICS6 as far as the data shows, but it
   still needs a documented default, and it may bite harder in MICS4/5.*
   Changes the numerator of every MICS estimate. Trap 2 argues against
   inheriting the DHS "include" default: `MM21` routes sisters who died before
   age 12 straight past `MM22`–`MM25`, so those items are `NA` *by design*, and
   "include" would make every under-12 female death maternal. Needs a default
   and a documented rationale. `add_maternal_deaths()` is not to gain a MICS
   branch until this is decided.
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
