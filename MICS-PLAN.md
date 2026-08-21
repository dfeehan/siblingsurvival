Plan: MICS support in `siblingsurvival`
====

Working plan for adding MICS6 surveys alongside DHS. Split out of
`PACKAGE-HANDOFF.md` (section B) once sections A, C, D and E were done, so that
this file is the single place the MICS work is tracked.

Written 2026-08-20 on branch `mics`, against `0.3.0.9000`. Every code claim below
was checked against the source or demonstrated by running it; claims about MICS
content come from the feasibility audit in the analysis repo and are marked as
such.


State of play
----

**The estimator applies.** MICS6 collects a full per-sibling roster — `[S1]`
oldest, `[S2]` second oldest, … with MM16 as loop control — not the indirect
sisterhood method that several secondary sources attribute to MICS. Verified
against the MICS6 model Individual Women's questionnaire. So the individual
visibility estimator is computable from MICS in principle.

**The blocker is data access, not code.** The audit's Q4 — does the roster
survive into the *released* microdata, and in what layout — cannot be answered
without a file, and Q8 records that no survey has been requested yet.
Registration is per-survey at mics.unicef.org. The four shortlisted MICS6
surveys, chosen because they are in countries already in the DHS sample and so
support a within-country comparison of the two instruments:

| Survey | |
|---|---|
| Benin | 2021–22 |
| Gambia | 2018 |
| Malawi | 2019–20 |
| Sierra Leone | 2017 |

**Everything except the layout question can be built before a file arrives**, by
testing against a synthetic fixture (M7). That is the point of the sequencing
below.

Audit artifacts, all in the analysis repo
(`~/Dropbox/maternal-mortality/maternal-mortality`):

- `code/mics-audit/mics-feasibility.qmd` — the memo, with a go/no-go table
- `out/mics-audit/mics_item_crosswalk.csv` — MICS6 items mapped to the names
  this package expects, each row citing its evidence
- `mics_varmaps/sibhist_varmap_mics6_DRAFT.csv` — generated from the crosswalk,
  so the two cannot drift


The deliverable: `prep_mics_sib_histories()`
----

**Add a real `prep_mics_sib_histories()`, exported, alongside
`prep_dhs_sib_histories()` and `prep_nrsim_sib_histories()`.**

An earlier draft of this plan hedged — proposing it as a thin wrapper over
`prep_nrsim_sib_histories()` that could be dropped if it held no logic of its
own. That was wrong. Once M2 is on the table, the function has real substance:
MICS supplies **none** of `caseid`, `survey`, `doi` or `psu`, so something has to
construct all four from `HH1`/`HH2`/`LN` and `WM6M`/`WM6Y` before the shared
machinery can run. That construction is MICS-specific. It does not belong in
`get_ego_df()`, which is shared with the DHS path, and it does not belong in
`prep_nrsim_sib_histories()`, which is deliberately generic.

It is also the discoverable name. Someone holding a MICS file will look for
`prep_mics_sib_histories()`, and making them find `prep_nrsim_sib_histories()`
and hand-assemble a varmap plus four constructed columns is a worse interface
than the DHS path offers.

### Division of labour

The rule: **survey-family-specific work goes in the prep; cross-cutting fixes go
in the shared internals.** Three functions, one set of internals.

| Item | Lives in |
|---|---|
| M1 derived `sib.dob` / `sib.death.date` | `get_sib_df()` — shared; the DHS path benefits too |
| M2 construct `caseid`, `doi`, `survey`, `psu` | `prep_mics_sib_histories()` |
| M3 lowercase names | `prep_mics_sib_histories()` |
| M4 wide vs long layout | `prep_mics_sib_histories()` |
| M5 varmap + `MM16` guard | data object + guard in the prep |
| M6 `add_maternal_deaths(style = "mics")` | `add_maternal_deaths()` — shared, switched |
| M7 fixture | tests |

What stays shared and must **not** be forked: varmap handling,
`check_varmap_cols()`, `get_ego_df()`, `get_sib_df()`, the summary block. Those
are already near-identical across the two existing preps; a third copy would make
three places to fix every future bug. `prep_mics_sib_histories()` should
pre-process the raw women's file into the shape those internals expect and then
delegate, not reimplement them.

### Proposed signature

    prep_mics_sib_histories(df,
                            survey,
                            varmap       = sibhist_varmap_mics6,
                            sib.df       = NULL,
                            layout       = c("wide", "long"),
                            id.vars      = c("hh1", "hh2", "ln"),
                            doi.var      = NULL,
                            doi.ym       = c("wm6y", "wm6m"),
                            lowercase    = TRUE,
                            weight.scale = 1,
                            add_maternal = FALSE,
                            na.action    = c("include", "exclude"),
                            keep_missing = FALSE,
                            keep_varmap_only = FALSE,
                            verbose      = TRUE)

Notes on the choices, since several are deliberate:

- **`survey` is required and has no default.** MICS has no `v000` equivalent, so
  the package cannot derive one and should not invent one. Making the caller
  name the survey (`"BJ2021"`) is honest and keeps MICS ids comparable with the
  DHS codes. See open decision 2.
- **`weight.scale = 1`** — `wmweight` is already normalized to mean 1. This is
  the single most consequential default in the signature; see M2.
- **`doi.var` before `doi.ym`.** If the released `wm.sav` ships a ready-made CMC,
  use it (`doi.var`); otherwise construct one from year and month (`doi.ym`).
  Audit Q7 is open on which applies, so support both rather than betting.
- **`sib.df`** is only meaningful when `layout = "long"`, and the function should
  error if one is supplied without the other. See M4.
- **`id.vars` / `doi.ym` are arguments, not constants**, because the real `.sav`
  variable names are unknown until a file arrives (M5) and MICS6 country
  customisations do vary.
- **`na.action`** is passed through to `add_maternal_deaths()`. See M6 — it is a
  statistical choice, not a coding detail.

### On "try `prep_nrsim_sib_histories()` first"

Still worth doing as a *probe* — that is how several items below were found — but
it is not a way *around* them. Both preps call the same internals, so both hit
the same hardcoded DHS assumptions. Use it to discover breakage, then fix the
breakage in the right place per the table above.


What the package requires today
----

Established by feeding minimal varmaps through `prep_nrsim_sib_histories()` and
reading the failures. This is the contract the MICS prep has to satisfy.

`get_ego_df()` requires, after varmap renaming:

| Column | Why |
|---|---|
| `age` | builds `age.cat` / `age.cat10`, unconditionally |
| `survey` | `cur.survey <- ego.dat$survey[1]` |

`get_sib_df()` requires on the ego data, because they are carried onto every
sibling row (`ego.vars` is hardcoded):

    caseid, wwgt, psu, doi, sex

and on the sibling data, because the derivation `case_when`s reference them
unconditionally:

    sib.sex, sib.alive, sib.age, sib.dob,
    sib.death.date, sib.death.yrsago, sib.death.age

Also hardcoded, and not covered by any guard:

- `sib.sex = ifelse(sib.sex == 2, 'f', 'm')` — anything not literally `2`,
  including a `9` don't-know, silently becomes `'m'`
- `doi` is treated as a **CMC** (century month code). Every date derivation is
  integer arithmetic in months off it.
- `aggregate_maternal_estimates()` hardcodes `ego.id = 'caseid'` and
  `sib.frame.indicator = 'in.F'`, so those names are not negotiable downstream.


The work
----

### M1. `sib.dob` and `sib.death.date` must become optional inputs

**This blocks everything else, and it is a change to code added in the last
batch.** MICS collects ages and years-ago-died, not CMC dates: the draft varmap
deliberately has no `sib.dob` and no `sib.death.date`. But `get_sib_df()`'s new
required-column guard lists both, so it rejects the MICS varmap outright:

    The sibling data is missing required column(s): sib.dob, sib.death.date.

The guard is not wrong to want them — the derivations below reference them — but
it conflates *inputs the varmap must supply* with *columns that must exist by
the time the derivations run*.

**Change:** split the two lists. `sib.sex`, `sib.alive`, `sib.age`,
`sib.death.yrsago` and `sib.death.age` stay required as inputs. `sib.dob` and
`sib.death.date` become *derived*: if absent, initialize to `NA_real_` before the
`case_when` block and let the existing derivations fill them.

**Then check the derivations actually close for MICS.** They should, but this is
the assumption most likely to be quietly wrong, so assert it in the fixture
tests rather than eyeballing it:

| Target | Derivation | MICS input |
|---|---|---|
| `sib.death.date` | `doi - (12*yrsago + 6)` | MM8 years ago |
| `sib.dob`, living sib | `doi - (12*age + 6)` | MM7 current age |
| `sib.dob`, dead sib | `death.date - 12*death.age` | MM9 age at death |

Note the third row works only because of the D4 fix in `bc0c38b`, which
re-guarded that branch on `sib.death.age` instead of `sib.death.yrsago`. It
matters for the subset of MICS rows where years-ago is missing or don't-know but
age-at-death is reported — not for the common case, where both are present.

### M2. Construct the ego identifiers MICS does not have

**The core of `prep_mics_sib_histories()`.** The draft varmap has **no ego rows
at all**; every one of these has to be built by the prep, before the shared
`get_ego_df()` / `get_sib_df()` machinery runs.

- **`caseid`** — MICS has no single respondent id. Build it from cluster +
  household + line number (`HH1`, `HH2`, `LN`). Must be unique:
  `attributes.to.long()` errors on duplicates, and `aggregate_maternal_estimates()`
  hardcodes the name.
- **`doi` as a CMC** — MICS records the interview date as WM6D/WM6M/WM6Y.
  Convert: `cmc = (year - 1900) * 12 + month`. **Audit Q7 is open** on whether
  `wm.sav` also ships a ready-made CMC; if it does, prefer it.
- **`survey`** — MICS has no `v000`. Construct something stable and comparable
  with the DHS codes, e.g. country code + round.
- **`psu`** — MICS uses `HH1` (cluster) as the PSU in its standard designs;
  confirm against the survey's own documentation rather than assuming.
- **`age`** — respondent age in single years, from `WB4`.
- **`wwgt`** — `wmweight`, **already normalized to mean 1**, so the prep must
  pass `weight.scale = 1`. This is now a parameter rather than a hazard (fixed in
  `c3b3be3`), but it remains the single most consequential thing to get wrong:
  nothing downstream would complain and every estimate would be off by 10^6.

### M3. Lowercase the variable names

MICS `.sav` files are conventionally uppercase (`MM5_1`); DHS `.dta` are
lowercase. `attributes.to.long()`'s regex is case-sensitive, so an uppercase file
against a lowercase varmap silently matches nothing and produces zero sibling
rows — no error, just an empty roster.

**Change:** a `lowercase = TRUE` argument on the prep that lowercases `names(df)`
up front. Cheaper and less fragile than making the regex case-insensitive, which
would risk collisions in files that use case to distinguish variables.

Guard it in the fixture tests: an uppercase input must produce the same sibling
row count as its lowercase twin.

### M4. Handle both roster layouts

The audit's Q4, still open. The prep should take a `layout` argument so either
answer is absorbed rather than requiring a rewrite:

| If the released `wm.sav` … | then … |
|---|---|
| carries the roster **wide** (`mm4_1`, `mm4_2`, …) | `attributes.to.long()` works untouched — its default `sep = "\\.\|_"` already matches |
| ships a **separate long file** | skip the reshape entirely; rename and join |

Write the wide path first — it is what the model questionnaire implies and what
the existing machinery already handles — but keep the seam.

### M5. The MICS varmap, and the `MM16` collision

Ship `sibhist_varmap_mics6` as package data, in the same five-column format as
the DHS varmaps. Generate it from the audit crosswalk so the two cannot drift.
The draft is correct in its mapping of *concepts* and provisional in its
*spelling*, since the real `.sav` variable names are unknown until a file
arrives.

Sibling mappings from the draft:

    mm5  -> sib.sex            mm9  -> sib.death.age
    mm6  -> sib.alive          mm12 -> sib.died.pregnant.mics
    mm7  -> sib.age            mm13 -> sib.died.childbirth.mics
    mm8  -> sib.death.yrsago   mm14 -> sib.died.postpartum.mics
                               mm15 -> sib.num.children

Deliberately absent: `sib.dob`, `sib.death.date` (see M1).

> ⚠ **`MM16` means opposite things in the two systems.** In MICS it is loop
> control ("is there a younger sibling?"). In DHS-VII+ `mm16` is "died of
> violence or accident". Mapping one to the other would silently reclassify
> deaths. The crosswalk already records it as `status = collision` and the
> varmap generator refuses to emit it; the prep should *also* error if a varmap
> maps `mm16 -> sib.died.accident`, so the guard survives someone hand-editing
> the CSV.

**Consequence worth stating in the package docs:** MICS has no violence/accident
item at all, so it supports **pregnancy-related**, not strictly **maternal**,
mortality — the same limitation as DHS phases 2–6. The existing
`if ('sib.died.accident' %in% names(sib_df))` guard in `add_maternal_deaths()`
already handles this correctly; MICS will simply always take the fallback branch.
This strengthens the case for "pregnancy-related" as the paper's label.

### M6. `add_maternal_deaths()` needs a MICS branch

DHS keys off `sib.died.pregnant` codes 2/3/4/5 combined with
`sib.time.delivery.death` bands. MICS asks three separate binaries instead —
**MM12** pregnant when she died, **MM13** died during childbirth, **MM14** died
within two months of the end of a pregnancy — and has no time-since-delivery
variable at all.

**Change:**

    add_maternal_deaths(sib_df,
                        style = c("dhs", "mics"),
                        na.action = c("include", "exclude"),
                        keep_missing = FALSE, verbose = TRUE)

Factor the existing condition into an internal `is_preg_related_dhs()` and add
`is_preg_related_mics()`, each returning a logical vector. Everything else — the
`-1` sentinel, the `NA` fill, the male-blanking, the `sib.died.accident` guard —
is unchanged and shared. `style` defaults to `"dhs"`, so every existing call site
is untouched.

> **`na.action` is a substantive statistical choice, not a coding detail.** The
> DHS branch currently treats a *missing* `sib.time.delivery.death` as
> **include** — the `is.na(...)` sits inside the OR. MICS needs the analogous
> decision made explicitly for don't-know/missing on each of MM12/13/14, and it
> changes the numerator of every MICS estimate. Making it an argument turns the
> sensitivity check into a one-line change. **Whatever default is chosen, the
> reasoning goes in the docs.** Decide this before writing the branch, not after.

### M7. The synthetic fixture and the contract tests

This is what takes MICS data-access lead time off the critical path, and it
should be built **first**, before M1–M6, so the rest is test-driven.

Build a small MICS women's file in code — say 10 respondents × ≤5 siblings, wide
layout, uppercase names, known answers — as
`tests/testthat/helper-simulate-mics.R`.

**Test-only, not shipped package data.** A fabricated fixture sitting in `data/`
looking like real MICS invites someone to mistake it for one. Add
`model_mics_dat` only once a real file is in hand, as `model_dhs_dat` already is.

Assert the contract:

- row counts, and `sibid` uniqueness
- **weights not divided by 1e6** — guards the M2 hazard
- `sib.dob` non-`NA` for every sibling with the inputs to derive it, and `doi` a
  plausible CMC — guards M1, which is the assumption most likely to be quietly
  wrong
- uppercase input gives the same sibling row count as lowercase — guards M3
- `sib.preg_related.death.date == sib.death.date` exactly where MM12/13/14 fire,
  and `-1` everywhere else — guards M6
- `NA` for male siblings
- a varmap mapping `mm16 -> sib.died.accident` errors — guards M5
- `sib.sex` recode: a `9` don't-know does not silently become `'m'`

That makes essentially all of M1–M6 testable before any real MICS file exists.


Sequence
----

1. **Request MICS microdata access now**, in parallel with everything else. It is
   the long pole, nothing in the package shortens it, and M4 cannot be resolved
   without it.
2. **M7** — the fixture and the contract tests. First, so the rest is
   test-driven.
3. **M1** — make `sib.dob` / `sib.death.date` derived rather than required, in
   the shared `get_sib_df()`. Small and self-contained, and until it is done no
   MICS varmap can run at all.
4. **`prep_mics_sib_histories()` skeleton** — the signature above, `lowercase`
   (M3), and straight delegation to the shared internals. Gets an exported
   function that runs end-to-end against the fixture before any of the harder
   construction lands, so M2 has something to grow inside.
5. **M2** — the constructed `caseid`, `doi`, `survey` and `psu`. The bulk of the
   function.
6. **M5, M6** — the varmap and the `add_maternal_deaths()` branch. M6 needs the
   `na.action` decision made first.
7. **M4** — resolve the layout once a real file is in hand; write the wide path
   before then.
8. **Validate against a real survey**, and only then add `model_mics_dat`.


Open decisions
----

Both belong to the analyst, not to the implementation:

1. **`na.action` for MICS don't-know/missing on MM12/13/14** (M6). Changes the
   numerator of every MICS estimate. Needs a default and a documented rationale.
2. **How `survey` should be constructed** (M2) so MICS and DHS survey ids are
   comparable in the same analysis, given MICS has no `v000` equivalent.

And one carried over from `PACKAGE-HANDOFF.md` that touches MICS only indirectly:

3. **E4** — whether `adj.factor` is meant to be a global scalar. MICS
   respondents are women, as DHS respondents are, so the answer is the same for
   both; but the two-sex path is now reachable and the question is live.


Things that will not work, and are not bugs
----

Worth writing down so they are not rediscovered as defects:

- **No maternal mortality from MICS, only pregnancy-related.** No
  violence/accident item exists. See M5.
- **No male sibling estimates with a visibility adjustment.** MICS interviews
  women only, so `only_females = FALSE` will warn and return `NA` for male
  siblings — correctly. See E3 in `PACKAGE-HANDOFF.md`.
- **`sib.dob` and `sib.death.date` are approximations for MICS**, not reported
  values: both are derived from ages and years-ago on a mid-year assumption. The
  DHS supplies real CMC dates for these. Any comparison of MICS and DHS estimates
  should say so, because the derived dates carry heaping that the DHS ones do not.
