Handoff: changes needed in `siblingsurvival`
====

For a session working in **`dfeehan/siblingsurvival`**, not in this repo.

Everything here was found while reorganizing the maternal mortality analysis
(`~/Dropbox/maternal-mortality/maternal-mortality`) and starting a MICS
feasibility audit. The analysis repo deliberately contains **no** package
changes; it documents the interface it needs and waits. See `STATUS.md` there
for the analysis-side picture.

Written 2026-08-20 against upstream `main` (pushed 2026-03-03, `Version: 0.3.0`)
and the locally installed build (`0.2.0`, sha `41748b0`, built 2025-10-23).
**Upstream is well ahead of what is installed** — check upstream before assuming
anything below is still outstanding. Each item says whether it is done.

**Revised 2026-08-20 in the `siblingsurvival` repo itself**, on branch `mics`.
Every claim below was checked against the code. Blockquoted notes record what was
verified, what was corrected, and what was completed; item D4 is new.
The work-in-progress is **uncommitted** on `mics`.


Why any of this matters
----

Two things drive the list.

**1. The analysis pipeline cannot run at all right now.** It dies inside
`prep_dhs_sib_histories()`. That fix is already upstream but not in any build
the analysis can install, so the practical blocker is a release, not a code
change (item A1).

**2. We want to add MICS surveys alongside DHS.** MICS6 collects a full
per-sibling roster — verified against the MICS6 model Individual Women's
questionnaire — so the individual visibility estimator applies. But MICS differs
from DHS in ways the package currently hardcodes as DHS assumptions (items B1–B4).

There is also a third, smaller theme: the analysis repo has accumulated glue
that exists only because certain package internals are unexported or recompute
things the caller already has (items C1–C3).


A. Unblock the analysis pipeline
----

### A1. Release the `select_()` fix — **already fixed upstream, needs a release**

`attributes.to.long()` used `dplyr::select_()`, defunct since dplyr 1.1. Any
call to `prep_dhs_sib_histories()` fails immediately on dplyr 1.2.0:

    `select_()` was deprecated in dplyr 0.7.0 and is now defunct.

Fixed upstream in `dbb9e21` (2026-02-25), recorded in `NEWS.md` under 0.3.0. The
installed build predates it.

**Action:** confirm the fix is on `main`, bump/tag, and make sure the analysis
repo can `install_github()` a build that has it. Nothing else in the analysis
can be verified end-to-end until this lands.

> **Verified 2026-08-20:** `main` == `origin/main`, and `select_()` is gone from
> `R/` (only a commented-out line survives at `R/occ_exp.R:172`). `DESCRIPTION`
> already read `0.3.0`. So there is **no code work here at all** — the analysis
> repo just needs to reinstall from `main`. Note the repo has **no git tags
> whatsoever**, so "bump/tag" is not an existing practice; cutting a first
> `v0.3.0` tag is optional but would make the build referenceable.

### A2. Document the sibling-id contract — *decide, then document*

`prep_dhs_sib_histories()` creates the sibling id column as **`sibid`**
(`R/prep_dhs_sib_histories.R:488`), and nothing anywhere creates `sib.id`. But
`sibling_estimator()` takes `sib.id` as a parameter and resolves it with
`!!sym(sib.id)`, so a caller passing `sib.id = "sib.id"` — which the analysis
code has been doing — errors.

This is a caller bug, and it is fixed on the analysis side (one argument default
in `code/R/estimate.R`). But it is worth deciding whether the package should
help: either default `sibling_estimator(sib.id = "sibid")` to match what prep
produces, or fail with a message naming the columns that do exist. Right now the
error is opaque.

> **DONE 2026-08-20 — both.** `sibling_estimator()` now defaults
> `sib.id = 'sibid'`, and checks up front that the columns named by `ego.id`,
> `sib.id`, `sib.frame.indicator`, `sib.sex` and `weights` all exist, erroring
> with a message that names the offending arguments *and* lists the columns
> present. Callers passing `sib.id` explicitly are unaffected.


B. MICS support — **moved to `MICS-PLAN.md`**
----

> **2026-08-20:** now that A, C, D and E are done, the MICS work is the only
> thing left, and it has grown its own structure. It lives in **`MICS-PLAN.md`**,
> which supersedes this section: items B1–B5 map onto M1–M7 there, with the
> requirement list corrected and expanded, the layout/access dependency made
> explicit, and one new blocking item (M1) that this section did not anticipate.
>
> The text below is kept as the original statement of the problem. Where the two
> disagree, `MICS-PLAN.md` is current.

The substantive work. Sequenced **after** the feasibility audit answers whether
the sibling roster survives into released MICS microdata and in what layout —
that answer determines the shape of B1. The audit lives at
`code/mics-audit/mics-feasibility.qmd` in the analysis repo.

### B1. `prep_mics_sib_histories()`

Mirror `prep_dhs_sib_histories()` in signature and return value
(`list(survey, ego.dat, sib.dat, summ)`). `prep_nrsim_sib_histories()` already
exists as a generic varmap-driven prep — **try it with a MICS varmap before
writing anything new.**

Six things MICS needs that DHS does not (the last two found 2026-08-20):

- **Construct `caseid`.** MICS has no single respondent id; it has to be built
  from cluster + household + line number. `aggregate_maternal_estimates()`
  hardcodes `ego.id = "caseid"` internally, so the name is not negotiable.
- **Construct `survey`.** MICS has no `v000`, and `get_ego_df()` hard-`stop()`s
  without a `survey` column (`R/prep_dhs_sib_histories.R:397`).
- **Lowercase variable names.** MICS `.sav` files are conventionally uppercase,
  DHS `.dta` lowercase, and `attributes.to.long()`'s regex is case-sensitive.
- **Handle both layouts.** If the roster ships wide in `wm.sav` (`mm4_1`,
  `mm4_2`, …) then `attributes.to.long()` works untouched — its default
  `sep = "\\.|_"` already matches. If it ships as a separate long file, the prep
  skips the reshape entirely and just renames plus joins. Take a `layout`
  argument so either answer is absorbed rather than requiring a rewrite.
- **Construct `doi` as a CMC month, and `psu`.** `get_sib_df()` hardcodes
  `ego.vars = c('caseid','wwgt','psu','doi','sex')`
  (`R/prep_dhs_sib_histories.R:416-419`), and *every* date derivation in it —
  `approx_death_date()`, `approx_birth_date_from_age()`,
  `approx_birth_date_from_death()` — is arithmetic in CMC months off `doi`.
  MICS records the interview date as WM6Y/WM6M, not a CMC, so the prep has to
  build one.
- **Check the sex recode.** `R/prep_dhs_sib_histories.R:421` hardcodes
  `sib.sex = ifelse(sib.sex == 2, 'f', 'm')`. Anything that is not literally
  `2` — including a `9` don't-know — silently becomes `'m'`. Same class of
  silent-wrong failure as B2, and it needs the MICS MM5 coding confirmed.
  (The draft varmap already carries this warning in its `comments` column.)
- **Map an ego `age` in single years.** `get_ego_df()` builds `age.cat` and
  `age.cat10` from a column called `age`, unconditionally. Found by feeding a
  minimal varmap through `prep_nrsim_sib_histories()`.
- **Map `sib.death.yrsago` *and* `sib.death.age`.** The birth- and death-date
  derivations in `get_sib_df()` reference both unconditionally, so a varmap that
  omits either fails. The MICS draft varmap has both (`mm8`, `mm9`), so this is
  satisfied — but see the note under B4: "unconditional on survey family" also
  means "unconditional on variable presence".

  Both of these now produce a message naming the missing column rather than an
  opaque `case_when()` error (done 2026-08-20).

> **On "try `prep_nrsim_sib_histories()` first" (verified 2026-08-20):** worth
> doing, but be clear about what it buys. `prep_nrsim_sib_histories()` calls the
> *same* `get_ego_df()` and `get_sib_df()` internals, so it hits the weight
> scaling (B2), the `doi`/`psu` requirement, and the sex recode identically. It
> is a good starting point; it is not a way around B1's hazards.

### B2. The weight-scaling hazard — **highest-risk item on this page**

`get_ego_df()` divides any column named `wwgt` by `1e6`
(`R/prep_dhs_sib_histories.R:373-380`), announcing "Found wwgt column; assuming
we have a DHS survey and scaling weights."

MICS `wmweight` is already normalized to mean 1. A varmap that maps it to `wwgt`
— the obvious thing to write — would silently divide every MICS weight by a
million. Nothing downstream would complain and every MICS estimate would be
wrong by a factor of 10^6.

**Suggested change:** add `weight.scale = 1e6` to `get_ego_df()`, keeping the
current DHS behaviour as the default, and have the MICS prep pass `1`. Three
lines. There is already a `TODO` at line 352 anticipating exactly this
("figure out when/where to prep weights (ie, for DHS divide by 1e6)").

> **DONE 2026-08-20 — and this was a live bug, not just a MICS hazard.**
>
> `prep_nrsim_sib_histories()` — the function whose whole purpose is *non-DHS*
> data — calls the same `get_ego_df()`, which keys off the *column name* `wwgt`
> rather than the survey family. Every varmap has to map its weight to `wwgt`,
> because everything downstream expects that name. So non-DHS data going through
> the existing generic prep was already wrong by 10^6. Demonstrated on a
> synthetic NR-SIM file with mean-1 weights:
>
>     input  weights: 1 1.2 0.8
>     output weights: 1e-06 1.2e-06 8e-07
>
> `weight.scale` is now plumbed through `get_ego_df()`,
> `prep_dhs_sib_histories()` (default `1e6`, unchanged) and
> `prep_nrsim_sib_histories()` (default **`1`**). The nrsim default is a
> deliberate behaviour change, recorded in `NEWS.md`; it changes results by a
> factor of a million, so it will be obvious rather than subtle. Pass
> `weight.scale = 1e6` to restore the old behaviour.
>
> A new exported `prep_mics_sib_histories()` should carry `weight.scale = 1` as
> its default — see `MICS-PLAN.md`, where that function is now a committed
> deliverable rather than an optional wrapper.

### B3. `add_maternal_deaths()` needs a MICS branch

Currently `add_maternal_deaths(sib_df, keep_missing, verbose)` at
`R/prep_dhs_sib_histories.R:518`, and its logic is DHS-specific: it keys off
`sib.died.pregnant` codes 2/3/4/5 combined with `sib.time.delivery.death` bands.

MICS asks three separate binaries instead: **MM12** (pregnant when she died),
**MM13** (died during childbirth), **MM14** (died within two months of the end of
a pregnancy). There is no time-since-delivery variable.

**Suggested change:**

    add_maternal_deaths(sib_df,
                        style = c("dhs", "mics"),
                        na.action = c("include", "exclude"),
                        keep_missing = FALSE, verbose = TRUE)

Factor the existing condition into an internal `is_preg_related_dhs()` and add
`is_preg_related_mics()`, each returning a logical vector. Everything else — the
`-1` sentinel, the `NA` fill, the male-blanking, the `sib.died.accident` guard —
is unchanged and shared. `style` defaults to `"dhs"`, so every existing call site
is untouched.

**`na.action` is a substantive statistical choice, not a coding detail.** The DHS
branch currently treats a *missing* `sib.time.delivery.death` as **include** —
the `is.na(...)` sits inside the OR. MICS needs the analogous decision made
explicitly for don't-know/missing on each of MM12/13/14, and it changes the
numerator of every MICS estimate. Making it an argument turns the sensitivity
check into a one-line change. Whatever default you pick, document the reasoning.

### B4. The MICS varmap, and the `MM16` collision

A `sibhist_varmap_mics6` package data object, in the same five-column format as
the DHS varmaps. A draft generated from the audit crosswalk is at
`mics_varmaps/sibhist_varmap_mics6_DRAFT.csv` in the analysis repo — correct in
its mapping of *concepts*, provisional in its *spelling*, since the real `.sav`
variable names are still unknown.

Mapped: `mm5 -> sib.sex`, `mm6 -> sib.alive`, `mm7 -> sib.age`,
`mm8 -> sib.death.yrsago`, `mm9 -> sib.death.age`, `mm15 -> sib.num.children`,
plus MM12/13/14 for the pregnancy items.

Deliberately **absent**: `sib.dob` and `sib.death.date`. MICS collects ages and
years-ago-died, not CMC dates — and `get_sib_df()` already derives both via
`approx_death_date()`, `approx_birth_date_from_age()` and
`approx_birth_date_from_death()`. Those `case_when`s are unconditional on survey
family, so this *should* work untouched. It is the MICS assumption most likely to
be quietly wrong; check it against real data first.

**⚠ `MM16` means opposite things in the two systems.** In MICS it is loop control
("is there a younger sibling?"). In DHS-VII+ `mm16` is "died of violence or
accident". Mapping one to the other would silently reclassify deaths. Suggest a
guard: `prep_mics_sib_histories()` errors if the varmap maps
`mm16 -> sib.died.accident`.

Consequence worth stating in the docs: MICS has no violence/accident item at all,
so it supports **pregnancy-related**, not strictly **maternal**, mortality —
the same limitation as DHS phases 2–6. The existing
`if ("sib.died.accident" %in% names(sib_df))` guard already handles this
correctly; MICS will simply always take the fallback branch.

### B5. Test against a synthetic fixture, not real data

This is what takes MICS data-access lead time off the critical path. MICS
microdata needs per-survey registration and approval; none is downloaded yet.

`tests/testthat/` already has `helper-simulate.R` and `test_maternal.R`, and the
package already ships `model_dhs_dat` — so the precedent exists. Build a small
synthetic MICS women's file in code (say 10 respondents × ≤5 siblings, wide
layout, known answers) and assert the contract:

- row counts and `sibid` uniqueness
- `sib.preg_related.death.date == sib.death.date` exactly where MM12/13/14 fire,
  and `-1` everywhere else
- `NA` for male siblings
- **weights not divided by 1e6** (guards B2)
- **`sib.dob` is non-`NA` for every sibling with the inputs to derive it**
  (guards D4, and guards B4's assumption that MICS can omit `sib.dob` entirely)
- **`doi` is a plausible CMC month** and the derived dates fall in range

That makes essentially all of B1–B4 testable before any real MICS file exists.
Add a `model_mics_dat` once one is in hand.


C. Reduce the glue the analysis repo has to carry
----

Lower priority than A and B, and each is a judgment call about how much belongs
in the package. Listed because all three produce the same smell in the caller.

### C1. Export `get_ego_age_distn()` — `get_visibility()` is already done

The analysis calls two internals directly:

    siblingsurvival:::get_visibility(ego, "caseid", sib, "in.F")
    siblingsurvival:::get_ego_age_distn(ego, only_females = FALSE)

`get_visibility()` **is now exported upstream in 0.3.0**. `get_ego_age_distn()`
is not. Both are genuinely part of the analysis vocabulary — the age distribution
of respondents is the reference population the aggregation weights by — so the
`:::` is a sign of a missing export rather than of a caller reaching where it
shouldn't.

> **DONE 2026-08-20.** `get_ego_age_distn()` is exported. Both `:::` calls in the
> analysis repo can now drop the prefix.

### C2. Let `aggregate_maternal_estimates()` accept precomputed inputs

`aggregate_maternal_estimates()` calls `get_ego_age_distn()` and
`get_visibility()` internally (`R/maternal_estimators.R:36,39`). The analysis
also needs both for its age-specific output, and it calls
`aggregate_maternal_estimates()` twice per survey (all-cause and
pregnancy-related).

Net effect: **visibility and the respondent age distribution are computed three
times per survey**, from identical inputs.

**Suggested change:** optional `age_prop = NULL` and `vis_res = NULL` arguments,
computed internally when not supplied. Backwards compatible, and it removes the
redundancy without changing any result.

> **DONE 2026-08-20**, exactly as suggested, with a test asserting that the
> results are identical whether the two are computed internally or supplied.

### C3. Make the reproductive age-group filter a package-level definition

    filter(! sib.age %in% c("[50,55)", "[55,60)", "[60,65)"))

appears twice inside `R/maternal_estimators.R` (lines 58 and 158) and again in
the analysis repo's `code/R/estimate.R:137`. Three copies of one demographic
definition, in two repositories. If the age grouping ever changes they diverge
silently and nothing errors.

Suggest an exported constant or accessor (`reproductive_age_groups()`), used
everywhere, so the definition has one home.

> **DONE 2026-08-20.** `reproductive_age_groups()` is exported and used by both
> `aggregate_maternal_estimates()` and `get_ego_age_distn()` — which, note, had a
> *third* copy of the list, in inclusion rather than exclusion form.
>
> The two call sites in `maternal_estimators.R` were converted from excluding
> `[50,55)`/`[55,60)`/`[60,65)` to including the seven reproductive groups. For
> every built-in `cell_config()` age grouping (all of which start at 15) the two
> are identical, and there is a test pinning that down — so **no result changes**.
> The inclusion form is strictly more correct for a custom `age.groups` that
> reaches below 15: under the old exclusion filter a `[10,15)` group survived the
> filter, then left-joined to a missing `agegrp_prop` and propagated `NA` into
> the aggregate.
>
> Still outstanding: the third copy in the analysis repo's
> `code/R/estimate.R:137` should be replaced with a call to
> `siblingsurvival::reproductive_age_groups()`.


D. Small correctness and robustness fixes
----

### D1. Missing **sibling** variables pass silently

`prep_dhs_sib_histories()` warns about columns missing from the dataset, but only
for ego variables — `miss_col` is computed from `resp.attrib` alone
(`R/prep_dhs_sib_histories.R:69`). Sibling variables (`sibvar == 1`) are never
checked, so a genuinely absent sibling variable in an older DHS phase or a MICS
file produces no message at all.

This matters more once MICS exists, since MICS is missing several variables the
DHS varmaps take for granted.

> **DONE 2026-08-20.** Both prep functions (the check was duplicated in
> `prep_nrsim_sib_histories()` too) now call a shared internal
> `check_varmap_cols()`, and `summ` gained `sib.cols.notfound` alongside
> `ego.cols.notfound`.
>
> One wrinkle worth recording: sibling variables **cannot** be checked with
> `%in% names(df)` the way ego variables are. `mm3` is never a column; the data
> has `mm3_01`, `mm3_02`, … So the check matches prefixes with the same regular
> expression `attributes.to.long()` uses, and takes the same `sep` argument.
>
> This also turned up something about the package's own example data: running it
> on `model_dhs_dat` with `sibhist_varmap_dhs6` reports **`mm16` missing**. The
> shipped model dataset has no violence/accident variable, which is why
> `add_maternal_deaths()` on it always takes the pregnancy-related fallback
> branch. Previously invisible.
>
> Note the failure mode was not quite as described: a missing sibling variable
> doesn't pass silently *downstream* — it crashes inside `get_sib_df()` with
> `object 'sib.age' not found`. What was missing was any message saying **which**
> variable, which is what the warning now supplies.

### D2. `if (length(miss_col > 0))` — readability, not a bug

`R/prep_dhs_sib_histories.R:71`. Should be `length(miss_col) > 0`. It happens to
behave correctly for both the empty and non-empty cases, so this is tidying, not
a fix. Worth doing while you are in the file for D1.

> **DONE 2026-08-20 — and there were three sites, not one.** The same pattern was
> duplicated in `prep_nrsim_sib_histories()` (both are gone now, replaced by
> `check_varmap_cols()`), and a third lives in `R/attributes_to_long.R:162`:
>
>     if(length(intersect(names(ego.vars), names(these.altercols)) > 0))
>
> That third one is the interesting case. It *does* fire correctly, but only by
> accident: when the intersection is non-empty, `"somename" > 0` is `TRUE` via
> string comparison, so `length()` is 1 and `if(1)` is `TRUE`. Fixed.

### D3. Check the DHS-II varmap against the documented questionnaire history

`sibhist_varmap_dhs2` includes `mm15` (sibling year of death), but the analysis
repo's `dhs_varmaps/README.md` states that DHS-III introduced MM15. One of the
two is wrong. The DHS 2–6 varmaps are otherwise identical to DHS-7 apart from
`mm16`.

> **Still open, and larger than described (checked 2026-08-20).** Diffing the
> sibling rows of all seven shipped varmaps:
>
> - DHS **2, 3, 4, 5** are byte-identical to one another.
> - DHS **6, 7, 8** are those plus one row, `mm16 -> sib.died.accident`.
>
> So the "identical apart from `mm16`" split is at 5/6, not 6/7 — `mm16` is
> already in the DHS-6 varmap.
>
> More importantly, `sibhist_varmap_dhs2` doesn't merely include `mm15`; it
> carries the **entire DHS-III+ numbering**: `mm5 -> sib.marital.status`,
> `mm9 -> sib.died.pregnant`, `mm12 -> sib.time.delivery.death`,
> `mm14 -> sib.num.children`. If the README is right that the MM numbering
> changed at DHS-III, then the whole DHS-2 sibling block is suspect, not one row.
> Resolve against the DHS questionnaire history before using
> `sibhist_varmap_dhs2` on a real DHS-II file.

### D4. `sib.dob` derivation was guarded on the wrong variable — **DONE 2026-08-20**

Found while verifying B4's assumption that MICS can rely on the unconditional
`case_when` derivations. `R/prep_dhs_sib_histories.R` derived a missing
`sib.dob` from date-of-death and age-at-death, but guarded the condition on
`sib.death.yrsago`:

    mutate(sib.dob = case_when((is.na(sib.dob) &
                                  (! is.na(sib.death.yrsago)) &   # <- wrong variable
                                  (! is.na(sib.death.date))) ~
                                 approx_birth_date_from_death(sib.death.date,
                                                              sib.death.age),
                               TRUE ~ sib.dob))

A sibling with a known age at death but no years-since-death got a silent `NA`
birth date, and one with years-since-death but no age at death got `NA` from the
arithmetic. Now guarded on `sib.death.age`, with a regression test that fails
against the old code.

**This lands directly on MICS**, because B4 deliberately omits `sib.dob` from the
varmap and leans entirely on these derivations — so B5 should assert on the
`sib.dob` non-`NA` rate, not just on row counts.


E. The `maternal_estimators.R` TODOs — **DONE 2026-08-20**
----

Added after the initial handoff. `R/maternal_estimators.R` carried six `TODO`
markers, including `TODO NEED TO ADAPT BOOTSTRAP VERSION BELOW` and
`TODO LEFT OFF HERE`. **None were stale**; each described a live defect. The
bootstrap block had been written but never reconciled against the
point-estimate block, and the two had drifted.

### E1. The bootstrap join dropped the sex key

The point-estimate branch joined the visibility results on age *and* sex; the
bootstrap branch joined on age alone. When `ego_vis_agg` contains both sexes,
every bootstrap row matches twice.

Demonstrated with bootstrap weights set equal to the real weights, which must
reproduce the point estimate exactly:

    point ind.est      : 0.001383984
    bootstrap mean     : 0.002767967
    ratio              : 2

So the confidence intervals were centred on twice the point estimate, silently.

**This is currently unreachable in the analysis** — DHS respondents are all
female, so `ego_vis_agg` has a single sex and the duplication never fires. But
the analysis carries the same landmine in its own code:
`code/R/estimate.R:134` does
`left_join(vis_df$ego_vis_agg, by = c("sib.age" = "age.cat"))` — same missing
key. Benign today for the same reason; worth fixing there too.

This is what the `TODO` at the old line 86 was asking ("test that this works
even if `vis_res$ego_vis_agg` has males and females?"). The answer was no.

### E2. `only_females = FALSE` errored outright

    Join columns in `y` must be present in the data.

Three defects stacked in one branch:

- it joined `age_prop` on a `sex` column that `get_ego_age_distn()` **never
  produced** — that function grouped by `age.cat` only, in both modes;
- it then grouped by `sex`, which the join consumes into `sib.sex`;
- it removed a `dummy` column that its own grouping never created.

The path had almost certainly never been run. The analysis calls
`aggregate_maternal_estimates(..., only_females = TRUE)` and uses
`get_ego_age_distn(only_females = FALSE)` directly, which works standalone.

**Resolved as:** per-sex reference distributions.
`get_ego_age_distn(only_females = FALSE)` now returns a `sex` column with
`agegrp_prop` summing to 1 *within* each sex, and both joins key on sex.
Results are reported per sibling sex. `only_females = TRUE` is untouched.

> ⚠ **This changes the return shape of `get_ego_age_distn(only_females = FALSE)`**,
> which the analysis repo calls at `code/R/estimate.R:129` and then joins by
> `age.cat` alone at `:135`. That join will now also pull in a `sex` column and
> may collide with the `sex` coming from the `ego_vis_agg` join one line above.
> **Check that call site before re-running the analysis.**

### E3. An uninterviewed sex now warns instead of returning a silent NA

A reference age distribution and a visibility adjustment can only come from
respondents of the same sex. In the usual survey only women are interviewed, so
male sibling estimates under `only_females = FALSE` are `NA`. That is the honest
answer — you cannot estimate a visibility adjustment for a sex that was never
interviewed — but it is now announced rather than silent.

### E4. `adj.factor` and `adj.factor.allage` are global scalars — **open, needs a decision**

Pre-existing behaviour, deliberately left alone in the fixes above, but it
becomes *visible* now that the two-sex path actually runs, so it needs an answer.

`get_visibility()` (`R/get_sibship_visibility.R:121-148`) computes three
adjustment factors, and two of the three are **scalars over the entire
respondent sample**, computed before any grouping:

    S.hat               <- wh.mean(ego_vis$y.F + 1, ego_vis$.weight)   # ALL egos
    S.adj.factor        <- 1 - (1/S.hat)

    y.F.bar             <- weighted.mean(ego_vis$y.F, ego_vis$.weight) # ALL egos
    approx.S.adj.factor <- 1 - (1/approx.S.hat)

    ego_vis_agg <- ego_vis %>%
      group_by(sex, .agecat) %>%
      summarise(y.F.bar = weighted.mean(y.F, .weight), ...) %>%
      mutate(adj.factor         = S.adj.factor,          # <- recycled scalar
             adj.factor.allage  = approx.S.adj.factor,   # <- recycled scalar
             adj.factor.agespec = y.F.bar/(y.F.bar + 1)) # <- group-specific

Note that the `y.F.bar` inside `summarise()` shadows the global one, so only
`adj.factor.agespec` varies by `(sex, age)`. The other two are constant down
every row of `ego_vis_agg`.

Demonstrated on identical respondent data where only the sex *labels* differ:

    all female  adj.factor=0.4295711  allage=0.5772943  agespec range=[0.5041, 0.6212]
    half male   adj.factor=0.4295711  allage=0.5772943  agespec range=[0.4922, 0.6288]

Three separate questions here, worth separating:

1. **Is it intended?** For `adj.factor.allage` the name says so — "allage" is an
   all-ages approximation, and a constant is exactly right. For `adj.factor`,
   the unqualified name gives no such signal, yet it is equally global. If it is
   meant to be global, it should be named to say so (`adj.factor.overall`?); if
   it is meant to vary, it is wrong.

2. **Should it respect `only_females`?** `S.hat` is computed over *all* egos
   regardless of what the caller asked for. With mixed-sex respondents, an
   `only_females = TRUE` aggregate therefore carries an adjustment factor
   computed partly from male respondents. Invisible in the DHS, where all
   respondents are women; not invisible in anything else.

3. **`adj.factor[1]` is a fragile idiom.** `aggregate_maternal_estimates()`
   summarises these two with `adj.factor[1]`, which is correct *only because*
   they are constant within the group. If either ever becomes group-varying,
   `[1]` silently picks an arbitrary row instead of erroring. Worth replacing
   with something that asserts constancy.

None of this changes any current DHS result — all three factors are what they
have always been. It matters for MICS only insofar as MICS respondents are also
all women, so the answer there is the same. It matters most for the
`only_females = FALSE` path, which is now reachable for the first time.


Suggested order
----

*(revised 2026-08-20; struck items are done)*

1. ~~**A1**~~ — no code work needed; the fix is on `origin/main`. The analysis
   repo just needs to reinstall. Optionally cut a first `v0.3.0` tag.
2. ~~**C1, C3, A2, D1, D2, D4**~~ — done on the `mics` branch, 22 new tests in
   `tests/testthat/test_prep_cleanup.R`, full suite 101 passing / 0 failing.
   Version bumped to `0.3.0.9000`; `NEWS.md` updated. **Not yet committed.**
3. ~~**B2, C2**, plus required-column guards~~ — done on `mics`. B2 turned out to
   be a live bug in `prep_nrsim_sib_histories()`, not only a MICS hazard.
4. **Request MICS microdata access.** This is the long pole and nothing in the
   package unblocks it: the audit's Q4 (does the roster survive into the released
   microdata, and in what layout) cannot be answered without a file, and Q8
   records that no survey has been requested yet. Registration is per-survey. The
   four shortlisted MICS6 surveys are Benin 2021-22, Gambia 2018, Malawi 2019-20
   and Sierra Leone 2017.
5. **B5 scaffolding** — the synthetic MICS fixture, so B1–B4 are test-driven
   rather than validated by eye against a dataset that has not arrived. Build it
   as `tests/testthat/helper-simulate-mics.R`; a fabricated fixture should not
   ship as package data that looks like real MICS. Add `model_mics_dat` only once
   a real file is in hand.
6. **B3, B4, B1** — the MICS prep proper, once the audit has answered the layout
   question. B1 now has eight requirements, not four.
7. **D3** — needs a documentary answer (the DHS questionnaire history), not a
   code change.
8. ~~**E1–E3**~~ — done; see section E. Two follow-ups land in the *analysis*
   repo, not here: the same missing sex key at `code/R/estimate.R:134`, and the
   changed `get_ego_age_distn(only_females = FALSE)` shape at `:129`/`:135`.
9. **E4** — decide what `adj.factor` is supposed to be. Needs a judgement about
   the estimator, not a code change; see section E4 for the three questions.


Context the next session will want
----

- **Analysis repo:** `~/Dropbox/maternal-mortality/maternal-mortality`.
  `STATUS.md` is the working document. `code/R/prep_mics.R` states the interface
  this package needs and lists the same hazards as B1–B4.
  `code/mics-audit/mics-feasibility.qmd` is the audit.
- **The old working copy is gone.** The results currently in the paper were
  produced by `devtools::load_all("~/Dropbox/library/siblingsurvival")`, a
  directory that no longer exists. Clone fresh to a durable path and record it.
- **Expect re-running to change the numbers.** `NEWS.md` for 0.3.0 records a fix
  to `aggregate_maternal_estimates()`, which "was accidentally referencing
  package-level example objects (`ex.ego`, `ex.sib`) instead of the `ego.dat` and
  `sib.dat` arguments passed by the caller." That would corrupt exactly the
  aggregation weights and visibility adjustment behind the cached totals. The
  analysis repo has an open question about the individual/aggregate comparison
  having reversed sign since June 2025, and this is the leading candidate
  explanation — but it cannot be confirmed, because the code that produced those
  results is unrecoverable. Treat a change in the numbers after a re-run as
  expected and informative, not as a regression.
- **A useful reference implementation exists.** The analysis repo's
  `code/archive/20250219-prev/helper_long.R` holds pre-migration versions of
  `get_visibility()`, `get_ego_age_distn()`, and the `calculate_total*` trio that
  became `aggregate_maternal_estimates()`. If you need to know what one of these
  is *supposed* to do, that is the most readable statement of it anywhere.
