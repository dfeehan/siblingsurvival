Plan: validate the DHS code against published DHS estimates
====

Written 2026-08-21, after finishing the MICS validation on branch `mics`.

The MICS work turned out to hinge on one thing: finding UNICEF's own tabulation
syntax and transcribing it literally, so that every disagreement with a published
table could be attributed to a *named convention* rather than guessed at. That
found two real defects, corrected a wrong belief about what published tables
report, and diagnosed Zimbabwe 2019 as a bad table rather than a bad estimator.

This plan does the same for DHS. **DHS publishes an equivalent reference
implementation, and it is better documented than the MICS one.**

Nothing below has been run. The findings in section 2 come from reading code, and
each is written as a hypothesis with the evidence for it, not as a conclusion.


1. The reference implementation
----

`DHSProgram/DHS-Indicators-Stata`, directory **`Chap16_AM`**:

| File | What it does |
|---|---|
| `!AMmain.do` | driver |
| `AM_rates.do` | 1,376 lines: adult mortality, maternal and pregnancy-related mortality, 35q15, PMDF, MMR, lifetime risk |
| `AM_gfr.do` | 351 lines: the age-standardised GFR that the MMR is divided by |

Written by **Thomas Pullum** (DHS's own demographer) and modified by Trevor Croft
and Shireen Assaf. Its header says:

> The program agrees exactly with DHS procedures, except for confidence intervals.
> It is a complete re-write, not a translation, of the CSPro program.

That is as authoritative as the MICS syntax was, and unlike the MICS file it
carries extensive prose explaining *why* each step is as it is.

Second source: the **Guide to DHS Statistics**, chapter "Adult and Maternal
Mortality" (`dhsprogram.com/data/Guide-to-DHS-Statistics/`). `AM_rates.do` cites
it directly on at least one substantive point, so the two should be read together.

Both are public and need no registration.


2. What reading the code already suggests
----

Seven hypotheses, in rough order of how much they could move published numbers.
Each needs confirming against real data before anything is changed.

### H1 ⚠ The package's DHS pregnancy-related definition may be too narrow

`AM_rates.do:725`

    replace prdeaths_in_`li'=1 if deaths_in_`li'==1 & mm9>=2 & mm9<=6

and, in the header, **`mm12 is not needed`**.

The package, `is_preg_related_dhs()`:

    within.window <- (sib.time.delivery.death >= 100 & sib.time.delivery.death <= 141) |
                      sib.time.delivery.death %in% c(997, 998)
    died.pregnant <- sib.died.pregnant %in% c(2, 3, 4, 5)

Two differences:

* **`mm9 = 6` is excluded by the package.** DHS includes it. The `mm9` codes are
  `2` died while pregnant, `3` died during delivery, `4` since delivery (never
  actually assigned), `5` six weeks after delivery, **`6` two months after
  delivery**. `AM_rates.do:717` comments: *"Note that PR includes mm9=6. Not
  good but gives a match with MM estimates before the question on violence and
  accidents was added."*
* **The package imposes an `mm12` window that DHS says it does not need.**
  `mm12` `100`--`141` is 0--41 days, so the package applies a 42-day cut on top
  of a code that already encodes the window. A death two months after delivery
  has `mm12 = 202` and fails it.

Both push the same way, so the package's DHS pregnancy-related count is probably
**lower** than DHS's. Note also that `add_maternal_deaths()`'s own documentation
describes pregnancy-related as "within two months", which is *not* what the code
does --- so code and docs already disagree, independent of DHS.

This is the single highest-stakes item: it is the estimand underpinning the
paper, and it is the DHS analogue of the MICS `preg.window` finding.

### H2 ⚠ The maternal cause exclusion is applied to different codes

`AM_rates.do:728`

    replace mdeaths_in_`li'=1 if deaths_in_`li'==1 & mm9>=2 & mm9<=5 & mm16!=1 & mm16!=2

So the reference excludes violence/accident across all of `mm9` 2--5, uniformly.
The package's `is_maternal_dhs()` applies `not.accident` to codes 2 and 5 only,
leaves code 3 unconditional, and includes code 4. Its comment says this matches
"the behaviour this package has always had", so it is a known choice --- but it
should be re-examined against the reference rather than preserved by inertia.

**One ambiguity to resolve in D4, not by reading.** `AM_rates.do:307-320`
contains, *inside a comment block and therefore not executed*:

    If mm9=2, and mm16=1 or 2, recode mm9 to 1
    replace mm9=1 if mm9==2 & (mm16==1 | mm16==2)

introduced as "Important for redefinition of Pregnancy Related Mortality Ratio
(PRMR) in surveys from 2016 onwards", citing
`blog.dhsprogram.com/mmr-prmr/`. So DHS documents a rule that would remove
accident deaths from the *pregnancy-related* count in recent surveys, but the
reference code as shipped does not apply it: `prdeaths` is `mm9` 2--6 with no
cause condition at all.

Only 5 of the 43 surveys in the sample carry `mm16`, so this affects a small
subset --- but it is precisely the shape of the MICS finding (a documented rule
the shipped code does not implement), and it can only be settled by running both
variants against a published table for one of those 5.

Note this makes H2 far narrower than H1: **H1 governs all 43 surveys, H2 only 5.**

### H3 The life table constant differs, and this time it is documented

`AM_rates.do:1084-1085`

    //See DHS Guide to Statistics for use of 2.4 rather than 2.5 in the following formula
    gen q5=5*mx/(1+2.4*mx)

The package uses `nax = 2.5`. DHS uses the equivalent of `nax = 2.6`, and so does
the MICS syntax --- but where MICS contradicted its own header, DHS says the
Guide documents it deliberately. Effect is small (≈0.2 per 1,000 on 35q15) but
it is a free exact match, and it means both major sources agree against us.

**Decide:** match the sources, or keep 2.5 and document the divergence. Whichever,
it should become an argument rather than a literal.

### H4 ⚠ Male rates are standardised by a male age distribution we do not have

`get_age_distributions` in `AM_rates.do:450` uses the **MR file** (or the PR
file) for men's age distribution, because 35m15 for brothers must be standardised
by the age distribution of *men*, not of the interviewed women.

The package's `ego.dat` for DHS holds women only, so `get_ego_age_distn()` cannot
supply this. Male 35q15 and male age-adjusted rates therefore *cannot* reproduce
DHS without reading an MR or PR file.

This connects directly to the still-open `only_females = FALSE` questions in
`PACKAGE-HANDOFF.md` (E4) and to `ANALYSIS-REPO-CHANGES.md` A1. Resolving H4 may
resolve those too, or may show that `only_females = FALSE` is answering a
question DHS does not ask.

### H5 The observation window --- resolved by reading, still confirm numerically

`AM_rates.do:230-231` with the documented defaults `lw = -6, uw = 0`:

    gen start_month=doi+12*lw-12      ->  doi - 84
    gen end_month=doi+12*uw-1         ->  doi - 1

so the window is `[doi - 84, doi - 1]`, **identical to the MICS convention**, with
*"As is standard DHS practice, exposure or events in the month of interview are
ignored."* Expect `cell_config(time.periods = '7yr_beforeinterview')` to agree.

But note one genuine **DHS/MICS divergence** in `get_exposure_and_deaths`:

    replace last_`li'=mm8 if first_`li'<=mm8 & last_`li'>=mm8 & mm8<.

DHS ends a decedent's exposure *at* the month of death, inclusive. The MICS
syntax ends it at `MM18C - 1`, the month before. The package should not be made
to satisfy both at once; find out which it currently does and document the
choice.

### H6 Unknown survival status

`AM_rates.do:328`: `drop if mm2>1` --- drops `mm2 = 8`. The package drops them
too, so unlike MICS this should agree exactly. Confirm and move on.

### H7 Sweep for the silent-NA class

Both MICS defects were of one kind: a single unusable row, or a missing column,
NA-ing out or zero-lengthing an entire estimate with no error. Worth a deliberate
pass over the DHS path for the same shape, rather than waiting to trip over it.
Specific things to try: an IR file with a respondent missing `v005`; a phase
without `mm16`; a sibling with `mm4` present but `mm8` missing; `mm9 = 98`.


3. Staged validation
----

Mirrors V1--V7 from `MICS-PLAN.md`, which worked well.

**D1. Data and target selection. --- DONE, 2026-08-21.**

The paper's sample is already fixed: **43 DHS surveys**, listed in
`out/survey-index.rds` in the analysis repo and held as `.DTA` under
`data/dhs/`. Since the paper covers every DHS country with a maternal module,
coverage of *phases* rather than countries is what matters, and running cleanly
on all 43 is a first-class deliverable rather than a closing formality.

Phase spread of the 43: 1 in phase 2, 9 in 3, 11 in 4, 7 in 5, 9 in 6, 5 in 7,
1 in 8.

Three things found while inventorying, all worth knowing before starting:

* **Only 5 of the 43 carry `mm16`** --- `GAIR71FL`, `ZAIR71FL`, `LBIR7AFL`,
  `MLIR7AFL`, `GMIR81FL`. Maternal mortality proper is computable for those
  five; the other 38 support pregnancy-related only. Note `RWIR70FL` is phase 7
  but predates the 2016 introduction of `mm16` and does *not* have it, so phase
  alone is not a safe proxy. **This is why H1 dominates H2.**
* **`mm15` is absent from `MWIR22FL`** (Malawi 1992, the one phase-2 survey).
* **`GAIR41FL` (Gabon 2000) cannot be read at all** with `haven::read_dta()`
  defaults --- it fails with "Unable to convert string to the requested
  encoding (invalid byte sequence)". `encoding = "latin1"` reads it fine
  (3,361 variables). The analysis pipeline will hit this too; see
  `ANALYSIS-REPO-CHANGES.md`.

Validation set --- **seven surveys, one per phase**, using Rwanda as a
within-country series across phases 5/6/7 so that differences there are
attributable to the questionnaire rather than to the population, and Malawi and
Benin for the overlap with the MICS 13:

| Survey | Country | Phase | Why |
|---|---|---|---|
| `MWIR22FL` | Malawi 1992 | 2 | oldest; no `mm15`; MICS-overlap country |
| `BJIR31FL` | Benin 1996 | 3 | MICS-overlap country |
| `MWIR41FL` | Malawi 2000 | 4 | largest phase; MICS-overlap country |
| `RWIR53FL` | Rwanda 2005 | 5 | start of the Rwanda series |
| `RWIR61FL` | Rwanda 2010 | 6 | |
| `RWIR70FL` | Rwanda 2014-15 | 7 | phase 7 *without* `mm16` |
| `GMIR81FL` | Gambia 2019-20 | 8 | newest; **has `mm16`**, so H2 is testable |

`GMIR81FL` is the only one of the seven that can test H2 and the commented-out
PRMR recode. If that turns out to be the crux, add a second `mm16` survey from
the remaining four.

Published targets still have to come from final-report PDFs on dhsprogram.com.

**D2. Build the replica.** Transcribe `AM_rates.do` literally into
`data-raw/dhs-validation/stata-reference-replica.R`, the same way
`spss-syntax-replica.R` was done: not the package's approach, a deliberate
line-by-line translation, with a header listing every convention it encodes.

Two shortcuts are legitimate. DHS fits Poisson regressions to get the rates, but
with a saturated age term and `log(exposure)` offset the point estimates are
exactly `deaths/exposure`, so a plain ratio suffices --- **verify this once on
one survey rather than assuming it**. And `AM_gfr.do` is only needed for MMR and
lifetime risk; do it second.

**D3. Extract published targets** into
`data-raw/dhs-validation/published-targets.csv`, same schema as the MICS file so
tooling is shared. Take exposure and death counts by age and sex, the age-adjusted
rates, 35q15, PMDF, MMR and GFR.

**D4. Replica vs published.** This is the load-bearing step. If the replica does
not reproduce published tables to roughly the person-year, stop and fix the
replica --- everything downstream depends on it being right. In MICS this step is
what converted "we cannot explain Zimbabwe" into "Zimbabwe's table is wrong".

**D5. Package vs replica, decomposed.** For each survey, compare female and male
exposure, female and male all-cause deaths, pregnancy-related deaths, maternal
deaths, and each rate. Attribute every gap to a specific named convention, and
quantify it. Toggle one hypothesis at a time --- the MICS decomposition table
(each candidate definition against the published count) was the single most
useful artefact produced, because it made the answer unarguable.

**D6. Fix, with regression tests.** Each confirmed defect gets a test that fails
against the current code. For anything that changes existing DHS results ---
which H1 and H2 both would --- follow the `preg.window` pattern: add an argument,
default to current behaviour so nothing moves silently, document loudly which
setting reproduces published DHS figures, and let the estimand be chosen
deliberately.

**D7. Breadth and write-up.** Run every DHS phase 2--8 varmap through the prep on
real files and confirm each completes with no NA estimates. Then a `dhs-data`
vignette section mirroring "What the official tabulation syntax says", and an
update to `ANALYSIS-REPO-CHANGES.md` for anything the analysis repo must change.


4. What will legitimately not reproduce
----

State these up front so they are not chased:

* **Confidence intervals.** `AM_rates.do` says so itself. DHS uses log-scale
  intervals from a Poisson fit; this package bootstraps.
* **`asdr.ind`.** The individual-visibility estimator is the research
  contribution and appears in no published table, by construction.
* **Subpopulation rates.** DHS explicitly declines to compute maternal mortality
  within subpopulations; there is nothing to validate against.
* **MMR and lifetime risk**, until `AM_gfr.do` is also replicated, since they need
  the birth history.


5. Order of work
----

1. Merge `mics` into `main` first --- this plan assumes the MICS fixes are in.
2. D1: pick the surveys. Needs a judgement call on which ones; worth deciding
   together rather than my choosing.
3. D2 + D4: replica, and reproduce published tables with it. Highest information
   per unit effort, and it either validates or invalidates everything after.
4. D5: decompose. Expect H1 to dominate.
5. D6: fix, test, and decide the estimand for the paper.
6. D3 for the remaining surveys, D7 breadth, write-up.

Steps 3 and 4 are where the real answer is. If they come back clean, the rest is
bookkeeping; if H1 is confirmed, the paper's DHS numbers move and that needs to be
understood before anything else is built on them.
