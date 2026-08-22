# Plan: validate the DHS code against published DHS estimates

Written 2026-08-21, after finishing the MICS validation on branch
`mics`.

The MICS work turned out to hinge on one thing: finding UNICEF’s own
tabulation syntax and transcribing it literally, so that every
disagreement with a published table could be attributed to a *named
convention* rather than guessed at. That found two real defects,
corrected a wrong belief about what published tables report, and
diagnosed Zimbabwe 2019 as a bad table rather than a bad estimator.

This plan does the same for DHS. **DHS publishes an equivalent reference
implementation, and it is better documented than the MICS one.**

Nothing below has been run. The findings in section 2 come from reading
code, and each is written as a hypothesis with the evidence for it, not
as a conclusion.

## 1. The reference implementation

`DHSProgram/DHS-Indicators-Stata`, directory **`Chap16_AM`**:

| File | What it does |
|----|----|
| `!AMmain.do` | driver |
| `AM_rates.do` | 1,376 lines: adult mortality, maternal and pregnancy-related mortality, 35q15, PMDF, MMR, lifetime risk |
| `AM_gfr.do` | 351 lines: the age-standardised GFR that the MMR is divided by |

Written by **Thomas Pullum** (DHS’s own demographer) and modified by
Trevor Croft and Shireen Assaf. Its header says:

> The program agrees exactly with DHS procedures, except for confidence
> intervals. It is a complete re-write, not a translation, of the CSPro
> program.

That is as authoritative as the MICS syntax was, and unlike the MICS
file it carries extensive prose explaining *why* each step is as it is.

Second source: the **Guide to DHS Statistics**, chapter “Adult and
Maternal Mortality” (`dhsprogram.com/data/Guide-to-DHS-Statistics/`).
`AM_rates.do` cites it directly on at least one substantive point, so
the two should be read together.

Both are public and need no registration.

## 2. What reading the code already suggests

Seven hypotheses, in rough order of how much they could move published
numbers. Each needs confirming against real data before anything is
changed.

### H1 ⚠ The package’s DHS pregnancy-related definition may be too narrow

`AM_rates.do:725`

``` R
replace prdeaths_in_`li'=1 if deaths_in_`li'==1 & mm9>=2 & mm9<=6
```

and, in the header, **`mm12 is not needed`**.

The package,
[`is_preg_related_dhs()`](http://dennisfeehan.org/siblingsurvival/reference/is_preg_related_dhs.md):

``` R
within.window <- (sib.time.delivery.death >= 100 & sib.time.delivery.death <= 141) |
                  sib.time.delivery.death %in% c(997, 998)
died.pregnant <- sib.died.pregnant %in% c(2, 3, 4, 5)
```

Two differences:

- **`mm9 = 6` is excluded by the package.** DHS includes it. The `mm9`
  codes are `2` died while pregnant, `3` died during delivery, `4` since
  delivery (never actually assigned), `5` six weeks after delivery,
  **`6` two months after delivery**. `AM_rates.do:717` comments: *“Note
  that PR includes mm9=6. Not good but gives a match with MM estimates
  before the question on violence and accidents was added.”*
- **The package imposes an `mm12` window that DHS says it does not
  need.** `mm12` `100`–`141` is 0–41 days, so the package applies a
  42-day cut on top of a code that already encodes the window. A death
  two months after delivery has `mm12 = 202` and fails it.

Both push the same way, so the package’s DHS pregnancy-related count is
probably **lower** than DHS’s. Note also that
[`add_maternal_deaths()`](http://dennisfeehan.org/siblingsurvival/reference/add_maternal_deaths.md)’s
own documentation describes pregnancy-related as “within two months”,
which is *not* what the code does — so code and docs already disagree,
independent of DHS.

This is the single highest-stakes item: it is the estimand underpinning
the paper, and it is the DHS analogue of the MICS `preg.window` finding.

### H2 ⚠ The maternal cause exclusion is applied to different codes

`AM_rates.do:728`

``` R
replace mdeaths_in_`li'=1 if deaths_in_`li'==1 & mm9>=2 & mm9<=5 & mm16!=1 & mm16!=2
```

plus an **upfront recode** at `:314`, which runs *before* both flags:

``` R
replace mm9=1 if mm9==2 & (mm16==1 | mm16==2)
```

So DHS excludes violence/accident from the maternal count across all of
`mm9` 2–5, *and* separately removes accident deaths among `mm9 = 2` from
the pregnancy-related count as well.

The package’s
[`is_maternal_dhs()`](http://dennisfeehan.org/siblingsurvival/reference/is_maternal_dhs.md)
applies `not.accident` to codes 2 and 5 only, leaves code 3
unconditional, includes code 4, and does no upfront recode. The in-code
comment says this matches “the behaviour this package has always had”,
so it is a known choice — but it should be re-examined against the
reference rather than preserved by inertia.

### H3 The life table constant differs, and this time it is documented

`AM_rates.do:1084-1085`

``` R
//See DHS Guide to Statistics for use of 2.4 rather than 2.5 in the following formula
gen q5=5*mx/(1+2.4*mx)
```

The package uses `nax = 2.5`. DHS uses the equivalent of `nax = 2.6`,
and so does the MICS syntax — but where MICS contradicted its own
header, DHS says the Guide documents it deliberately. Effect is small
(≈0.2 per 1,000 on 35q15) but it is a free exact match, and it means
both major sources agree against us.

**Decide:** match the sources, or keep 2.5 and document the divergence.
Whichever, it should become an argument rather than a literal.

### H4 ⚠ Male rates are standardised by a male age distribution we do not have

`get_age_distributions` in `AM_rates.do:450` uses the **MR file** (or
the PR file) for men’s age distribution, because 35m15 for brothers must
be standardised by the age distribution of *men*, not of the interviewed
women.

The package’s `ego.dat` for DHS holds women only, so
[`get_ego_age_distn()`](http://dennisfeehan.org/siblingsurvival/reference/get_ego_age_distn.md)
cannot supply this. Male 35q15 and male age-adjusted rates therefore
*cannot* reproduce DHS without reading an MR or PR file.

This connects directly to the still-open `only_females = FALSE`
questions in `PACKAGE-HANDOFF.md` (E4) and to `ANALYSIS-REPO-CHANGES.md`
A1. Resolving H4 may resolve those too, or may show that
`only_females = FALSE` is answering a question DHS does not ask.

### H5 The observation window

`AM_rates.do:230-231`

``` R
gen start_month=doi+12*lw-12
gen end_month=doi+12*uw-1
```

with *“As is standard DHS practice, exposure or events in the month of
interview are ignored.”* Same rule the MICS syntax uses. Expect
`cell_config(time.periods = '7yr_beforeinterview')` to agree; confirm
rather than assume, and check the *lower* bound too, which is where the
MICS convention was non-obvious.

### H6 Unknown survival status

`AM_rates.do:328`: `drop if mm2>1` — drops `mm2 = 8`. The package drops
them too, so unlike MICS this should agree exactly. Confirm and move on.

### H7 Sweep for the silent-NA class

Both MICS defects were of one kind: a single unusable row, or a missing
column, NA-ing out or zero-lengthing an entire estimate with no error.
Worth a deliberate pass over the DHS path for the same shape, rather
than waiting to trip over it. Specific things to try: an IR file with a
respondent missing `v005`; a phase without `mm16`; a sibling with `mm4`
present but `mm8` missing; `mm9 = 98`.

## 3. Staged validation

Mirrors V1–V7 from `MICS-PLAN.md`, which worked well.

**D1. Data and target selection.** ~5,134 DHS files are already local
under `~/Dropbox/dhs/data/20250202/`, so no download is needed for the
microdata. The work is choosing surveys and getting the *published*
numbers, which means final report PDFs from dhsprogram.com.

Selection criteria — aim for about five surveys:

- at least two **phase 7+** surveys (`mm16` present, so maternal is
  computable) and two **pre-2016** (pregnancy-related only), since H1/H2
  bite differently
- at least one country that also appears in the MICS 13, so the DHS↔︎MICS
  comparison in the paper gets validated end to end
- one survey with a known-awkward feature, to be the DHS analogue of São
  Tomé

**D2. Build the replica.** Transcribe `AM_rates.do` literally into
`data-raw/dhs-validation/stata-reference-replica.R`, the same way
`spss-syntax-replica.R` was done: not the package’s approach, a
deliberate line-by-line translation, with a header listing every
convention it encodes.

Two shortcuts are legitimate. DHS fits Poisson regressions to get the
rates, but with a saturated age term and `log(exposure)` offset the
point estimates are exactly `deaths/exposure`, so a plain ratio suffices
— **verify this once on one survey rather than assuming it**. And
`AM_gfr.do` is only needed for MMR and lifetime risk; do it second.

**D3. Extract published targets** into
`data-raw/dhs-validation/published-targets.csv`, same schema as the MICS
file so tooling is shared. Take exposure and death counts by age and
sex, the age-adjusted rates, 35q15, PMDF, MMR and GFR.

**D4. Replica vs published.** This is the load-bearing step. If the
replica does not reproduce published tables to roughly the person-year,
stop and fix the replica — everything downstream depends on it being
right. In MICS this step is what converted “we cannot explain Zimbabwe”
into “Zimbabwe’s table is wrong”.

**D5. Package vs replica, decomposed.** For each survey, compare female
and male exposure, female and male all-cause deaths, pregnancy-related
deaths, maternal deaths, and each rate. Attribute every gap to a
specific named convention, and quantify it. Toggle one hypothesis at a
time — the MICS decomposition table (each candidate definition against
the published count) was the single most useful artefact produced,
because it made the answer unarguable.

**D6. Fix, with regression tests.** Each confirmed defect gets a test
that fails against the current code. For anything that changes existing
DHS results — which H1 and H2 both would — follow the `preg.window`
pattern: add an argument, default to current behaviour so nothing moves
silently, document loudly which setting reproduces published DHS
figures, and let the estimand be chosen deliberately.

**D7. Breadth and write-up.** Run every DHS phase 2–8 varmap through the
prep on real files and confirm each completes with no NA estimates. Then
a `dhs-data` vignette section mirroring “What the official tabulation
syntax says”, and an update to `ANALYSIS-REPO-CHANGES.md` for anything
the analysis repo must change.

## 4. What will legitimately not reproduce

State these up front so they are not chased:

- **Confidence intervals.** `AM_rates.do` says so itself. DHS uses
  log-scale intervals from a Poisson fit; this package bootstraps.
- **`asdr.ind`.** The individual-visibility estimator is the research
  contribution and appears in no published table, by construction.
- **Subpopulation rates.** DHS explicitly declines to compute maternal
  mortality within subpopulations; there is nothing to validate against.
- **MMR and lifetime risk**, until `AM_gfr.do` is also replicated, since
  they need the birth history.

## 5. Order of work

1.  Merge `mics` into `main` first — this plan assumes the MICS fixes
    are in.
2.  D1: pick the surveys. Needs a judgement call on which ones; worth
    deciding together rather than my choosing.
3.  D2 + D4: replica, and reproduce published tables with it. Highest
    information per unit effort, and it either validates or invalidates
    everything after.
4.  D5: decompose. Expect H1 to dominate.
5.  D6: fix, test, and decide the estimand for the paper.
6.  D3 for the remaining surveys, D7 breadth, write-up.

Steps 3 and 4 are where the real answer is. If they come back clean, the
rest is bookkeeping; if H1 is confirmed, the paper’s DHS numbers move
and that needs to be understood before anything else is built on them.
