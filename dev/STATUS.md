Status of `siblingsurvival`
====

Where the package stands, what has been validated, and how it fits with the
repos around it. Last updated **2026-09-16**.

This and `FUTURE-IMPROVEMENTS.md` are the only live documents in `dev/`.
Everything else is in `attic/`, which has a README saying what each closed plan
was and what is still worth reading it for.

`dev/` is in `.Rbuildignore`, so none of it ships.


Where things stand
----

**Version `0.3.0.9000`, on `main`, tree clean.** Test suite: **332 passing, 0
failing, 0 skipped** (2026-09-16). The repo has **no git tags at all** — the last
released version was `0.3.0` and there is no tag for it.

Everything that was planned in `dev/` has been carried out. Since 0.3.0 the
package has:

* been **validated against The DHS Program's own tabulation code**
  (`DHS-Indicators-Stata`, `Chap16_AM/AM_rates.do`), on all 43 surveys in the
  paper's sample, and against five published all-cause mortality tables;
* been **validated against UNICEF's MICS tabulation syntax** and published MICS
  tables on four surveys across two roster schemes;
* **gained MICS support** — `prep_mics_sib_histories()`, varmaps for MICS4--7,
  and a MICS branch of `add_maternal_deaths()`;
* **handed its tie-agnostic estimator spine to `networkreporting`**, keeping
  only the code that knows about DHS, MICS or maternal mortality, and gained a
  declarable `visibility` rule in `sibling_estimator()`.

`NEWS.md` is the authoritative account of all of that, in the package's own
words, and is long and specific. Read it before this file.


What is validated, and how much
----

Kept here because it is the answer to "can I trust a number this produces", and
because the evidence is spread across `data-raw/`, `NEWS.md` and the attic.

| | Reference | Result |
|---|---|---|
| DHS, all 43 surveys | literal replica of `AM_rates.do` | female exposure, female and male all-cause deaths, and pregnancy-related deaths all match at ratio 1.0000000 in **43 of 43**. Male *exposure* matches in 42; Gabon 2012 is high by 6.1 person-years in 105,599, from one sibling |
| DHS, 5 surveys | published all-cause tables, phases 4--8 | exposure matches **to the person-year in all 70 cells**, both sexes; deaths to rounding; age-adjusted rates to 0.1% |
| DHS, Rwanda 2010 | published `FR259` Table 16.4 | reproduces cell by cell |
| MICS, Iraq 2018 | published TM.9.1/9.3 | exposure 1.0000, all-cause deaths 1.0007, pregnancy-related 1.008 |
| MICS, Zimbabwe 2019 | published TM.9.1/9.2 | reproduces; **its TM.9.3 female figures do not, and the report is the problem, not the package** |

Three things that will legitimately never reproduce, so they are not chased:
**confidence intervals** (this package bootstraps; DHS fits Poisson models, MICS
jackknifes), **`asdr.ind`** (the individual-visibility estimator is the research
contribution and appears in no published table), and **MMR** (needs a birth
history this package does not read).

Two published tables are known to be unreliable and should not be used as
targets: **Rwanda 2010's summary rows** (its age-specific cells are exact) and
**Zimbabwe 2019's female mortality figures** (its male ones reproduce exactly).

The harnesses are `data-raw/dhs-validation/` and `data-raw/mics-validation/` —
scripts and expected values tracked, microdata gitignored. They are also the
regression gate for anything structural: the `networkreporting` move was
accepted only because both reproduced every published figure unchanged.


Where things live
----

    R/                    19 files; no src/ any more, it moved with occ.exp()
    data/                 7 DHS varmaps, 4 MICS varmaps, model_dhs_dat, ex.ego/ex.sib
    tests/testthat/       11 files, incl. helper-simulate.R and helper-simulate-mics.R
    vignettes/            sibling-estimates, maternal-estimates, preparing-data
    vignettes-drafts/     dhs-data, mics-data, recoding-decisions -- written, not shipped
    data-raw/             varmap generation + the two validation harnesses
    notes/                mics-maternal-mortality-reference.md, and a DHS syntax zip
    dev/                  this file, FUTURE-IMPROVEMENTS.md, attic/

**`vignettes-drafts/` is where the validation write-ups actually landed.**
`dhs-data.Rmd` and `mics-data.Rmd` carry the substance of the two closed
validation plans in user-facing form — which surveys can be used, the file
quirks, what reproduces and what does not, and the maternal/pregnancy-related
distinction. `recoding-decisions.Rmd` (added 2026-09-16) is the reference
statement of every recoding convention needed to match published DHS and MICS
figures, written instrument-first so it is usable without this package. All
three are build-ignored and their chunks are `eval = FALSE`, because they need
registration-gated microdata. Moving them into `vignettes/` is an open item; see
`FUTURE-IMPROVEMENTS.md`.

`notes/mics-maternal-mortality-reference.md` is the sourced methods reference the
MICS work was built on — verbatim questionnaires, real `mm.sav` dictionaries,
published tables checked arithmetically. Note its §8 conclusion about which
estimand the published MICS "Maternal Deaths" column reports **was subsequently
shown to be backwards** by Iraq 2018; the column is a 42-day pregnancy-related
count.


The repos this one sits between
----

Four repos, and a change in one usually implies something in another.

**`~/dev/networkreporting`** — holds the estimator spine as of 0.3.2:
`occ.exp()` and its `src/`, `cell_config()`, the age and time-period helpers,
`get_esc_reports()`, `get_ec_reports()`, the estimator helpers, the visibility
internals, `get_ic_reports()` and `life_table.R`. This package re-exports every
public name from `R/reexports.R`, so no caller broke. Its `dev/VISIBILITY-PLAN.md`
and `dev/FUTURE-IMPROVEMENTS.md` are the live documents there.

> **Mind the install order.** Since `DESCRIPTION` declares
> `Imports: networkreporting (>= 0.3.2)` with `Remotes: dfeehan/networkreporting`,
> this package **cannot load** until a `networkreporting` build containing the
> spine is installed, and no such build is on CRAN. Always
> `devtools::install()` `networkreporting` first, then `load_all()` here. A
> first test run that fails with a missing-package error is this, not a mistake
> in the move — do not "fix" it by reverting.

**`~/Dropbox/maternal-mortality/maternal-mortality`** — the DHS maternal
mortality sensitivity analysis (Erhardt-Ohren and Feehan), the main consumer.
`STATUS.md` is its working document. Its handoff to this package is CLOSED;
every item was done. Its pipeline runs 44 DHS and 13 MICS surveys, and it has
decided to report **pregnancy-related** mortality throughout.

**`~/Dropbox/matlab-mortality`** — the multi-tie study (siblings, households,
parents, cousins, aunts/uncles, neighbours, acquaintances). It is what drove the
spine move: the `1/y.F` vs `1/(y.F + 1)` rule is a theorem about *cliques*, and
cousins and neighbours are not cliques. It consumes `networkreporting` directly,
not this package.


Open threads
----

Short, and none of them blocks anything. They are in
`FUTURE-IMPROVEMENTS.md` with the reasoning; in brief:

* **Male age standardisation from an `MR`/`PR` file** — a nice-to-have, not a
  blocker. The original premise (that you *need* a male age distribution to
  reproduce published male rates) turned out to be wrong.
* **Four statistical decisions that are open rather than defaulted** — the MICS
  `preg.window` default, a delivery death reported as an accident, the now-inert
  `na.action` under `style = "dhs"`, and whether Burkina Faso 2003 belongs in an
  analysis sample.
* **Housekeeping** — ship the three draft vignettes, a `model_mics_dat` if a
  redistributable extract ever exists, and cut a first tag.

**Stale local branches.** `dhstest`, `mics`, `visibility-phase0`,
`visibility-tie-config` and `visibility-tie-config-finish` are all merged into
`main` and can be deleted. `origin` still carries `dhstest` and `mics`.
