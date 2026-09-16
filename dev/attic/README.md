Attic
====

Closed planning documents. **Nothing in here is live.** Each file was a working
plan that has been carried out; they are kept because they record *why* the
package does what it does, and because the evidence behind several conventions
exists nowhere else in this repo.

Moved here 2026-09-16, when `dev/` was consolidated into `STATUS.md` (where
things stand) and `FUTURE-IMPROVEMENTS.md` (what is still worth doing). Anything
in these files that was still open at that point was carried into one of those
two; everything else was done.

If you are looking for current work, do not start here.


What each one is
----

### `PACKAGE-HANDOFF.md` — closed

Written 2026-08-20 in the maternal mortality analysis repo, as a list of changes
that repo needed from this package. Sections A--E are all done, item by item;
section F (moving the estimator spine to `networkreporting`) is done too, and
`NEWS.md` for 0.3.0.9000 describes the result.

The one item that was never coded is **D3**, the `sibhist_varmap_dhs2` `mm15`
question, and it was closed by decision rather than by change: the DHS validation
exercised all seven varmaps against published tables directly, which is a
stronger check than the questionnaire-history note that raised the doubt. See
`FUTURE-IMPROVEMENTS.md` for the residue.

Still worth reading for: the **"Context the next session will want"** section at
the end, which records that the results in the paper were produced by a working
copy that no longer exists, and why a change in the numbers after a re-run is
expected rather than a regression.

### `ANALYSIS-REPO-CHANGES.md` — delivered

Written 2026-08-21, pointing the other way: what the analysis repo had to change
to keep working after the package's DHS and MICS fixes. It has been consumed.
The analysis repo's own `PACKAGE-HANDOFF.md` is marked CLOSED, its `STATUS.md`
records the estimand decision (pregnancy-related throughout), and its pipeline
now runs 44 DHS and 13 MICS surveys.

Still worth reading for: the per-survey table of how much the `mm9 = 6` fix moved
each DHS pregnancy-related count (0% to 48%, and *not* uniform across surveys).

### `DHS-VALIDATION-PLAN.md` — done

Written 2026-08-21. Seven hypotheses (H1--H7) about where this package diverged
from `DHSProgram/DHS-Indicators-Stata`, `Chap16_AM`. All resolved: H1, H2, H3,
H5, H7 and H9 became code changes, H4's premise turned out to be wrong, H6
already agreed. D7 runs all 43 surveys against a literal replica and D8 checks
all-cause mortality against five published tables.

The findings live on in `NEWS.md`, in `data-raw/dhs-validation/`, and in
`vignettes-drafts/dhs-data.Rmd`. Kept for the working: the Rwanda 2010 cell-by-cell
reproduction, the evidence that Rwanda 2010's published *summary rows* are
unreliable while its age-specific cells are exact, and the `mm9` 5/6
questionnaire artefact (H1b) that makes maternal counts non-comparable across
surveys.

### `MICS-PLAN.md` — done

Written 2026-08-21. The staged design (M1--M8) and validation (V1--V7) for MICS
support. Everything shipped: `prep_mics_sib_histories()`, the four MICS varmaps,
the MICS branch of `add_maternal_deaths()`, `helper-simulate-mics.R` and
`test_mics.R`.

Kept for the working: the 229-survey inventory that found only **13** usable
sibling rosters, the Iraq 2018 result establishing that the published
"Maternal Deaths" column is really a 42-day *pregnancy-related* count, and the
evidence that Zimbabwe 2019's published female figures are unreliable while its
male ones reproduce exactly.
