MICS validation harness
====

Checks that `siblingsurvival` reproduces published MICS estimates. See the
"Validation against published MICS numbers" section of `../../dev/MICS-PLAN.md` for
the reasoning and the staged design; this file covers only how to run it.

**The target is the aggregate-visibility estimator.** `asdr.agg` is
`sum(w*deaths) / sum(w*exposure)` with no visibility weighting, which is exactly
the conventional MICS calculation — MICS applies no sibship-size correction. So
published numbers are a genuine ground truth for `asdr.agg`. `asdr.ind` is the
contribution and is *expected* to differ; that gap is the finding, not a bug.

Layout
----

    data-raw/
      mics-data/          .sav files. GITIGNORED. never commit these.
      mics-validation/    this directory. tracked.
        published-targets.csv
        README.md
        validate.R        (to be written)

`published-targets.csv` is tracked even though the microdata is not: the
expected values come from public survey reports, so they are shareable, and
anyone who obtains the data can reproduce the check.

Getting the data
----

MICS microdata is registration-gated and **cannot be redistributed**. Register
per survey at <https://mics.unicef.org/surveys> and place the extracted files as

    data-raw/mics-data/<survey_id>/wm.sav
    data-raw/mics-data/<survey_id>/mm.sav

using the `survey` ids in `published-targets.csv` (`ZW2019`, `IQ2018`,
`PKPunjab2017`). `data-raw/mics-data/` is in `.gitignore`, and `^data-raw$` is in
`.Rbuildignore`, so neither the data nor these scripts reach the built package.

Priority order: **ZW2019** first — it has the fullest published tables. Then
IQ2018 as a second country, then Pakistan Sindh 2018-19, which lacks
`MM17C`/`MM18C` and so exercises the date-derivation fallback that Zimbabwe
never reaches.

The targets file
----

Long format, one expected value per row:

| column | |
|---|---|
| `survey` | matches the directory under `mics-data/` |
| `table` | source table in the published report (`TM.9.3`, `DQ.7.1`, …) |
| `quantity` | `exposure`, `maternal_deaths`, `maternal_rate`, `pm`, `q35_15`, … |
| `sex`, `age_group` | `age_group` uses the package's own labels (`[15,20)`), plus `total` and `age_adjusted` |
| `value`, `units` | |
| `note` | caveats, including which rows are out of scope |

Rows marked **out of package scope** in `note` (`gfr`, `mmr`, `lifetime_risk`)
need the women's birth history, which this package does not read. They are
recorded for the analysis repo, which does. Validation here stops at the rate and
PM.

Traps
----

Transcribed from `notes/mics-maternal-mortality-reference.md` so they are not
mistaken for pipeline bugs:

- Zimbabwe 2019's sampling-error table gives MMR 413.637 while the headline is
  462. **Validate against TM.9.3, not SE.1.** Iraq's two tables agree.
- The printed rate column is **per 1,000 woman-years**, despite a MICS6 footnote
  elsewhere saying "per 100,000 women 15–49".
- Published counts and exposures are weighted and rounded, so compare with a
  tolerance rather than for exact equality.
- CIs will not match: MICS uses jackknife with ±2·se, this package uses a
  bootstrap with percentile intervals.
- Do not compare Zimbabwe 2014 (614) with 2019 (462) — different estimand,
  window and cause exclusion.

Two ambiguities the validation is meant to *resolve*, not assume:

1. **The 7-year boundary.** No MICS document states whether it is years-since-death
   0–6 or 1–7. Try both, and both CMC-based exposure allocation and integer-year
   binning. Only one combination will reproduce the exposure column (V2).
2. **The age standard.** MICS text says "the age distribution of the survey
   respondents", IUSSP says "women 15–49 in the households surveyed", MMEIG says
   "the female population of respondent households". Try both; only one will
   reproduce the age-adjusted rate (V5).
