Future improvements to `siblingsurvival`
====

A running list of things worth doing to the package that are *not* defects and
so did not get fixed as part of the DHS and MICS validation work. Each is either
a capability the package does not have, or a decision that should be made
deliberately rather than defaulted into.

Started 2026-08-22, at the end of the DHS validation round.

Ordered roughly by how much they would matter.


1. Male age standardisation --- read an `MR` or `PR` file
----

**Status: open, but much less important than it first looked.** H4 in
`DHS-VALIDATION-PLAN.md`.

**Read this first.** The original premise --- that a male age distribution is
*needed* to reproduce published male rates --- turned out to be wrong. Published
DHS reports standardise both sexes by the age distribution of the survey
respondents, which `get_ego_age_distn(only_females = TRUE)` already returns, and
that reproduces published male rates on four surveys spanning phases 4 to 8
(Malawi 2000, Rwanda 2005, Rwanda 2014-15, Gambia 2019-20). Standardising by an
`MR`-derived male distribution instead moves *away* from the published figure in
every case checked. So this is a nice-to-have for matching the current
`AM_rates.do`, not a blocker for anything.

What remains true: `Chap16_AM/AM_rates.do:450` does take the men's age
distribution from the men's recode (`MR`) file, or the household (`PR`) file
where there was no men's survey, and this package reads neither. So the package
cannot reproduce *that particular calculation*.

What is not true, and was the original motivation for this item: that you need it
to get a published male rate. You do not --- see the table in H4.

What is already done: `get_ego_age_distn()` warns when `only_females = FALSE` is
asked of a single-sex sample and points at `only_females = TRUE` as the thing
published reports actually use; `aggregate_maternal_estimates()` warns through
`warn_uninterviewed_sex()` and returns `NA` for the uninterviewed sex. Both are
documented, so the package fails loudly rather than quietly, and the loud failure
now names the right remedy.

Options, roughly in increasing order of commitment:

* **Accept an external age distribution.** Let the caller pass a male age
  distribution to `aggregate_maternal_estimates()` --- `age_prop` already exists
  as an argument, so this may be mostly documentation plus a worked example.
  Cheapest, and keeps file-reading out of the package.
* **A helper that builds one from an MR or PR file.** Something like
  `mr_age_distn(mr.df)`, returning the same shape `get_ego_age_distn()` does.
  Small, self-contained, no new dependency.
* **Read it inside the prep.** Most convenient for users, most coupling.

On the `only_females = FALSE` question in `PACKAGE-HANDOFF.md` E4 and
`ANALYSIS-REPO-CHANGES.md` A1: the D8 evidence points at the second horn of that
dilemma. Published DHS figures use one reference distribution for both sexes, so
`only_females = FALSE` is answering a question the reports do not ask. That does
not settle whether the two-sex path should exist --- it may be right for the
`nrsim` case, where both sexes really are interviewed --- but it does mean
nothing in the DHS pipeline depends on it.


2. ~~`adj.factor` and `adj.factor.allage` are global scalars~~
----

**Status: resolved 2026-08-25 — the adjustment factors were removed.** See
`PACKAGE-HANDOFF.md` E4 for the full record.

`get_visibility()` computed three adjustment factors, two of which were scalars
over the *entire* respondent sample rather than being computed within group.
All three are now gone, from both `get_visibility()` and
`aggregate_maternal_estimates()`, which settles the three sub-questions —
the empty (harmonic, age-specific) corner, whether `S.hat` should respect
`only_females`, and whether `adj.factor[1]` should assert constancy — by
removing the thing they were about. `ego_vis_agg` still carries `y.F.bar` and
`avg.sib.size`, so a caller who wants an adjustment can build one. No estimate
changes: the factors were reported next to `ind.est`/`agg.est`, never applied
to them.

**Successor, added 2026-08-25.** A caller who wants one should no longer build
it by hand. `networkreporting::vis_from_donor(statistic = "arithmetic")` is the
supported replacement: it estimates the same group size the old factor was
built from, so `adj.factor` is exactly `(S.hat - 1) / S.hat`. The
`sibling-estimates` vignette now shows the two agreeing on real data.

Note the default is `statistic = "harmonic"`, not `"arithmetic"`. The individual
estimator averages `1/v`, so the summary that makes the plug-in unbiased is
`(E[1/v])^-1`; the old factor used the arithmetic mean, which by Jensen is never
smaller. Reproducing an older analysis therefore means asking for `"arithmetic"`
explicitly. On the vignette's DHS extract the harmonic group size runs about
25% below the arithmetic one, so the choice is not cosmetic.


3. Confidence intervals do not match either reference
----

**Status: known and documented; no action unless comparability matters.**

This package bootstraps and reports percentile intervals. DHS fits Poisson models
and reports log-scale intervals (`AM_rates.do` says its agreement with DHS
procedures holds "except for confidence intervals"). MICS uses jackknife
replication with \eqn{\pm 2\,\mathrm{se}}. All three are defensible; none agree.

Worth doing only if a paper needs intervals that match a published table. Note
DHS's own sampling-error appendix for maternal mortality comes from a *separate*
CSPro application, not from `AM_rates.do`, and in Zimbabwe 2019 the two disagree
with each other by 11%.


4. The maternal mortality ratio needs a birth history
----

**Status: out of scope, but the replicas do compute it.**

MMR is the age-standardised maternal (or pregnancy-related) rate divided by a
general fertility rate from the women's birth history --- a module this package
does not read. `data-raw/dhs-validation/stata-reference-replica.R` and
`data-raw/mics-validation/spss-syntax-replica.R` both compute it, for validation
only. If the package ever wants to produce MMR directly, that machinery exists to
copy.


5. Decisions that are open, not defects
----

Small, but each is a real choice that someone should make explicitly rather than
inherit.

* **`preg.window` default for MICS.** Currently `"2months"`, which is what the
  package has always done. `"42days"` is the WHO definition, is what published
  MICS tables report, and is what the DHS side already effectively applies. A
  case could be made for switching the default; it would move existing MICS
  results.
* **A delivery death reported as an accident.** `is_maternal_dhs()` now follows
  the DHS reference and excludes it. South Africa 2016 is the only survey of 43
  where this bites, by 3 deaths. The other reading --- that a death during
  delivery is unconditionally maternal --- is defensible but would stop
  reproducing published figures.
* **`na.action` is now inert under `style = "dhs"`.** It governed a `mm12`
  condition that neither DHS column uses any more. Kept so existing call sites
  work. Consider deprecating it for that style so it cannot mislead.
* **Burkina Faso 2003 has an entirely missing `mm9`**, so its pregnancy-related
  count is a structural zero. The package now warns. Whether it stays in an
  analysis sample is an analysis decision, noted in
  `ANALYSIS-REPO-CHANGES.md`.


6. Test data
----

* **`model_mics_dat`.** There is a `model_dhs_dat` shipped with the package but
  no MICS equivalent, so the MICS examples and vignette cannot be run by a user
  without registration-gated microdata. Worth adding if a redistributable
  extract ever exists.
* **The synthetic fixtures encoded a convention the prep did not.** They set
  `end_obs = death` while `prep_*_sib_histories()` sets `death + 1`, which is
  why they could not catch the event-boundary bug (H9 in
  `DHS-VALIDATION-PLAN.md`). Now fixed, but worth a periodic check that fixtures
  are built the way the prep builds real data --- a fixture that is internally
  consistent but unlike real output tests the wrong thing.
