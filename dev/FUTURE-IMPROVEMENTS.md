Future improvements to `siblingsurvival`
====

A running list of things worth doing to the package that are *not* defects. Each
is either a capability the package does not have, or a decision that should be
made deliberately rather than defaulted into.

Started 2026-08-22, at the end of the DHS validation round. Revised 2026-09-16
when `dev/` was consolidated: the items that were open in the four planning
documents were folded in here, and those documents moved to `dev/attic/`.

See `STATUS.md` for where the package stands. Ordered roughly by how much these
would matter.


1. Decisions that are open, not defects
----

**Status: open. Four real choices, each currently inherited rather than made.**

Small individually, but each is a statistical choice that someone should make
explicitly. None of them is a bug and none of them is urgent; all four would move
published numbers if changed, which is exactly why they need deciding rather than
drifting.

* **`preg.window` default for MICS.** Currently `"2months"`, which is what the
  package has always done. `"42days"` is the WHO definition, is what published
  MICS tables actually report (established by Iraq 2018 — see
  `attic/MICS-PLAN.md`), and is what the DHS side already effectively applies. A
  case could be made for switching the default; it would move existing MICS
  results.

* **A delivery death reported as an accident.** `is_maternal_dhs()` now follows
  the DHS reference and excludes it. South Africa 2016 is the only survey of 43
  where this bites, by 3 deaths. The other reading — that a death during
  delivery is unconditionally maternal — is defensible but would stop
  reproducing published figures.

* **`na.action` is now inert under `style = "dhs"`.** It governed an `mm12`
  condition that neither DHS column uses any more. Kept so existing call sites
  work. Consider deprecating it for that style so it cannot mislead.

* **Burkina Faso 2003 has an entirely missing `mm9`**, so its pregnancy-related
  count is a structural zero. The package now warns. Whether it stays in an
  analysis sample is an analysis decision, and one the analysis repo has to make
  rather than this one.


2. Ship the three draft vignettes
----

**Status: open, and the cheapest real improvement on this list.**

`vignettes-drafts/` holds `dhs-data.Rmd`, `mics-data.Rmd` and
`recoding-decisions.Rmd`, all written. Between them they carry the user-facing
half of both validation rounds: which surveys can be used and how to tell, the
file quirks (Gabon 2000's encoding, the `mm.sav` name collision in MICS3,
country customisation), what reproduces against published tables and what does
not, the maternal versus pregnancy-related distinction, and the two published
tables that are unreliable.

`recoding-decisions.Rmd` (added 2026-09-16) is the one most worth shipping. It
states every recoding convention needed to reproduce a published DHS or MICS
figure, organised around the questionnaire variables rather than around this
package's functions, so it is usable by someone working in Stata, SPSS or
Python. It is the only place the conventions are set out in one piece; the other
two now overlap it and could be trimmed to point at it.

They sit in `vignettes-drafts/`, which is build-ignored, so none of that reaches
a user. They are `eval = FALSE` throughout because the data is
registration-gated, which is also why they were set aside rather than shipped.

The question to settle is whether a vignette whose chunks never run should be a
vignette at all, or an article on the pkgdown site. Either is better than the
current state, where the material exists but is invisible.


3. Male age standardisation --- read an `MR` or `PR` file
----

**Status: open, but much less important than it first looked.** H4 in
`attic/DHS-VALIDATION-PLAN.md`.

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

On the related `only_females = FALSE` question: published DHS figures use one
reference distribution for both sexes, so `only_females = FALSE` is answering a
question the reports do not ask. That does not settle whether the two-sex path
should exist --- it may be right for the `nrsim` case, where both sexes really
are interviewed --- but it does mean nothing in the DHS pipeline depends on it.


4. Test data
----

**Status: open.**

* **`model_mics_dat`.** There is a `model_dhs_dat` shipped with the package but
  no MICS equivalent, so the MICS examples and vignette cannot be run by a user
  without registration-gated microdata. Worth adding if a redistributable
  extract ever exists. Until then `tests/testthat/helper-simulate-mics.R` is the
  substitute, and deliberately test-only: a fabricated fixture in `data/` looking
  like real MICS invites someone to mistake it for one.

* **The synthetic fixtures encoded a convention the prep did not.** They set
  `end_obs = death` while `prep_*_sib_histories()` sets `death + 1`, which is
  why they could not catch the event-boundary bug (H9 in
  `attic/DHS-VALIDATION-PLAN.md`). Now fixed, but worth a periodic check that
  fixtures are built the way the prep builds real data --- a fixture that is
  internally consistent but unlike real output tests the wrong thing.


5. Confidence intervals do not match either reference
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


6. The maternal mortality ratio needs a birth history
----

**Status: out of scope, but the replicas do compute it.**

MMR is the age-standardised maternal (or pregnancy-related) rate divided by a
general fertility rate from the women's birth history --- a module this package
does not read. `data-raw/dhs-validation/stata-reference-replica.R` and
`data-raw/mics-validation/spss-syntax-replica.R` both compute it, for validation
only. If the package ever wants to produce MMR directly, that machinery exists to
copy.


7. Release housekeeping
----

**Status: open, low stakes, but it will bite a user eventually.**

* **No git tags whatsoever**, including for the released `0.3.0`. Cutting tags is
  not an existing practice here; starting one would make a build referenceable,
  which matters more now that the analysis repo installs from GitHub.
* **The `networkreporting` dependency is unreleased.** `DESCRIPTION` carries
  `Imports: networkreporting (>= 0.3.2)` and `Remotes: dfeehan/networkreporting`,
  and no CRAN build contains the spine. Anyone installing this package needs the
  GitHub `networkreporting` first. Worth a line in `README.md`, which does not
  currently say so.


Closed, kept for the pointer
----

**`adj.factor` and `adj.factor.allage` were global scalars.** Resolved
2026-08-25 by removing all three adjustment factors from `get_visibility()` and
`aggregate_maternal_estimates()` rather than choosing among them. No estimate
changed: the factors were reported next to `ind.est`/`agg.est`, never applied to
them. `ego_vis_agg` still carries `y.F.bar` and `avg.sib.size`, so a caller who
wants an adjustment can build one. The full record is section E4 of
`attic/PACKAGE-HANDOFF.md`.

The forward-looking part, and the reason this is kept rather than deleted: a
caller who wants that adjustment should no longer build it by hand.
`networkreporting::vis_from_donor(statistic = "arithmetic")` is the supported
replacement --- it estimates the same group size the old factor was built from,
so `adj.factor` is exactly `(S.hat - 1) / S.hat`, and the `sibling-estimates`
vignette shows the two agreeing on real data.

**Note the default is `statistic = "harmonic"`, not `"arithmetic"`.** The
individual estimator averages `1/v`, so the summary that makes the plug-in
unbiased is `(E[1/v])^-1`; the old factor used the arithmetic mean, which by
Jensen is never smaller. Reproducing an older analysis therefore means asking for
`"arithmetic"` explicitly. On the vignette's DHS extract the harmonic group size
runs about 25% below the arithmetic one, so the choice is not cosmetic.

**The DHS-II varmap and `mm15`.** `sibhist_varmap_dhs2` carries the DHS-III+
numbering, which the analysis repo's varmap README said should not be possible.
Closed by decision, not by change: the DHS validation ran all seven varmaps
against published tables directly, and all 43 surveys reproduce the reference
exactly, which is a stronger check than the questionnaire-history note that
raised the doubt. Reopen only if a real DHS-II file disagrees.
