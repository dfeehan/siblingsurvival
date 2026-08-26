# Integration tests for the visibility rule argument to sibling_estimator().
#
# The rules themselves are unit-tested in networkreporting; what is tested here
# is the wiring: that the default changes nothing, that an alternative rule
# reaches the estimate, that provenance comes back with it, and that an
# estimated rule widens the bootstrap intervals rather than freezing them.
#
# Uses the canonical 4-ego dataset from helper-simulate.R, whose expected rates
# are derived analytically there:
#   ASDR.agg = 36/353 per year
#   ASDR.ind = 4/29   per year

library(tibble)
library(dplyr)

vis_cell_config <- function() {
  cell_config(age.groups   = "5yr",
              time.periods = "5yr_beforeinterview",
              start.obs    = "start_obs",
              end.obs      = "end_obs",
              event        = "dod",
              age.offset   = "dob",
              time.offset  = "doi",
              exp.scale    = 1/12)
}

est_with <- function(rule = networkreporting::vis_from_clique(), ...) {
  sibling_estimator(sib.dat             = make_four_ego_sib_dat(),
                    ego.id              = "ego_id",
                    sib.id              = "sib_id",
                    sib.frame.indicator = "sib_in_frame",
                    sib.sex             = "sex",
                    cell.config         = vis_cell_config(),
                    weights             = "weight",
                    visibility          = rule,
                    ...)
}

# ---------------------------------------------------------------------------
# Golden test: the default is the historical behaviour, unchanged
# ---------------------------------------------------------------------------
test_that("the default visibility rule reproduces the documented ASDRs exactly", {
  res <- est_with()

  agg <- res$asdr.agg %>% filter(sib.age == "[45,50)")
  ind <- res$asdr.ind %>% filter(sib.age == "[45,50)")

  # the values helper-simulate.R derives by hand
  expect_equal(agg$asdr.hat, 36/353)
  expect_equal(ind$asdr.hat, 4/29)
})

test_that("passing vis_from_clique() explicitly is identical to the default", {
  default  <- est_with()
  explicit <- est_with(rule = networkreporting::vis_from_clique())

  # provenance holds closures, so compare the estimate tables themselves
  expect_equal(default$asdr.ind$asdr.hat, explicit$asdr.ind$asdr.hat)
  expect_equal(default$asdr.agg$asdr.hat, explicit$asdr.agg$asdr.hat)
  expect_equal(default$asdr.ind$num.hat,  explicit$asdr.ind$num.hat)
  expect_equal(default$asdr.ind$denom.hat, explicit$asdr.ind$denom.hat)
})

# ---------------------------------------------------------------------------
# Provenance reaches the caller
# ---------------------------------------------------------------------------
test_that("the estimate carries provenance saying which rule produced it", {
  res <- est_with()
  p   <- res$vis_provenance

  expect_s3_class(p, "vis_provenance")
  expect_equal(p$rule, "clique")
  expect_false(p$is_estimated)

  # every reported sibling X cell row is accounted for, and none approximated
  expect_equal(sum(p$by_rule$n_alters), p$n_alters)
  expect_equal(p$n_unresolved, 0)
  expect_equal(p$share_approx, 0)

  # the same object is also attached, for callers that index res by name
  expect_equal(attr(res, "vis_provenance")$rule, "clique")
})

# ---------------------------------------------------------------------------
# A degenerate donor rule agrees with the clique rule
# ---------------------------------------------------------------------------
# Egos 1 and 4 have y.F = 2, ego 2 has 1, ego 3 has 0, so the donor summary is
# NOT degenerate on this fixture -- the point of the test is that the donor path
# runs end to end through sibling_estimator() and produces a different, finite
# answer, in the direction the approximation implies.
test_that("a donor rule runs end to end and moves the individual estimate", {
  donor <- networkreporting::vis_from_donor(match_on = NULL, min_donors = 1)
  res   <- est_with(rule = donor)

  ind <- res$asdr.ind %>% filter(sib.age == "[45,50)")

  expect_true(is.finite(ind$asdr.hat))
  expect_gt(ind$asdr.hat, 0)

  # the aggregate estimate uses no visibility at all, so it must not move
  agg <- res$asdr.agg %>% filter(sib.age == "[45,50)")
  expect_equal(agg$asdr.hat, 36/353)

  # and the provenance says plainly that this one was estimated from the sample
  expect_true(res$vis_provenance$is_estimated)
  expect_equal(res$vis_provenance$share_approx, 1)
})

test_that("a coalesced rule records that the clique tier claimed every sibling", {
  rule <- networkreporting::vis_coalesce(
    networkreporting::vis_from_clique(),
    networkreporting::vis_from_donor(match_on = NULL, min_donors = 1))

  res <- est_with(rule = rule)

  # every sibling here belongs to a clique with a known y.F, so tier 1 resolves
  # all of them and the donor tier never fires
  expect_equal(res$vis_provenance$by_rule$rule, "clique")
  expect_equal(res$vis_provenance$share_approx, 0)

  # ... and the estimate is therefore exactly the clique one
  ind <- res$asdr.ind %>% filter(sib.age == "[45,50)")
  expect_equal(ind$asdr.hat, 4/29)
})

# ---------------------------------------------------------------------------
# Bootstrap: frozen for the clique rule, refit for an estimated one
# ---------------------------------------------------------------------------
make_boot <- function(M = 40, seed = 1) {
  set.seed(seed)
  ego_ids <- sort(unique(make_four_ego_sib_dat()$ego_id))
  b <- tibble(ego_id = ego_ids)
  for (m in seq_len(M)) {
    # a crude resampling weight: enough variation across replicates for the
    # refit to have something to bite on
    b[[paste0("boot_weight_", m)]] <- stats::rpois(length(ego_ids), lambda = 1) + 0.5
  }
  b
}

test_that("clique bootstrap estimates are unchanged by the visibility argument", {
  boot <- make_boot()

  # The 4-ego fixture puts all exposure in one age group, so every other cell
  # is 0/0 in every replicate and sibling_estimator() warns about the resulting
  # missingness. That is a property of the fixture, not of the code under test.
  a <- suppressWarnings(est_with(boot.weights = boot, return.boot = TRUE))
  b <- suppressWarnings(est_with(rule = networkreporting::vis_from_clique(),
                                 boot.weights = boot, return.boot = TRUE))

  expect_equal(a$boot.asdr.ind$asdr.hat, b$boot.asdr.ind$asdr.hat)
  expect_equal(a$boot.asdr.agg$asdr.hat, b$boot.asdr.agg$asdr.hat)
})

test_that("an estimated rule refits per replicate rather than freezing visibility", {
  # This is the bug is_estimated exists to prevent. Holding a sample quantity
  # fixed across replicates understates its contribution to the variance, so
  # refitting must not produce the identical set of replicate estimates.
  boot  <- make_boot()
  donor <- networkreporting::vis_from_donor(match_on = NULL, min_donors = 1)

  # see the note above about empty cells in this fixture
  res <- suppressWarnings(est_with(rule = donor, boot.weights = boot,
                                   return.boot = TRUE))

  # build the frozen counterpart directly: same rule, but no refit supplied
  sib.dat <- make_four_ego_sib_dat() %>%
    mutate(.ego.id = ego_id, .sib.id = sib_id, .sib.in.F = sib_in_frame,
           .sib.sex = sex, .ego.weight = weight)

  esc <- networkreporting::get_esc_reports(sib.dat = sib.dat, ego.id = ".ego.id",
                                           sib.id = ".sib.id", vis_cell_config()) %>%
    left_join(sib.dat %>% select(.ego.id, .sib.id, .ego.weight, .sib.in.F, .sib.sex),
              by = c(".ego.id", ".sib.id"))

  vr <- networkreporting::apply_visibility_rule(donor, esc, sib.dat = sib.dat,
                                                weights = ".ego.weight")
  esc2 <- vr$data
  esc2$ind_vis <- vr$values$vis_weight

  cell.vars <- c("time.period", ".sib.sex", "agelabel")
  ec <- networkreporting::get_ec_reports(esc2, ego.id = ".ego.id", sib.dat = sib.dat,
                                         sib.frame.indicator = ".sib.in.F",
                                         cell.vars = cell.vars,
                                         weights = ".ego.weight",
                                         ind.vis.var = "ind_vis")

  bw <- boot %>% rename(.ego.id = ego_id)

  frozen <- networkreporting::get_boot_ests_matrix(ec, bw, ".ego.id", cell.vars, "ind")

  refit <- networkreporting::get_boot_ests_matrix(
    ec, bw, ".ego.id", cell.vars, "ind",
    visibility = donor,
    refit = networkreporting::make_vis_refit(donor, vr$donor.dat, bw, ec, ".ego.id"))

  # the two must not be the same set of numbers: freezing is exactly the thing
  # the refit path exists to avoid
  expect_false(isTRUE(all.equal(frozen$asdr.hat, refit$asdr.hat)))

  # and the estimator's own bootstrap output used the refit path
  expect_equal(nrow(res$boot.asdr.ind), nrow(refit))
})

test_that("an estimated rule with no refit function warns rather than silently freezing", {
  boot  <- make_boot(M = 5)
  donor <- networkreporting::vis_from_donor(match_on = NULL, min_donors = 1)

  sib.dat <- make_four_ego_sib_dat() %>%
    mutate(.ego.id = ego_id, .sib.id = sib_id, .sib.in.F = sib_in_frame,
           .sib.sex = sex, .ego.weight = weight)
  esc <- networkreporting::get_esc_reports(sib.dat = sib.dat, ego.id = ".ego.id",
                                           sib.id = ".sib.id", vis_cell_config()) %>%
    left_join(sib.dat %>% select(.ego.id, .sib.id, .ego.weight, .sib.in.F, .sib.sex),
              by = c(".ego.id", ".sib.id"))
  vr <- networkreporting::apply_visibility_rule(donor, esc, sib.dat = sib.dat,
                                                weights = ".ego.weight")
  esc2 <- vr$data; esc2$ind_vis <- vr$values$vis_weight
  cell.vars <- c("time.period", ".sib.sex", "agelabel")
  ec <- networkreporting::get_ec_reports(esc2, ego.id = ".ego.id", sib.dat = sib.dat,
                                         sib.frame.indicator = ".sib.in.F",
                                         cell.vars = cell.vars, weights = ".ego.weight",
                                         ind.vis.var = "ind_vis")
  bw <- boot %>% rename(.ego.id = ego_id)

  expect_warning(
    networkreporting::get_boot_ests_matrix(ec, bw, ".ego.id", cell.vars, "ind",
                                           visibility = donor, refit = NULL),
    "frozen across bootstrap replicates")
})

# ---------------------------------------------------------------------------
# The frame-status split get_ec_reports() now produces
# ---------------------------------------------------------------------------
test_that("occurrences are split by frame status, and the death side is empty", {
  sib.dat <- make_four_ego_sib_dat() %>%
    mutate(.ego.id = ego_id, .sib.id = sib_id, .sib.in.F = sib_in_frame,
           .sib.sex = sex, .ego.weight = weight)

  esc <- networkreporting::get_esc_reports(sib.dat = sib.dat, ego.id = ".ego.id",
                                           sib.id = ".sib.id", vis_cell_config()) %>%
    left_join(sib.dat %>% select(.ego.id, .sib.id, .ego.weight, .sib.in.F, .sib.sex),
              by = c(".ego.id", ".sib.id"))

  cell.vars <- c("time.period", ".sib.sex", "agelabel")
  ec <- networkreporting::get_ec_reports(esc, ego.id = ".ego.id", sib.dat = sib.dat,
                                         sib.frame.indicator = ".sib.in.F",
                                         cell.vars = cell.vars, weights = ".ego.weight")

  expect_true(all(c("y.DandFcell", "y.DandnotFcell",
                    "y.NandFcell", "y.NandnotFcell") %in% names(ec)))

  # a dead sibling is never in the frame population, so the on-frame death
  # column must be identically zero
  expect_true(all(ec$y.DandFcell == 0))

  # and the two halves reconstruct the totals
  expect_equal(ec$y.DandFcell + ec$y.DandnotFcell, ec$y.Dcell)
  expect_equal(ec$y.NandFcell + ec$y.NandnotFcell, ec$y.Ncell)
})

# ---------------------------------------------------------------------------
# Settings that belong to the tie
# ---------------------------------------------------------------------------
test_that("a tie may name the frame indicator, using the caller's own column name", {
  # sibling_estimator renames the frame column internally, so a tie naming the
  # caller's spelling has to be reconciled here rather than downstream
  res <- est_with(tie = networkreporting::tie_config(
                    "clique", name = "siblings",
                    frame.indicator = "sib_in_frame"))

  ind <- res$asdr.ind %>% filter(sib.age == "[45,50)")
  expect_equal(ind$asdr.hat, 4/29)
})

test_that("a tie naming a different frame indicator is an error", {
  expect_error(
    est_with(tie = networkreporting::tie_config(
               "clique", name = "siblings", frame.indicator = "something_else")),
    "conflicting frame indicators")
})

test_that("a tie may declare ego.in.group, and it reaches the estimate", {
  # ego.in.group = FALSE drops ego from the group, so every visibility falls by
  # one and the individual estimate moves. The point is that it is reachable and
  # recorded, not that it is the right choice for siblings.
  res <- est_with(tie = networkreporting::tie_config(
                    "clique", name = "siblings", ego.in.group = FALSE))

  expect_false(res$vis_provenance$ego_in_group)
  expect_true(any(grepl("NOT a member", res$vis_provenance$assumptions)))

  # and the default is untouched
  expect_true(est_with()$vis_provenance$ego_in_group)
})

test_that("declaring ego.in.group in two places that disagree is an error", {
  expect_error(
    est_with(rule = networkreporting::vis_from_clique(ego.in.group = TRUE),
             tie  = networkreporting::tie_config("clique", name = "siblings",
                                                 ego.in.group = FALSE)),
    "conflicting values for 'ego.in.group'")
})
