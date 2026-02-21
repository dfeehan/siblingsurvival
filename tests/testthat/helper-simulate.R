# Helper functions for constructing synthetic sibling datasets
# used across multiple test files.

# Build the canonical 4-ego test dataset used in sibling_estimator tests.
#
# Design (all dates in months):
#   doi = 600 (= 50 years), dob = 0 for every sibling.
#   Time period "5yr_beforeinterview" → absolute window [540, 600).
#   During that window every sib is aged 540-600 months (45-50 years),
#   so all exposure falls in the "[45,50)" 5-year age group.
#
#   Ego 1: sibs A, B  both alive     → 60+60=120 months exp, 0 deaths, y.F=2
#   Ego 2: sib  C dies at 570,
#           sib  D alive             → 30+60= 90 months exp, 1 death,  y.F=1
#   Ego 3: sibs E, F both die at 550 → 10+10= 20 months exp, 2 deaths, y.F=0
#   Ego 4: sibs G, H both alive      → 60+60=120 months exp, 0 deaths, y.F=2
#
# Expected rates (with exp.scale = 1/12 → person-years):
#   ASDR.agg = 3 / (350/12)  = 36/350 per year
#   ASDR.ind = 2.5 / (215/12) = 30/215 per year
#     (ind_vis: alive sib = 1/y.F, dead sib = 1/(y.F+1))
make_four_ego_sib_dat <- function() {
  doi <- 600
  dob <- 0
  dod <- c(-1, -1, 570, -1, 550, 550, -1, -1)

  tibble::tibble(
    ego_id       = c(1,   1,   2,   2,   3,   3,   4,   4),
    sib_id       = c("A","B","C","D","E","F","G","H"),
    dob          = dob,
    doi          = doi,
    dod          = dod,
    # observation starts at birth; ends at death or interview
    start_obs    = dob,
    end_obs      = ifelse(dod == -1, doi, dod),
    # alive siblings are in the frame population; dead ones are not
    sib_in_frame = c(1, 1, 0, 1, 0, 0, 1, 1),
    sex          = "f",
    weight       = 1
  )
}
