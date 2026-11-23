# COPIED from imslu.program.dash@a9b96c7 R/constants.R — keep in sync until these
# metrics move into a shared package (Fred, 2026-11-23). Edit there first.

# constants.R — every threshold and date rule the program dashboard uses,
# in one place so the weekly email can match them later.
#
# Thresholds are Fred's (approved 2026-10-31, plan Q14). Change them here,
# never inline in a metrics file.

# Test / non-resident records in RDM prod (same list as import_helpers.R).
TEST_RECORDS <- c("157", "194", "999", "2039", "2040", "2042", "2043")

CLASS_LEVELS <- c("Intern", "PGY2", "PGY3")

# ── Behind-grid thresholds ─────────────────────────────────────────────────
THRESH <- list(
  hours_red          = 80,   # 4-wk avg hours/week
  hours_amber        = 70,
  days_red           = 6,    # 4-wk avg days worked/week (> 6 = < 1 day off in 7)
  days_amber         = 5.5,
  duty_entry_window  = 14,   # look-back days for unconfirmed / needs-entry
  duty_entry_red     = 5,    # >= this many open days in the window
  evals_window       = 28,   # look-back days for evals received / done
  fac_evals_amber    = 1,    # exactly this many done in the window = amber
  class_low_quantile = 0.25, # "below class 25th percentile" amber rule
  mksap_amber        = 0.75, # pace >= this but < 1 = amber; below = red
  mksap_stale_days   = 35    # MKSAP export older than this = stale badge
)

# Areas shown as Watch at most and left out of the grid's Behind/Watch counts
# and sort (Fred, 2026-10-31): almost no one enters duty hours or does
# faculty or peer evals yet, so they flag nearly every resident.
# Remove a name here to make that area count again.
WATCH_ONLY_DOMAINS <- c("duty_entry", "fac_evals", "peer")

# ── MKSAP goals (Fred, 2026-10-30), due April 1 of the academic year ───────
# "Number Complete" is continuous across residency. AY 2026-27 is the
# transition year: pace runs linearly from 0 on July 1 to the goal. From
# AY 2027-28 each class starts at the prior class's goal (plan §1D-2).
MKSAP_DUE_MMDD    <- "04-01"
MKSAP_GOAL        <- c(Intern = 1000, PGY2 = 2000, PGY3 = 2000)
MKSAP_PGY3_ALL_BELOW <- 0.90  # PGY3 below this predicted ABIM pass -> goal = whole bank

# ── ABIM pass prediction (ITE) ─────────────────────────────────────────────
# Estimates, tiers and labels come from iteviz (R mirror of im-ite-analyzer's
# ite_core/nomogram.py). Tier cutoffs: High < 50% <= Intermediate < 90% <= Low.
ITE_CUTOFFS <- iteviz::DEFAULT_CUTOFFS

# ── Dates ──────────────────────────────────────────────────────────────────
ay_start_of <- function(date) {
  y <- as.integer(format(date, "%Y"))
  as.Date(sprintf("%d-07-01", ifelse(as.integer(format(date, "%m")) >= 7, y, y - 1L)))
}

# Most recent completed Sunday-Saturday week: it ends on the last Saturday
# strictly before `as_of` (a Saturday render reports the week before).
report_week <- function(as_of) {
  sat <- as_of - (as.integer(format(as_of, "%u")) %% 7L + 1L)
  list(start = sat - 6, end = sat)
}
