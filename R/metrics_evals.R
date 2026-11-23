# COPIED from imslu.program.dash@a9b96c7 R/metrics_evals.R — keep in sync until these
# metrics move into a shared package (Fred, 2026-11-23). Edit there first.

# metrics_evals.R — assessments received, faculty evals done, peer reviews.
#
# The assessment-type classifier lives here (plan Q10). It covers the
# newer v2 / Diamond / Bridge item blocks that imslu.email's
# determine_assessment_type() sends to "Other". Moving it into gmed is a
# separate, approved change.

# First matching item block wins. Order matters only for rows that fill
# more than one block (none seen in prod as of 2026-10-31).
ASSESSMENT_TYPES <- c(
  "^ass_cc_"      = "Continuity clinic",
  "^ass_obs_"     = "Observation",
  "^ass_day_"     = "Clinic day",
  "^ass_cons_"    = "Consults",
  "^ass_intern2_" = "Intern inpatient",
  "^ass_int_ip_"  = "Intern inpatient",
  "^ass_senior2_" = "Senior inpatient",
  "^ass_res_ip_"  = "Senior inpatient",
  "^ass_diamond_" = "Diamond team",
  "^ass_bridge_"  = "Bridge"
)

classify_assessments <- function(assessments) {
  type <- rep(NA_character_, nrow(assessments))
  for (pat in names(ASSESSMENT_TYPES)) {
    cols <- grep(pat, names(assessments), value = TRUE)
    if (!length(cols)) next
    filled <- rowSums(!is.na(assessments[cols]) &
                        trimws(as.matrix(assessments[cols])) != "") > 0
    type[is.na(type) & filled] <- ASSESSMENT_TYPES[[pat]]
  }
  assessments$ass_type <- dplyr::coalesce(type, "Other")
  assessments
}

# Drop exact-duplicate instances (identical content, different instance
# number); keeps the lowest instance. Copied from pcec-assessment-report —
# the CC double-submission bug since Aug 2026 makes this matter.
dedupe_assessments <- function(assessments) {
  assessments <- dplyr::arrange(assessments, record_id, redcap_repeat_instance)
  content_cols <- setdiff(names(assessments),
                          c("redcap_repeat_instance", "assessment_complete"))
  key <- do.call(paste, c(assessments[content_cols], sep = "\u001f"))
  assessments[!duplicated(key), , drop = FALSE]
}

# AY assessments on the active roster, deduped and typed.
evals_received_ay <- function(d) {
  d$assessments |>
    filter(record_id %in% d$roster$record_id,
           ass_date >= d$ay_start, ass_date <= d$week$end) |>
    dedupe_assessments() |>
    classify_assessments()
}

# Non-vacation days per resident in a window (0 = away the whole window,
# so a zero count there isn't "behind").
working_days <- function(activity, from, to) {
  activity |>
    filter(Date >= from, Date <= to) |>
    group_by(record_id) |>
    summarise(working_days = sum(!is_vacation), .groups = "drop")
}

evals_resident_status <- function(d) {
  ass  <- evals_received_ay(d)
  from <- d$week$end - THRESH$evals_window + 1
  fe   <- d$faculty_evals |> filter(fac_eval_date >= d$ay_start, fac_eval_date <= d$week$end)
  pe   <- d$peer |> filter(!is.na(peer_date), peer_date >= d$ay_start, peer_date <= d$week$end)

  received <- ass |>
    group_by(record_id) |>
    summarise(rec_week = sum(ass_date >= d$week$start),
              rec_window = sum(ass_date >= from),
              rec_ay = n(),
              last_received = max(ass_date),
              .groups = "drop")
  done <- fe |>
    group_by(record_id) |>
    summarise(fac_week = sum(fac_eval_date >= d$week$start),
              fac_window = sum(fac_eval_date >= from),
              fac_ay = n(), .groups = "drop")
  peer_done <- pe |> filter(!is.na(peer_evaluator_id), peer_evaluator_id != "") |>
    count(record_id = peer_evaluator_id, name = "peer_done_ay")
  peer_recv <- pe |> count(record_id, name = "peer_received_ay")

  d$roster |>
    select(record_id, resident, class, prelim) |>
    left_join(received, by = "record_id") |>
    left_join(done, by = "record_id") |>
    left_join(peer_done, by = "record_id") |>
    left_join(peer_recv, by = "record_id") |>
    left_join(working_days(d$activity, from, d$week$end), by = "record_id") |>
    mutate(across(c(rec_week, rec_window, rec_ay, fac_week, fac_window, fac_ay,
                    peer_done_ay, peer_received_ay), ~ coalesce(.x, 0L)),
           working_days = coalesce(working_days, THRESH$evals_window),
           days_since_received = as.integer(d$week$end - last_received)) |>
    group_by(class) |>
    mutate(rec_ay_class_q25 = quantile(rec_ay, THRESH$class_low_quantile)) |>
    ungroup()
}

# Counts by class x type, for the report week / 28 days / AY.
evals_by_type <- function(d) {
  ass  <- evals_received_ay(d) |> inner_join(d$roster |> select(record_id, class), by = "record_id")
  from <- d$week$end - THRESH$evals_window + 1
  ass |>
    group_by(class, ass_type) |>
    summarise(week = sum(ass_date >= d$week$start), last_28 = sum(ass_date >= from),
              ay = n(), .groups = "drop") |>
    arrange(class, desc(ay))
}

# Weekly program totals across the AY (received vs faculty evals done).
evals_weekly_trend <- function(d) {
  wk <- function(x) as.Date(lubridate::floor_date(x, "week", week_start = 7))
  rec <- evals_received_ay(d) |> count(week_start = wk(ass_date), name = "Assessments received")
  don <- d$faculty_evals |>
    filter(record_id %in% d$roster$record_id,
           fac_eval_date >= d$ay_start, fac_eval_date <= d$week$end) |>
    count(week_start = wk(fac_eval_date), name = "Faculty evals done")
  full_join(rec, don, by = "week_start") |>
    tidyr::pivot_longer(-week_start, names_to = "series", values_to = "n") |>
    mutate(n = coalesce(n, 0L))
}
