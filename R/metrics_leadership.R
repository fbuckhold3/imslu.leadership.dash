# metrics_leadership.R — app-only views built on the copied program.dash
# metrics. Counts only: nothing here returns evaluator names, peer
# evaluator ids, or comment text (Fred, 2026-11-23).

# ── Program summary ────────────────────────────────────────────────────────

# One row per resident, the columns the summary table shows.
resident_overview <- function(d) {
  s <- d$status
  today_rot <- d$activity |>
    filter(Date == d$as_of) |>
    distinct(record_id, .keep_all = TRUE) |>
    select(record_id, current_rotation = Rotation)

  d$roster |>
    select(record_id, resident, class, prelim) |>
    left_join(today_rot, by = "record_id") |>
    left_join(s$duty |> select(record_id, avg4_hours, avg4_days, open_days_window, flag_80h),
              by = "record_id") |>
    left_join(s$evals |> select(record_id, rec_window, rec_ay, fac_ay, peer_done_ay, peer_received_ay),
              by = "record_id") |>
    left_join(s$conf |> select(record_id, ay_rate, ay_expected, ay_attended, ay_afternoon_school),
              by = "record_id") |>
    left_join(s$mksap |> select(record_id, mk_n_complete, goal, expected, pace, mk_export_date),
              by = "record_id")
}

# Program KPIs for the tiles.
program_kpis <- function(d) {
  ov <- resident_overview(d)
  conf_prog <- conf_class_rates(d, d$ay_start, d$week$end) |> filter(class == "Program")
  list(
    residents      = nrow(ov),
    by_class       = table(ov$class),
    over_80        = sum(ov$avg4_hours > THRESH$hours_red, na.rm = TRUE),
    open_duty_days = sum(ov$open_days_window, na.rm = TRUE),
    evals_rec_ay   = sum(ov$rec_ay, na.rm = TRUE),
    evals_rec_28   = sum(ov$rec_window, na.rm = TRUE),
    fac_done_ay    = sum(ov$fac_ay, na.rm = TRUE),
    peer_done_ay   = sum(ov$peer_done_ay, na.rm = TRUE),
    conf_rate_ay   = conf_prog$rate,
    afternoon_days = sum(ov$ay_afternoon_school, na.rm = TRUE),
    mksap_behind   = sum(!ov$prelim & !is.na(ov$pace) & ov$pace < 1),
    mksap_with_goal = sum(!ov$prelim & !is.na(ov$pace)),
    mksap_as_of    = mksap_as_of(d)
  )
}

# Evaluation counts by class: received (by assessment type), faculty evals
# done, peer reviews done. AY to date.
eval_counts_by_class <- function(d) {
  ev <- d$status$evals
  ev |>
    group_by(class) |>
    summarise(residents = n(),
              received_ay = sum(rec_ay), received_28d = sum(rec_window),
              faculty_evals_done = sum(fac_ay), peer_reviews_done = sum(peer_done_ay),
              .groups = "drop") |>
    mutate(class = as.character(class)) |>
    bind_rows(ev |> summarise(class = "Program", residents = n(),
                              received_ay = sum(rec_ay), received_28d = sum(rec_window),
                              faculty_evals_done = sum(fac_ay),
                              peer_reviews_done = sum(peer_done_ay)))
}

# ── One resident ───────────────────────────────────────────────────────────

# Weekly duty hours for the whole AY. Past weeks split verified vs Amion
# estimate; future weeks are anticipated (Amion schedule). Needs-entry days
# carry 0 h, so they're counted separately.
resident_duty_weeks <- function(d, rid) {
  a <- d$activity |> filter(record_id == rid)
  if (!nrow(a)) return(NULL)
  wk <- a |>
    group_by(week_start) |>
    summarise(
      future      = all(is_future),
      verified    = sum(duty_hours[duty_status %in% VERIFIED_STATUS], na.rm = TRUE),
      estimated   = sum(duty_hours[duty_status == "amion_default"], na.rm = TRUE),
      needs_entry = sum(duty_status == "needs_entry"),
      total       = sum(duty_hours, na.rm = TRUE),
      .groups = "drop") |>
    arrange(week_start)
  wk$avg4 <- as.numeric(stats::filter(wk$total, rep(1 / 4, 4), sides = 1))
  wk
}

# Consecutive-day blocks of the same rotation category. Off days inside a
# block don't split it (weekends off on an elective are still the elective).
resident_schedule_blocks <- function(d, rid) {
  a <- d$activity |>
    filter(record_id == rid) |>
    arrange(Date) |>
    select(Date, Rotation, category, is_future)
  if (!nrow(a)) return(NULL)
  off <- is.na(a$category) | a$category %in% c("Day off", "Off", "Vacation")
  on  <- a[!off, ]
  if (!nrow(on)) return(NULL)
  new_block <- c(TRUE, on$category[-1] != on$category[-nrow(on)] |
                   as.numeric(diff(on$Date)) > 4)
  on$block <- cumsum(new_block)
  on |>
    group_by(block) |>
    summarise(start = min(Date), end = max(Date), category = first(category),
              rotations = paste(utils::head(names(sort(table(Rotation), decreasing = TRUE)), 2),
                                collapse = " / "),
              days = n(), .groups = "drop") |>
    mutate(when = case_when(end < d$as_of ~ "Past",
                            start > d$as_of ~ "Upcoming",
                            TRUE ~ "Current"))
}

# Vacation days on the schedule, AY (past + upcoming).
resident_vacation <- function(d, rid) {
  a <- d$activity |> filter(record_id == rid)
  c(taken = sum(a$is_vacation & !a$is_future), upcoming = sum(a$is_vacation & a$is_future))
}

# Assessments received, counted by type: last 28 days and AY. Also the
# number of distinct evaluators (count only; names are never shown).
resident_evals_by_type <- function(d, rid) {
  from <- d$week$end - THRESH$evals_window + 1
  ass <- evals_received_ay(d) |> filter(record_id == rid)
  list(
    by_type = ass |>
      group_by(Type = ass_type) |>
      summarise(`Last 28 days` = sum(ass_date >= from), `AY to date` = n(), .groups = "drop") |>
      arrange(desc(`AY to date`)),
    evaluators = if ("ass_faculty" %in% names(ass))
      dplyr::n_distinct(tolower(trimws(ass$ass_faculty[!is.na(ass$ass_faculty) & ass$ass_faculty != ""])))
      else NA_integer_,
    monthly = ass |> count(month = as.Date(lubridate::floor_date(ass_date, "month")), name = "n")
  )
}

# Weekly conference attendance for one resident (AY, through last week).
resident_conf_weeks <- function(d, rid) {
  d$activity |>
    filter(record_id == rid, Date >= d$ay_start, Date <= d$week$end) |>
    group_by(week_start) |>
    summarise(expected = sum(conf_expected != "None"),
              attended = sum(conf_attended),
              afternoon_school = sum(afternoon_school), .groups = "drop")
}

# All MKSAP exports for one resident (the progress line).
resident_mksap_history <- function(d, rid) {
  d$mksap |>
    filter(record_id == rid, !is.na(mk_export_date)) |>
    mutate(mk_n_complete = as.numeric(mk_n_complete)) |>
    arrange(mk_export_date)
}
