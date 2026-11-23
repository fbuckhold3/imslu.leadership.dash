# COPIED from imslu.program.dash@a9b96c7 R/metrics_conference.R — keep in sync until these
# metrics move into a shared package (Fred, 2026-11-23). Edit there first.

# metrics_conference.R — noon conference attendance from the activity table.
#
# "Attended" = expected AND logged (amiontools rule). SLUH- and VA-expected
# days are kept apart because VA logging is near zero (plan Q8).

conf_window <- function(activity, from, to) {
  activity |>
    filter(Date >= from, Date <= to) |>
    group_by(record_id) |>
    summarise(expected      = sum(conf_expected != "None"),
              attended      = sum(conf_attended),
              expected_sluh = sum(conf_expected == "SLUH"),
              attended_sluh = sum(conf_attended & conf_expected == "SLUH"),
              expected_va   = sum(conf_expected == "VA"),
              attended_va   = sum(conf_attended & conf_expected == "VA"),
              logged_unexpected = sum(conf_logged & conf_expected == "None"),
              afternoon_school  = sum(afternoon_school),
              .groups = "drop") |>
    mutate(rate      = ifelse(expected > 0, attended / expected, NA_real_),
           rate_sluh = ifelse(expected_sluh > 0, attended_sluh / expected_sluh, NA_real_),
           rate_va   = ifelse(expected_va > 0, attended_va / expected_va, NA_real_))
}

# Per resident: report week, last 4 weeks, AY; days since last log.
conf_resident_status <- function(d) {
  a  <- d$activity |> filter(record_id %in% d$roster$record_id)
  wk <- conf_window(a, d$week$start, d$week$end)
  w4 <- conf_window(a, d$week$end - 27, d$week$end)
  ay <- conf_window(a, d$ay_start, d$week$end)
  last_log <- a |>
    filter(conf_logged, Date <= d$week$end) |>
    group_by(record_id) |>
    summarise(last_log = max(Date), .groups = "drop")

  d$roster |>
    select(record_id, resident, class, prelim) |>
    left_join(wk |> select(record_id, wk_expected = expected, wk_attended = attended, wk_rate = rate), by = "record_id") |>
    left_join(w4 |> select(record_id, w4_expected = expected, w4_attended = attended, w4_rate = rate), by = "record_id") |>
    left_join(ay |> select(record_id, ay_expected = expected, ay_attended = attended, ay_rate = rate,
                           ay_rate_sluh = rate_sluh, ay_rate_va = rate_va,
                           ay_expected_va = expected_va, ay_afternoon_school = afternoon_school,
                           ay_logged_unexpected = logged_unexpected),
              by = "record_id") |>
    left_join(last_log, by = "record_id") |>
    mutate(days_since_log = as.integer(d$week$end - last_log))
}

# Pooled rate by class (and program) for a window, split SLUH / VA.
conf_class_rates <- function(d, from, to) {
  a <- d$activity |>
    filter(Date >= from, Date <= to) |>
    inner_join(d$roster |> select(record_id, class), by = "record_id")
  bind_rows(a |> mutate(class = as.character(class)),
            a |> mutate(class = "Program")) |>
    group_by(class) |>
    summarise(rate      = sum(conf_attended) / pmax(sum(conf_expected != "None"), 1),
              rate_sluh = sum(conf_attended & conf_expected == "SLUH") / pmax(sum(conf_expected == "SLUH"), 1),
              rate_va   = sum(conf_attended & conf_expected == "VA") / pmax(sum(conf_expected == "VA"), 1),
              expected  = sum(conf_expected != "None"),
              .groups = "drop") |>
    mutate(class = factor(class, levels = c(CLASS_LEVELS, "Program")))
}

# Weekly program rate across the AY (trend line), split SLUH / VA.
conf_weekly_trend <- function(d) {
  d$activity |>
    filter(Date >= d$ay_start, Date <= d$week$end, record_id %in% d$roster$record_id) |>
    group_by(week_start) |>
    summarise(All  = sum(conf_attended) / pmax(sum(conf_expected != "None"), 1),
              SLUH = sum(conf_attended & conf_expected == "SLUH") / pmax(sum(conf_expected == "SLUH"), 1),
              VA   = sum(conf_attended & conf_expected == "VA") / pmax(sum(conf_expected == "VA"), 1),
              .groups = "drop") |>
    tidyr::pivot_longer(-week_start, names_to = "site", values_to = "rate")
}
