# COPIED from imslu.program.dash@a9b96c7 R/metrics_duty.R — keep in sync until these
# metrics move into a shared package (Fred, 2026-11-23). Edit there first.

# metrics_duty.R — duty hours: recorded vs Amion estimate vs needs entry.
#
# Everything reads amiontools' activity table (build_resident_activity())
# and summarise_activity(), so these numbers match ind.dash and the Friday
# digest by construction. Nothing here re-derives a duty-hour rule.

VERIFIED_STATUS <- c("confirmed", "entered")

# Per-resident weekly rows through the report week, joined to the roster
# class (the roster, not Amion's Level, decides class everywhere here).
duty_weekly <- function(d) {
  # summarise_activity() drops its has_duty_data / all_vacation helpers, so
  # the "clean week" test is rebuilt here with the same definitions.
  wk_flags <- d$activity |>
    filter(Date <= d$week$end) |>
    group_by(record_id, period_start = week_start) |>
    summarise(has_duty_data = any(!is.na(duty_hours)),
              all_vacation  = all(is_vacation[!is.na(duty_hours)]),
              .groups = "drop")
  amiontools::summarise_activity(d$activity, "week", "resident", through = d$week$end) |>
    inner_join(d$roster |> select(record_id, resident, class, prelim), by = "record_id") |>
    left_join(wk_flags, by = c("record_id", "period_start"))
}

# Share of each resident's hours that are still Amion estimates, over a
# date window. Needs-entry days count 0 h, so they show up as "open days".
duty_provenance_window <- function(activity, from, to) {
  activity |>
    filter(Date >= from, Date <= to) |>
    group_by(record_id) |>
    summarise(hours_total     = sum(duty_hours, na.rm = TRUE),
              hours_estimated = sum(duty_hours[duty_status == "amion_default"], na.rm = TRUE),
              days_verified   = sum(duty_status %in% VERIFIED_STATUS),
              days_estimated  = sum(duty_status == "amion_default"),
              days_needs_entry = sum(duty_status == "needs_entry"),
              .groups = "drop") |>
    mutate(pct_estimated = ifelse(hours_total > 0, hours_estimated / hours_total, NA_real_),
           open_days     = days_estimated + days_needs_entry)
}

# One row per resident for the report week: hours, 4-week rolling values,
# flags, and how much of the 4-week window is an estimate.
duty_resident_status <- function(d) {
  wk  <- duty_weekly(d)
  cur <- wk |> filter(period_start == d$week$start)
  prov4 <- duty_provenance_window(d$activity, d$week$end - 27, d$week$end)
  open  <- duty_provenance_window(d$activity, d$week$end - THRESH$duty_entry_window + 1, d$week$end) |>
    select(record_id, open_days_window = open_days,
           needs_entry_window = days_needs_entry, unconfirmed_window = days_estimated)

  d$roster |>
    select(record_id, resident, class, prelim) |>
    left_join(cur |> select(record_id, week_hours = duty_hours,
                            avg4_hours = rolling_4wk_avg_hours,
                            avg4_days = rolling_4wk_days_per_week,
                            flag_80h, flag_days_over_target, flag_days_off_violation),
              by = "record_id") |>
    left_join(prov4 |> select(record_id, pct_estimated_4wk = pct_estimated), by = "record_id") |>
    left_join(open, by = "record_id") |>
    mutate(across(c(open_days_window, needs_entry_window, unconfirmed_window), ~ coalesce(.x, 0L)))
}

# Program day counts by provenance for each AY week (the stacked chart).
duty_provenance_weekly <- function(d) {
  d$activity |>
    filter(Date <= d$week$end, record_id %in% d$roster$record_id) |>
    mutate(source = case_when(
      duty_status %in% VERIFIED_STATUS ~ "Resident-verified",
      duty_status == "amion_default"   ~ "Amion estimate",
      duty_status == "needs_entry"     ~ "Needs entry",
      TRUE ~ NA_character_)) |>
    filter(!is.na(source)) |>
    count(week_start, source, name = "days") |>
    mutate(source = factor(source, levels = c("Resident-verified", "Amion estimate", "Needs entry")))
}

# Weekly hours distribution by class (band chart). Weeks that are all
# vacation, or that contain needs-entry days, are dropped when
# `clean = TRUE`, the same toggle as the 10-25 prototype deck.
duty_class_bands <- function(d, clean = TRUE) {
  wk <- duty_weekly(d)
  if (clean) wk <- wk |> filter(!all_vacation, days_needs_entry == 0, has_duty_data)
  wk |>
    group_by(class, period_start) |>
    summarise(n = n(),
              mean = mean(duty_hours),
              p25 = quantile(duty_hours, .25), p75 = quantile(duty_hours, .75),
              p10 = quantile(duty_hours, .10), p90 = quantile(duty_hours, .90),
              .groups = "drop")
}

# AY-to-date class averages (clean weeks only), as quoted in the deck.
duty_class_averages <- function(d) {
  duty_weekly(d) |>
    filter(!all_vacation, days_needs_entry == 0, has_duty_data) |>
    group_by(class) |>
    summarise(residents = n_distinct(record_id),
              avg_weekly_hours = mean(duty_hours),
              weeks_over_80 = sum(duty_hours > 80),
              .groups = "drop")
}

# Program-wide verified share, report week and AY.
duty_verified_share <- function(d) {
  f <- function(from) {
    a <- d$activity |> filter(Date >= from, Date <= d$week$end,
                              record_id %in% d$roster$record_id,
                              duty_status %in% c(VERIFIED_STATUS, "amion_default", "needs_entry"))
    c(days = nrow(a), verified = sum(a$duty_status %in% VERIFIED_STATUS),
      residents_ever = n_distinct(a$record_id[a$duty_status %in% VERIFIED_STATUS]))
  }
  list(week = f(d$week$start), ay = f(d$ay_start))
}
