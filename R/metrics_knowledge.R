# COPIED from imslu.program.dash@a9b96c7 R/metrics_knowledge.R — keep in sync until these
# metrics move into a shared package (Fred, 2026-11-23). Edit there first.

# metrics_knowledge.R — ITE (ABIM pass prediction) and MKSAP pace.
#
# ITE risk comes from iteviz (R mirror of im-ite-analyzer's nomogram.py): same
# McDonald 2020 curves, tiers at ITE_CUTOFFS, estimates shown rounded DOWN to
# 5% ("~85%").

# Latest ITE per resident: the highest PGY year with a total % correct.
ite_resident_status <- function(d) {
  long <- d$ite |>
    select(record_id, matches("^pgy[123]_")) |>
    tidyr::pivot_longer(-record_id, names_to = c("pgy", ".value"),
                        names_pattern = "^pgy([123])_(.*)$") |>
    mutate(pgy = as.integer(pgy),
           tot_correct = suppressWarnings(as.numeric(tot_correct))) |>
    filter(!is.na(tot_correct))
  prev <- long |>
    group_by(record_id) |>
    arrange(pgy, .by_group = TRUE) |>
    mutate(prev_prob = lag(iteviz::pass_prob(tot_correct, pgy))) |>
    ungroup()
  latest <- prev |>
    group_by(record_id) |>
    slice_max(pgy, n = 1, with_ties = FALSE) |>
    ungroup() |>
    mutate(pass_prob = iteviz::pass_prob(tot_correct, pgy),
           tier = iteviz::risk_tier(pass_prob, ITE_CUTOFFS),
           prev_tier = iteviz::risk_tier(prev_prob, ITE_CUTOFFS),
           prob_label = iteviz::prob_label(pass_prob))

  d$roster |>
    select(record_id, resident, class, prelim) |>
    left_join(latest |> rename(ite_pgy = pgy), by = "record_id") |>
    mutate(ite_current = !is.na(ite_pgy) &
             ite_pgy == match(as.character(class), CLASS_LEVELS))
}

# Median area percentile by class, for the latest ITE of each resident.
ite_area_by_class <- function(ite_status) {
  area_cols <- paste0(ITE_AREAS, "_ile")
  ite_status |>
    filter(ite_current) |>
    select(class, all_of(intersect(area_cols, names(ite_status)))) |>
    tidyr::pivot_longer(-class, names_to = "area", values_to = "ile") |>
    mutate(ile = suppressWarnings(as.numeric(ile)),
           area = ITE_AREA_LABELS[sub("_ile$", "", area)]) |>
    group_by(class, area) |>
    summarise(median_ile = median(ile, na.rm = TRUE), n = sum(!is.na(ile)), .groups = "drop")
}

# ── MKSAP pace ────────────────────────────────────────────────────────────
# Goal: Intern 1,000 / PGY2 2,000 / PGY3 2,000, or the whole bank for a
# PGY3 below 90% predicted ABIM pass. Prelims have no goal. Expected-by-now
# is linear from July 1 to April 1 at the export date (transition year).
mksap_resident_status <- function(d, ite_status) {
  due <- as.Date(sprintf("%d-%s", d$ay + 1L, MKSAP_DUE_MMDD))
  latest <- d$mksap |>
    filter(!is.na(mk_export_date), mk_export_date <= d$as_of) |>
    group_by(record_id) |>
    arrange(mk_export_date, redcap_repeat_instance, .by_group = TRUE) |>
    mutate(prev_n_complete = lag(as.numeric(mk_n_complete)),
           prev_export_date = lag(mk_export_date)) |>
    slice_tail(n = 1) |>
    ungroup()

  d$roster |>
    select(record_id, resident, class, prelim) |>
    left_join(latest, by = "record_id") |>
    left_join(ite_status |> select(record_id, pass_prob, ite_pgy), by = "record_id") |>
    mutate(
      mk_n_complete = as.numeric(mk_n_complete),
      mk_total_questions = as.numeric(mk_total_questions),
      pgy3_all = class == "PGY3" & !is.na(pass_prob) & ite_pgy == 3 &
        pass_prob < MKSAP_PGY3_ALL_BELOW,
      goal = case_when(prelim ~ NA_real_,
                       pgy3_all ~ mk_total_questions,
                       TRUE ~ unname(MKSAP_GOAL[as.character(class)])),
      goal_label = case_when(is.na(goal) ~ "—",
                             pgy3_all ~ sprintf("All (%s)", format(goal, big.mark = ",")),
                             TRUE ~ format(goal, big.mark = ",")),
      frac = pmin(1, pmax(0, as.numeric(mk_export_date - d$ay_start) /
                             as.numeric(due - d$ay_start))),
      expected = goal * frac,
      pace = ifelse(!is.na(expected) & expected > 0, mk_n_complete / expected, NA_real_),
      days_left = as.numeric(due - d$as_of),
      per_day_needed = ifelse(!is.na(goal) & days_left > 0,
                              pmax(0, goal - mk_n_complete) / days_left, NA_real_),
      since_prev = mk_n_complete - prev_n_complete
    )
}

# Latest export date across the program (the "data as of" marker).
mksap_as_of <- function(d) {
  x <- d$mksap$mk_export_date[!is.na(d$mksap$mk_export_date)]
  if (!length(x)) return(NA)
  max(x)
}
