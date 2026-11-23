# load_data.R — the one place the app reads data.
#
# Read-only. RDM prod (RDM_TOKEN) + live Amion via amiontools. Adapted from
# imslu.program.dash R/load_program_data.R (same pulls, same roster rule) so
# the numbers match the weekly report. Differences from that file:
#   - free-text assessment comments (ass_plus / ass_delta) are dropped right
#     after the pull and never held in app memory (Fred, 2026-11-23: counts
#     only, no plus/delta);
#   - every per-domain status table is computed once here, not per session.
#
# Any failed pull stops the load on purpose: a half-loaded dashboard that
# looks complete is worse than an error message.

suppressPackageStartupMessages({
  library(dplyr)
})

rdm_credentials <- function() {
  token <- Sys.getenv("RDM_TOKEN")
  url   <- Sys.getenv("REDCAP_URL")
  if (!nzchar(token) || !nzchar(url)) {
    stop("RDM_TOKEN and REDCAP_URL must be set (~/.Renviron locally, Connect env vars when deployed).")
  }
  list(token = token, url = url)
}

.rd <- function(cred, fields = NULL, forms = NULL, raw_or_label = "raw") {
  out <- REDCapR::redcap_read(redcap_uri = cred$url, token = cred$token,
                              fields = fields, forms = forms,
                              raw_or_label = raw_or_label, verbose = FALSE)
  if (!out$success) stop("REDCap pull failed: ", out$outcome_message)
  tibble::as_tibble(out$data) |> mutate(record_id = as.character(record_id))
}

.repeat_rows <- function(df, instrument) {
  if (!"redcap_repeat_instrument" %in% names(df)) return(df[0, ])
  df |> filter(!is.na(redcap_repeat_instrument),
               tolower(redcap_repeat_instrument) == tolower(instrument))
}

.base_rows <- function(df) {
  if (!"redcap_repeat_instrument" %in% names(df)) return(df)
  df |> filter(is.na(redcap_repeat_instrument))
}

# Active residents with their class for the academic year starting `ay`.
# Same rule as program.dash / the PCEC report / the MKSAP import.
build_roster <- function(res, ay) {
  res |>
    .base_rows() |>
    mutate(grad_yr = suppressWarnings(as.integer(as.character(grad_yr))),
           years_left = grad_yr - ay,
           class = case_when(
             type == "Categorical" & years_left == 1L ~ "PGY3",
             type == "Categorical" & years_left == 2L ~ "PGY2",
             type == "Categorical" & years_left == 3L ~ "Intern",
             type == "Preliminary" & years_left == 1L ~ "Intern",
             TRUE ~ NA_character_),
           class = factor(class, levels = CLASS_LEVELS),
           prelim = type == "Preliminary",
           resident = coalesce(
             if_else(!is.na(last_name) & !is.na(first_name),
                     paste0(last_name, ", ", first_name), NA_character_),
             name)) |>
    filter(!record_id %in% TEST_RECORDS,
           type %in% c("Categorical", "Preliminary"),
           is.na(res_archive) | res_archive != "Yes",
           !is.na(class),
           !grepl("^test$", tolower(coalesce(first_name, ""))),
           !grepl("^test$", tolower(coalesce(last_name, "")))) |>
    select(record_id, resident, class, prelim, type, grad_yr, clinic_firm) |>
    arrange(class, resident)
}

# Free-text comment columns are replaced by a per-pull content id before
# anything else sees the data: dedupe_assessments() still tells two
# instances apart by their comments, but the text itself is gone.
.strip_assessment_text <- function(assessments) {
  text_cols <- intersect(c("ass_plus", "ass_delta"), names(assessments))
  if (!length(text_cols)) return(assessments)
  key <- do.call(paste, c(lapply(assessments[text_cols], as.character), sep = "\u001f"))
  assessments$ass_text_id <- as.integer(factor(key))
  assessments[setdiff(names(assessments), text_cols)]
}

#' Pull everything the dashboard needs and precompute the status tables.
#'
#' @param as_of Date the dashboard is "as of" (normally today).
#' @return list(as_of, ay, ay_start, week, roster, activity, assessments,
#'   faculty_evals, peer, ite, mksap, status = list(duty, conf, evals, ite,
#'   mksap), fetched_at)
load_dashboard_data <- function(as_of = Sys.Date(), cred = rdm_credentials()) {
  ay_start <- ay_start_of(as_of)
  ay       <- as.integer(format(ay_start, "%Y"))

  res <- .rd(cred, raw_or_label = "label",
             fields = c("record_id", "name", "first_name", "last_name", "type",
                        "grad_yr", "res_archive", "clinic_firm"))
  roster <- build_roster(res, ay)

  # Whole assessment form: the type classifier reads which item block is
  # filled, and the CC dedupe compares full content (PCEC report rule).
  assessments <- .rd(cred, forms = "assessment", raw_or_label = "label") |>
    .repeat_rows("assessment") |>
    .strip_assessment_text() |>
    mutate(ass_date = as.Date(ass_date)) |>
    filter(!is.na(ass_date))

  faculty_evals <- .rd(cred, fields = c("record_id", "fac_eval_date")) |>
    .repeat_rows("faculty_evaluation") |>
    mutate(fac_eval_date = as.Date(fac_eval_date)) |>
    filter(!is.na(fac_eval_date))

  # peer_evaluator_id is only ever counted, never displayed.
  peer <- .rd(cred, fields = c("record_id", "peer_date", "peer_evaluator_id")) |>
    .repeat_rows("peer_eval") |>
    mutate(peer_date = as.Date(peer_date),
           peer_evaluator_id = as.character(peer_evaluator_id))

  ite_fields <- c("record_id",
                  as.vector(outer(c("pgy1", "pgy2", "pgy3"),
                                  c("tot_correct", "total_ile", ITE_AREAS),
                                  function(p, f) paste0(p, "_", f, ifelse(f %in% ITE_AREAS, "_ile", "")))))
  ite <- .rd(cred, fields = ite_fields) |> .base_rows()

  mksap <- .rd(cred, fields = c("record_id", "mk_export_date", "mk_tracker",
                                "mk_total_questions", "mk_n_complete",
                                "mk_pct_complete", "mk_n_correct", "mk_pct_correct")) |>
    .repeat_rows("mksap_progress") |>
    mutate(mk_export_date = as.Date(mk_export_date))

  # Live Amion + questions log + duty-hour log; ~20 s. Covers the whole AY,
  # including future (anticipated) days.
  activity <- amiontools::build_resident_activity(rdm_token = cred$token,
                                                  redcap_url = cred$url,
                                                  ay_start = ay)
  activity$record_id <- as.character(activity$record_id)

  d <- list(as_of = as_of, ay = ay, ay_start = ay_start, week = report_week(as_of),
            roster = roster, activity = activity, assessments = assessments,
            faculty_evals = faculty_evals, peer = peer, ite = ite, mksap = mksap)

  ite_status <- ite_resident_status(d)
  d$status <- list(
    duty  = duty_resident_status(d),
    conf  = conf_resident_status(d),
    evals = evals_resident_status(d),
    ite   = ite_status,
    mksap = mksap_resident_status(d, ite_status)
  )
  d$fetched_at <- Sys.time()
  d
}

# ── Shared in-memory cache ─────────────────────────────────────────────────
# One copy per R process, shared by every session. The first visitor after
# a cold start (or after DATA_MAX_AGE_HOURS) waits for the ~30-40 s pull;
# everyone else gets the cached copy. "Refresh data" forces a new pull.
.data_cache <- new.env(parent = emptyenv())

get_dashboard_data <- function(force = FALSE) {
  fresh <- !is.null(.data_cache$d) &&
    difftime(Sys.time(), .data_cache$d$fetched_at, units = "hours") < DATA_MAX_AGE_HOURS
  # Local dev only: LD_DATA_RDS points at a saved load_dashboard_data()
  # result so UI work doesn't re-pull prod every restart. Never set on Connect.
  dev_rds <- Sys.getenv("LD_DATA_RDS")
  if (is.null(.data_cache$d) && nzchar(dev_rds) && file.exists(dev_rds)) {
    .data_cache$d <- readRDS(dev_rds)
    .data_cache$d$fetched_at <- Sys.time()
    return(.data_cache$d)
  }
  if (force || !fresh) {
    .data_cache$d <- load_dashboard_data()
  }
  .data_cache$d
}
