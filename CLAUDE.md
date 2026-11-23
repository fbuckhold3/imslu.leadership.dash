# imslu.leadership.dash: notes for Claude

Leadership dashboard (chiefs + PD) on Posit Connect Cloud. See README.md.

## Rules specific to this repo

- **Public GitHub repo.** Never commit data (`.rds`/`.csv`), tokens, access
  codes or resident names in examples. The access code is
  `LEADERSHIP_ACCESS_CODE` (env var only).
- **Read-only.** RDM prod via `RDM_TOKEN`. No writes anywhere in this app.
- **Evaluations are counts only** (Fred, 2026-11-23). Never display evaluator
  names (`ass_faculty`), peer evaluator ids, or plus/delta text.
  `ass_plus`/`ass_delta` are stripped in `load_data.R` right after the pull.
- **Copied metric files** (`R/constants.R`, `R/metrics_duty.R`,
  `R/metrics_conference.R`, `R/metrics_evals.R`, `R/metrics_knowledge.R`)
  come from `imslu.program.dash`. Fred chose "copy now, extract into a
  shared package later". A threshold or rule change goes in program.dash
  first, then gets re-copied here. App-only logic goes in
  `R/metrics_leadership.R`.
- Duty-hour and conference rules live in `amiontools`. Don't re-derive them.
- Chiefs and PD see the same thing for now. If roles are added, the gate in
  `R/mod_auth.R` is where it starts.

## Local run

- The `imslu.leadership.dash` entry in `rounds/.claude/launch.json` (port 3856)
  runs with the program.dash renv library (`R_LIBS`), which has amiontools
  873cfa9 + iteviz + roundsui. Without `LEADERSHIP_ACCESS_CODE` set, the
  local code defaults to `localdev`.
- macOS R doesn't support `%-d` in `format()`. Use `fmt_md()` / `fmt_date()`
  from `R/ui_helpers.R`.

## Not done yet

- `renv.lock` + `manifest.json` (Fred: `renv::init()`/`snapshot()` then
  `rsconnect::writeManifest()`). Connect Cloud can't deploy without them.
- PEAC data source.
