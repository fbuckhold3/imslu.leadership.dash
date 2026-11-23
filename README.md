# imslu.leadership.dash

Shiny dashboard for IMSLU internal medicine program leadership (chief
residents + program director). It is read-only and gated by an access code.

## What it shows

**Program summary**
- KPI tiles: residents by class, 4-week duty average over 80 h, open
  duty-hour days, evaluations received, faculty evals and peer reviews
  done, conference attendance, Afternoon School days, MKSAP pace.
- Evaluation counts by class, conference attendance by class (SLUH/VA),
  average weekly duty hours by class, MKSAP pace by class.
- A table of all residents. Click a row to open that resident.

**Resident view** (one tab per area)
- **Duty hours:** weekly hours for the whole academic year. Past weeks are
  split into resident-verified hours and Amion estimates; future weeks show
  anticipated hours from the Amion schedule. The chart includes the 4-week
  average and the 80 h line.
- **Schedule:** rotation blocks for the AY (timeline + table), the current
  and next block, and vacation days.
- **Evaluations:** counts only. Assessments received (by type and by
  month), number of distinct evaluators, faculty evals completed, and peer
  reviews completed. No evaluator names, no peer evaluator identities, and
  no plus/delta text.
- **Conference:** noon conference rate (SLUH/VA split), last 4 weeks, and
  Afternoon School days (REDCap questions log, `q_conference_type = 3`).
- **MKSAP:** questions completed vs. the April 1 goal, pace, and
  questions/day needed.
- **PEAC:** placeholder. No data source yet.

## Data

- RDM **prod** (`RDM_TOKEN`, `REDCAP_URL`): read-only.
- Amion schedule, duty-hour log and conference log come through
  `amiontools::build_resident_activity()`.
- One pull is shared by every session in the R process and reused for
  `DATA_MAX_AGE_HOURS` (4 h). A fresh pull takes about 60 s. **Refresh
  data** forces a new pull.
- `ass_plus` / `ass_delta` are dropped right after the pull and never held
  in memory.

## Code layout

| File | Purpose |
|---|---|
| `app.R` | Page shell, auth → data load → tabs |
| `R/load_data.R` | All REDCap / Amion reads, roster rule, shared cache |
| `R/constants.R`, `R/metrics_{duty,conference,evals,knowledge}.R` | **Copied from `imslu.program.dash`** (see header). Same thresholds, same numbers as the weekly report |
| `R/constants_leadership.R` | App-only settings (cache age, access-code env var) |
| `R/metrics_leadership.R` | App-only views (overview table, schedule blocks, per-resident weekly series) |
| `R/mod_auth.R`, `R/mod_summary.R`, `R/mod_resident.R` | Shiny modules |
| `R/ui_helpers.R`, `www/app.css` | KPI tiles, formatting, layout CSS |

## Environment variables

| Variable | Where |
|---|---|
| `RDM_TOKEN`, `REDCAP_URL` | `~/.Renviron` locally; Connect env vars when deployed |
| `LEADERSHIP_ACCESS_CODE` | Same. Shared by chiefs + PD |
| `LD_DATA_RDS` | **Local dev only.** Path to a saved `load_dashboard_data()` result, so UI work skips the 60 s pull |

## Dependencies

shiny, bslib, dplyr, tidyr, purrr, lubridate, plotly, DT, REDCapR, plus the
GitHub packages `amiontools`, `iteviz` and `roundsui`. All are already used
elsewhere in the ecosystem.
