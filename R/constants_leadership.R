# constants_leadership.R — settings that belong to this app only.
# (constants.R is a copy of program.dash's; don't add app-only values there.)

# How long the shared data pull is reused before the next visitor triggers
# a fresh one. Duty entries and conference logs change through the day.
DATA_MAX_AGE_HOURS <- 4

# Access-code environment variable (set on Connect, and in ~/.Renviron
# for local runs). Chiefs and the PD share one code for now (2026-11-23).
ACCESS_CODE_ENV <- "LEADERSHIP_ACCESS_CODE"

# Copied from program.dash R/load_program_data.R (ITE fields in test_data).
ITE_AREAS <- c("cards", "endo", "gi", "gim", "geri", "hemonc", "id",
               "nephro", "neuro", "pulm_ccm", "rheum", "hvc")
ITE_AREA_LABELS <- c(cards = "Cardiology", endo = "Endocrinology", gi = "GI",
                     gim = "General IM", geri = "Geriatrics", hemonc = "Heme/Onc",
                     id = "ID", nephro = "Nephrology", neuro = "Neurology",
                     pulm_ccm = "Pulm/CCM", rheum = "Rheumatology", hvc = "High-value care")
