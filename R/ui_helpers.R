# ui_helpers.R — small display helpers shared by the modules.

fmt_pct <- function(x, digits = 0) ifelse(is.na(x), "—", paste0(round(100 * x, digits), "%"))
fmt_num <- function(x, digits = 0) ifelse(is.na(x), "—", format(round(x, digits), big.mark = ",", nsmall = digits))
fmt_date <- function(x) ifelse(is.na(x), "—", sub(" 0", " ", format(as.Date(x), "%b %d, %Y")))
fmt_md <- function(x) sub(" 0", " ", format(as.Date(x), "%b %d"))

# A KPI tile: big number, label, optional sub-line, optional tone
# ("danger" / "warning" / "success") for the number's color.
kpi_tile <- function(value, label, sub = NULL, tone = NULL) {
  div(class = paste("ld-kpi", if (!is.null(tone)) paste0("ld-kpi--", tone)),
      div(class = "ld-kpi__value", value),
      div(class = "ld-kpi__label", label),
      if (!is.null(sub)) div(class = "ld-kpi__sub", sub))
}

kpi_row <- function(...) div(class = "ld-kpi-row", ...)

# Status of a pace / rate against thresholds, as a tone.
tone_for <- function(red, amber) {
  if (isTRUE(red)) "danger" else if (isTRUE(amber)) "warning" else NULL
}

# Theme-aware empty plotly with a message.
empty_plot <- function(msg) {
  plotly::plot_ly(type = "scatter", mode = "markers") |>
    roundsui::roundsui_plotly_layout() |>
    plotly::layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE),
                   annotations = list(list(text = msg, showarrow = FALSE,
                                           xref = "paper", yref = "paper", x = .5, y = .5))) |>
    plotly::config(displayModeBar = FALSE)
}

COL <- roundsui::roundsui_colors()
