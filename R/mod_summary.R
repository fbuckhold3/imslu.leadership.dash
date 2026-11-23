# mod_summary.R — program-level view: KPI tiles, class breakdowns, and the
# all-residents table (click a row to open that resident).

mod_summary_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("kpis")),
    layout_columns(
      col_widths = c(6, 6),
      roundsui::roundsui_card(
        title = "Evaluation counts by class (AY to date)",
        DT::DTOutput(ns("eval_counts"))
      ),
      roundsui::roundsui_card(
        title = "Conference attendance by class (AY to date)",
        plotly::plotlyOutput(ns("conf_class"), height = "260px")
      )
    ),
    layout_columns(
      col_widths = c(6, 6),
      roundsui::roundsui_card(
        title = "Average weekly duty hours by class",
        p(class = "ld-muted", "AY to date; clean weeks only (no all-vacation or needs-entry weeks)."),
        DT::DTOutput(ns("duty_class"))
      ),
      roundsui::roundsui_card(
        title = "MKSAP pace by class",
        uiOutput(ns("mksap_note")),
        plotly::plotlyOutput(ns("mksap_class"), height = "240px")
      )
    ),
    roundsui::roundsui_card(
      title = "All residents",
      p(class = "ld-muted", "Click a row to open that resident."),
      DT::DTOutput(ns("residents"))
    )
  )
}

# d: reactive returning the loaded data list. Returns a reactive with
# list(rid, at) for the row clicked (NULL until a click).
mod_summary_server <- function(id, d) {
  moduleServer(id, function(input, output, session) {

    overview <- reactive({
      req(d())
      resident_overview(d())
    })

    output$kpis <- renderUI({
      req(d())
      k <- program_kpis(d())
      cls <- paste(sprintf("%s %d", names(k$by_class), as.integer(k$by_class)), collapse = " · ")
      kpi_row(
        kpi_tile(k$residents, "Residents", cls),
        kpi_tile(k$over_80, "4-wk avg > 80 h", "duty hours", tone = if (k$over_80 > 0) "danger"),
        kpi_tile(fmt_num(k$open_duty_days), "Open duty days",
                 sprintf("last %d days, all residents", THRESH$duty_entry_window)),
        kpi_tile(fmt_num(k$evals_rec_ay), "Evals received (AY)",
                 sprintf("%s in last %d days", fmt_num(k$evals_rec_28), THRESH$evals_window)),
        kpi_tile(fmt_num(k$fac_done_ay), "Faculty evals done (AY)",
                 sprintf("%s peer reviews done", fmt_num(k$peer_done_ay))),
        kpi_tile(fmt_pct(k$conf_rate_ay), "Conference attendance (AY)",
                 sprintf("%s Afternoon School days logged", fmt_num(k$afternoon_days))),
        kpi_tile(sprintf("%d / %d", k$mksap_behind, k$mksap_with_goal), "Behind MKSAP pace",
                 sprintf("export %s", fmt_date(k$mksap_as_of)),
                 tone = if (k$mksap_behind > 0) "warning")
      )
    })

    output$eval_counts <- DT::renderDT({
      req(d())
      x <- eval_counts_by_class(d()) |>
        rename(Class = class, Residents = residents, `Received (AY)` = received_ay,
               `Received (28 d)` = received_28d, `Faculty evals done` = faculty_evals_done,
               `Peer reviews done` = peer_reviews_done)
      DT::datatable(x, rownames = FALSE, class = "compact stripe",
                    options = list(dom = "t", ordering = FALSE, paging = FALSE))
    })

    output$conf_class <- plotly::renderPlotly({
      req(d())
      x <- conf_class_rates(d(), d()$ay_start, d()$week$end)
      plotly::plot_ly(x, x = ~class, y = ~rate_sluh, type = "bar", name = "SLUH",
                      marker = list(color = COL$accent),
                      hovertemplate = "%{x}: %{y:.0%} (SLUH)<extra></extra>") |>
        plotly::add_trace(y = ~rate_va, name = "VA", marker = list(color = COL$ink_faint),
                          hovertemplate = "%{x}: %{y:.0%} (VA)<extra></extra>") |>
        roundsui::roundsui_plotly_layout(barmode = "group",
                                         yaxis = list(tickformat = ".0%", range = c(0, 1), title = ""),
                                         xaxis = list(title = "")) |>
        plotly::config(displayModeBar = FALSE)
    })

    output$duty_class <- DT::renderDT({
      req(d())
      x <- duty_class_averages(d()) |>
        transmute(Class = class, Residents = residents,
                  `Avg weekly hours` = round(avg_weekly_hours, 1),
                  `Weeks over 80 h` = weeks_over_80)
      DT::datatable(x, rownames = FALSE, class = "compact stripe",
                    options = list(dom = "t", ordering = FALSE, paging = FALSE))
    })

    output$mksap_note <- renderUI({
      req(d())
      p(class = "ld-muted",
        sprintf("Pace = questions done ÷ expected by the export date (linear to the April 1 goal). Latest export %s.",
                fmt_date(mksap_as_of(d()))))
    })

    output$mksap_class <- plotly::renderPlotly({
      req(d())
      x <- d()$status$mksap |> filter(!prelim, !is.na(pace))
      if (!nrow(x)) return(empty_plot("No MKSAP exports yet"))
      plotly::plot_ly(x, x = ~class, y = ~pace, type = "box", boxpoints = "all",
                      jitter = 0.4, pointpos = 0, text = ~resident,
                      marker = list(color = COL$accent, size = 6),
                      line = list(color = COL$ink_muted), fillcolor = COL$accent_tint,
                      hovertemplate = "%{text}: %{y:.0%}<extra></extra>") |>
        roundsui::roundsui_plotly_layout(
          yaxis = list(tickformat = ".0%", title = "", rangemode = "tozero"),
          xaxis = list(title = ""), showlegend = FALSE,
          shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 1, y1 = 1,
                             line = list(color = COL$success, dash = "dash")))) |>
        plotly::config(displayModeBar = FALSE)
    })

    table_data <- reactive({
      overview() |>
        transmute(
          record_id,
          Resident = resident, Class = class,
          `Current rotation` = current_rotation,
          `4-wk avg h` = round(avg4_hours, 1),
          `Open duty days` = open_days_window,
          `Evals rec'd (AY)` = rec_ay,
          `Fac evals done` = fac_ay,
          `Peer reviews done` = peer_done_ay,
          `Conf % (AY)` = round(100 * ay_rate),
          `Aft. School days` = ay_afternoon_school,
          `MKSAP done` = mk_n_complete,
          `MKSAP pace %` = ifelse(prelim, NA, round(100 * pace))
        )
    })

    output$residents <- DT::renderDT({
      x <- table_data()
      DT::datatable(
        x, rownames = FALSE, selection = "single", class = "compact stripe hover",
        filter = "top",
        options = list(pageLength = 25, scrollX = TRUE, order = list(list(2, "asc"), list(1, "asc")),
                       columnDefs = list(list(visible = FALSE, targets = 0)))
      ) |>
        DT::formatStyle("4-wk avg h", color = DT::styleInterval(
          c(THRESH$hours_amber, THRESH$hours_red), c(COL$ink, COL$warning, COL$danger))) |>
        DT::formatStyle("MKSAP pace %", color = DT::styleInterval(
          c(100 * THRESH$mksap_amber - 0.5, 99.5), c(COL$danger, COL$warning, COL$ink)))
    })

    selected <- reactiveVal(NULL)
    observeEvent(input$residents_rows_selected, {
      i <- input$residents_rows_selected
      req(length(i) == 1)
      # Timestamp so re-clicking the same resident still fires.
      selected(list(rid = table_data()$record_id[i], at = Sys.time()))
    })
    selected
  })
}
