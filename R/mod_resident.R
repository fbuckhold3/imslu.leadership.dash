# mod_resident.R — one resident: header + tabs for duty hours, schedule,
# evaluations (counts only), conference, MKSAP, PEAC (placeholder).

mod_resident_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "ld-picker",
        selectizeInput(ns("rid"), "Resident", choices = NULL, width = "360px",
                       options = list(placeholder = "Search by name"))),
    uiOutput(ns("header")),
    navset_card_underline(
      id = ns("tab"),
      nav_panel("Duty hours",
                uiOutput(ns("duty_kpis")),
                plotly::plotlyOutput(ns("duty_chart"), height = "380px"),
                p(class = "ld-muted",
                  "Solid = resident-verified hours; light = still the Amion estimate; ",
                  "grey = anticipated (future weeks, from the Amion schedule). ",
                  "Days with no Amion default (elective, ED, other) count 0 h until the resident logs them.")),
      nav_panel("Schedule",
                uiOutput(ns("sched_kpis")),
                plotly::plotlyOutput(ns("sched_chart"), height = "200px"),
                DT::DTOutput(ns("sched_table"))),
      nav_panel("Evaluations",
                uiOutput(ns("eval_kpis")),
                layout_columns(
                  col_widths = c(6, 6),
                  div(h4(class = "ld-h4", "Assessments received, by type"),
                      DT::DTOutput(ns("eval_types"))),
                  div(h4(class = "ld-h4", "Assessments received, by month"),
                      plotly::plotlyOutput(ns("eval_monthly"), height = "260px"))
                )),
      nav_panel("Conference",
                uiOutput(ns("conf_kpis")),
                plotly::plotlyOutput(ns("conf_chart"), height = "300px")),
      nav_panel("MKSAP",
                uiOutput(ns("mksap_kpis")),
                plotly::plotlyOutput(ns("mksap_chart"), height = "300px")),
      nav_panel("PEAC",
                roundsui::roundsui_empty_state(
                  "PEAC module completion isn't tracked yet. This tab will fill in once the data source is decided.",
                  icon = icon("hourglass-half")))
    )
  )
}

# d: reactive data list. goto: reactive list(rid, at) from the summary
# table; selecting there switches this module to that resident.
mod_resident_server <- function(id, d, goto) {
  moduleServer(id, function(input, output, session) {

    observeEvent(d(), {
      r <- d()$roster
      ch <- setNames(r$record_id, sprintf("%s (%s)", r$resident, r$class))
      updateSelectizeInput(session, "rid", choices = ch,
                           selected = isolate(input$rid) %||% character(0), server = TRUE)
    })

    observeEvent(goto(), {
      req(goto()$rid)
      updateSelectizeInput(session, "rid", selected = goto()$rid)
    })

    rid <- reactive({
      req(d(), input$rid)
      validate(need(input$rid %in% d()$roster$record_id, "Pick a resident."))
      input$rid
    })
    row <- function(tbl) tbl[tbl$record_id == rid(), , drop = FALSE]

    output$header <- renderUI({
      req(d(), input$rid)
      r <- row(d()$roster)
      req(nrow(r) == 1)
      rot <- d()$activity |> filter(record_id == rid(), Date == d()$as_of) |> pull(Rotation)
      roundsui::roundsui_resident_panel(
        resident_name = r$resident,
        level = paste0(as.character(r$class), if (isTRUE(r$prelim)) " (prelim)" else ""),
        period = if (length(rot) && !is.na(rot[1])) paste("Today:", rot[1]) else NULL)
    })

    # ── Duty hours ──────────────────────────────────────────────────────
    output$duty_kpis <- renderUI({
      s  <- row(d()$status$duty)
      wk <- resident_duty_weeks(d(), rid())
      past <- if (!is.null(wk)) wk |> filter(!future, week_start <= d()$week$start) else NULL
      fut  <- if (!is.null(wk)) wk |> filter(future) else NULL
      kpi_row(
        kpi_tile(fmt_num(s$avg4_hours, 1), "4-wk avg hours/week",
                 sprintf("%s days/week", fmt_num(s$avg4_days, 1)),
                 tone = tone_for(s$avg4_hours > THRESH$hours_red, s$avg4_hours > THRESH$hours_amber)),
        kpi_tile(fmt_num(s$week_hours, 1), "Last full week",
                 sprintf("week of %s", fmt_md(d()$week$start))),
        kpi_tile(fmt_pct(s$pct_estimated_4wk), "Of 4-wk hours still estimated",
                 "not yet confirmed by resident"),
        kpi_tile(s$open_days_window, "Open duty days",
                 sprintf("last %d days (%d need entry)", THRESH$duty_entry_window, s$needs_entry_window),
                 tone = tone_for(s$open_days_window >= THRESH$duty_entry_red, s$open_days_window >= 1)),
        kpi_tile(if (!is.null(fut) && nrow(fut)) fmt_num(max(fut$total), 0) else "—",
                 "Heaviest anticipated week",
                 if (!is.null(fut) && nrow(fut)) sprintf("week of %s", fmt_md(fut$week_start[which.max(fut$total)])) else NULL,
                 tone = if (!is.null(fut) && nrow(fut) && max(fut$total) > THRESH$hours_red) "danger")
      )
    })

    output$duty_chart <- plotly::renderPlotly({
      wk <- resident_duty_weeks(d(), rid())
      if (is.null(wk) || !nrow(wk)) return(empty_plot("No Amion schedule for this resident"))
      pastw <- !wk$future
      plotly::plot_ly(wk, x = ~week_start) |>
        plotly::add_bars(y = ~ifelse(pastw, verified, 0), name = "Verified",
                         marker = list(color = COL$accent),
                         hovertemplate = "Week of %{x|%b %d}: %{y:.0f} h verified<extra></extra>") |>
        plotly::add_bars(y = ~ifelse(pastw, estimated, 0), name = "Amion estimate",
                         marker = list(color = "#A9B8EE"),
                         hovertemplate = "Week of %{x|%b %d}: %{y:.0f} h estimated<extra></extra>") |>
        plotly::add_bars(y = ~ifelse(pastw, 0, total), name = "Anticipated",
                         marker = list(color = COL$border_strong),
                         hovertemplate = "Week of %{x|%b %d}: %{y:.0f} h anticipated<extra></extra>") |>
        plotly::add_lines(y = ~ifelse(pastw, avg4, NA), name = "4-wk avg",
                          line = list(color = COL$warning, width = 2),
                          hovertemplate = "4-wk avg: %{y:.1f} h<extra></extra>") |>
        roundsui::roundsui_plotly_layout(
          barmode = "stack", hovermode = "x unified",
          yaxis = list(title = "Hours / week", rangemode = "tozero"),
          xaxis = list(title = ""),
          legend = list(orientation = "h", y = -0.15),
          shapes = list(
            list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 80, y1 = 80,
                 line = list(color = COL$danger, dash = "dash")),
            list(type = "line", yref = "paper", y0 = 0, y1 = 1,
                 x0 = d()$as_of, x1 = d()$as_of, line = list(color = COL$ink_faint, dash = "dot")))) |>
        plotly::config(displayModeBar = FALSE)
    })

    # ── Schedule ────────────────────────────────────────────────────────
    blocks <- reactive(resident_schedule_blocks(d(), rid()))

    output$sched_kpis <- renderUI({
      b <- blocks()
      v <- resident_vacation(d(), rid())
      cur <- if (!is.null(b)) b |> filter(when == "Current") else NULL
      nxt <- if (!is.null(b)) b |> filter(when == "Upcoming") |> slice_head(n = 1) else NULL
      kpi_row(
        kpi_tile(if (length(cur$category)) cur$category[1] else "—", "Current block",
                 if (length(cur$category)) sprintf("%s – %s", fmt_md(cur$start[1]), fmt_md(cur$end[1]))),
        kpi_tile(if (length(nxt$category)) nxt$category[1] else "—", "Next block",
                 if (length(nxt$category)) sprintf("starts %s", fmt_md(nxt$start[1]))),
        kpi_tile(v[["taken"]], "Vacation days taken (AY)", sprintf("%d upcoming", v[["upcoming"]]))
      )
    })

    output$sched_chart <- plotly::renderPlotly({
      b <- blocks()
      if (is.null(b) || !nrow(b)) return(empty_plot("No Amion schedule for this resident"))
      cats <- unique(b$category)
      # 15 distinct hues: Amion has ~13 rotation categories, so none repeat.
      pal <- rep(c(COL$accent, "#0F8B8D", "#C2760A", "#7A4CC2", "#D33A2C", "#16A34A",
                   "#5B6472", "#B5478A", "#2E9FD6", "#8A6D1F", "#E0A526", "#1F4E79",
                   "#8BC34A", "#A0522D", "#CBD0DB"), length.out = length(cats))
      b$color <- pal[match(b$category, cats)]
      p <- plotly::plot_ly()
      for (i in seq_len(nrow(b))) {
        p <- plotly::add_segments(p, x = b$start[i], xend = b$end[i] + 1, y = 0, yend = 0,
                                  line = list(color = b$color[i], width = 40),
                                  name = b$category[i], legendgroup = b$category[i],
                                  showlegend = !duplicated(b$category)[i],
                                  hovertemplate = sprintf("%s<br>%s – %s<extra></extra>",
                                                          b$rotations[i], format(b$start[i], "%b %d"),
                                                          format(b$end[i], "%b %d")))
      }
      p |>
        roundsui::roundsui_plotly_layout(
          yaxis = list(visible = FALSE, range = c(-1, 1)), xaxis = list(title = ""),
          legend = list(orientation = "h", y = -0.4),
          shapes = list(list(type = "line", yref = "paper", y0 = 0, y1 = 1,
                             x0 = d()$as_of, x1 = d()$as_of, line = list(color = COL$ink, dash = "dot")))) |>
        plotly::config(displayModeBar = FALSE)
    })

    output$sched_table <- DT::renderDT({
      b <- blocks()
      req(b)
      x <- b |> transmute(When = when, Start = start, End = end, Category = category,
                          `Amion assignments` = rotations, Days = days)
      DT::datatable(x, rownames = FALSE, class = "compact stripe",
                    options = list(dom = "tp", pageLength = 15, ordering = FALSE)) |>
        DT::formatStyle("When", target = "row",
                        fontWeight = DT::styleEqual("Current", "600"),
                        color = DT::styleEqual("Past", COL$ink_faint))
    })

    # ── Evaluations (counts only) ───────────────────────────────────────
    ev <- reactive(resident_evals_by_type(d(), rid()))

    output$eval_kpis <- renderUI({
      s <- row(d()$status$evals)
      kpi_row(
        kpi_tile(s$rec_ay, "Assessments received (AY)",
                 sprintf("%d in last %d days", s$rec_window, THRESH$evals_window),
                 tone = tone_for(s$rec_window == 0 && s$working_days > 0, s$rec_ay < s$rec_ay_class_q25)),
        kpi_tile(fmt_num(ev()$evaluators), "Distinct evaluators", "count only"),
        kpi_tile(s$fac_ay, "Faculty evals completed (AY)",
                 sprintf("%d in last %d days", s$fac_window, THRESH$evals_window)),
        kpi_tile(s$peer_done_ay, "Peer reviews completed (AY)",
                 sprintf("%d peer reviews received", s$peer_received_ay))
      )
    })

    output$eval_types <- DT::renderDT({
      x <- ev()$by_type
      DT::datatable(x, rownames = FALSE, class = "compact stripe",
                    options = list(dom = "t", paging = FALSE, ordering = FALSE))
    })

    output$eval_monthly <- plotly::renderPlotly({
      m <- ev()$monthly
      if (!nrow(m)) return(empty_plot("No assessments this academic year"))
      plotly::plot_ly(m, x = ~month, y = ~n, type = "bar", marker = list(color = COL$accent),
                      hovertemplate = "%{x|%b %Y}: %{y}<extra></extra>") |>
        roundsui::roundsui_plotly_layout(yaxis = list(title = "", dtick = 1, rangemode = "tozero"),
                                         xaxis = list(title = "", tickformat = "%b")) |>
        plotly::config(displayModeBar = FALSE)
    })

    # ── Conference ──────────────────────────────────────────────────────
    output$conf_kpis <- renderUI({
      s <- row(d()$status$conf)
      kpi_row(
        kpi_tile(fmt_pct(s$ay_rate), "Noon conference (AY)",
                 sprintf("%s of %s expected days", fmt_num(s$ay_attended), fmt_num(s$ay_expected))),
        kpi_tile(fmt_pct(s$ay_rate_sluh), "SLUH-expected days",
                 sprintf("VA: %s of %s days", fmt_pct(s$ay_rate_va), fmt_num(s$ay_expected_va))),
        kpi_tile(fmt_pct(s$w4_rate), "Last 4 weeks",
                 sprintf("%s of %s", fmt_num(s$w4_attended), fmt_num(s$w4_expected))),
        kpi_tile(fmt_num(s$ay_afternoon_school), "Afternoon School days (AY)", "logged in REDCap"),
        kpi_tile(if (is.na(s$last_log)) "—" else fmt_md(s$last_log), "Last logged",
                 if (!is.na(s$days_since_log)) sprintf("%d days ago", s$days_since_log))
      )
    })

    output$conf_chart <- plotly::renderPlotly({
      w <- resident_conf_weeks(d(), rid())
      if (!nrow(w)) return(empty_plot("No conference data"))
      plotly::plot_ly(w, x = ~week_start) |>
        plotly::add_bars(y = ~attended, name = "Attended", marker = list(color = COL$accent),
                         hovertemplate = "%{y} attended<extra></extra>") |>
        plotly::add_bars(y = ~pmax(expected - attended, 0), name = "Expected, not logged",
                         marker = list(color = COL$border_strong),
                         hovertemplate = "%{y} missed<extra></extra>") |>
        plotly::add_markers(y = ~ifelse(afternoon_school > 0, afternoon_school, NA),
                            name = "Afternoon School", marker = list(color = "#0F8B8D", size = 9),
                            hovertemplate = "%{y} Afternoon School<extra></extra>") |>
        roundsui::roundsui_plotly_layout(barmode = "stack", hovermode = "x unified",
                                         yaxis = list(title = "Days", rangemode = "tozero"),
                                         xaxis = list(title = ""),
                                         legend = list(orientation = "h", y = -0.2)) |>
        plotly::config(displayModeBar = FALSE)
    })

    # ── MKSAP ───────────────────────────────────────────────────────────
    output$mksap_kpis <- renderUI({
      s <- row(d()$status$mksap)
      if (isTRUE(s$prelim)) return(roundsui::roundsui_empty_state("Prelims have no MKSAP goal.", compact = TRUE))
      if (is.na(s$mk_n_complete)) return(roundsui::roundsui_empty_state("No MKSAP export for this resident yet.", compact = TRUE))
      kpi_row(
        kpi_tile(fmt_num(s$mk_n_complete), "Questions completed",
                 sprintf("export %s%s", fmt_date(s$mk_export_date),
                         if (!is.na(s$since_prev)) sprintf(" · +%s since prior", fmt_num(s$since_prev)) else "")),
        kpi_tile(s$goal_label, "Goal (Apr 1)", sprintf("expected by export: %s", fmt_num(s$expected))),
        kpi_tile(fmt_pct(s$pace), "Pace",
                 tone = tone_for(!is.na(s$pace) && s$pace < THRESH$mksap_amber, !is.na(s$pace) && s$pace < 1)),
        kpi_tile(fmt_num(s$per_day_needed, 1), "Questions/day needed", "to reach goal by Apr 1"),
        kpi_tile(fmt_pct(as.numeric(s$mk_pct_correct) / 100), "% correct")
      )
    })

    output$mksap_chart <- plotly::renderPlotly({
      h <- resident_mksap_history(d(), rid())
      s <- row(d()$status$mksap)
      if (!nrow(h)) return(empty_plot("No MKSAP exports yet"))
      due <- as.Date(sprintf("%d-%s", d()$ay + 1L, MKSAP_DUE_MMDD))
      p <- plotly::plot_ly(h, x = ~mk_export_date, y = ~mk_n_complete, type = "scatter",
                           mode = "lines+markers", name = "Completed",
                           line = list(color = COL$accent), marker = list(color = COL$accent),
                           hovertemplate = "%{x|%b %d}: %{y:,} done<extra></extra>")
      if (!is.na(s$goal)) {
        p <- plotly::add_lines(p, x = c(d()$ay_start, due), y = c(0, s$goal), name = "Goal pace",
                               line = list(color = COL$success, dash = "dash"),
                               inherit = FALSE, hoverinfo = "skip")
      }
      p |>
        roundsui::roundsui_plotly_layout(yaxis = list(title = "Questions", rangemode = "tozero"),
                                         xaxis = list(title = "", range = c(d()$ay_start, due)),
                                         legend = list(orientation = "h", y = -0.2)) |>
        plotly::config(displayModeBar = FALSE)
    })
  })
}
