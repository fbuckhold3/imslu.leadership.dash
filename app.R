# imslu.leadership.dash — program leadership dashboard (chiefs + PD).
#
# Read-only view of RDM prod + Amion: program summary and a per-resident
# drill-down (duty hours, schedule, evaluation counts, conference, MKSAP,
# PEAC placeholder). Access-code gated.

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(dplyr)
})

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f, local = TRUE)

ui <- roundsui::roundsui_page(
  title = "IMSLU Leadership Dashboard",
  tags$head(tags$link(rel = "stylesheet", href = "app.css")),
  uiOutput("body")
)

server <- function(input, output, session) {
  authed <- mod_auth_server("auth")

  data_rv <- reactiveVal(NULL)
  load_error <- reactiveVal(NULL)

  load_data <- function(force = FALSE) {
    tryCatch({
      withProgress(message = "Loading REDCap + Amion data (about a minute on a fresh load)…", {
        data_rv(get_dashboard_data(force = force))
      })
      load_error(NULL)
    }, error = function(e) {
      load_error(conditionMessage(e))
    })
  }

  observeEvent(authed(), {
    req(authed())
    load_data()
  })
  observeEvent(input$refresh, load_data(force = TRUE))

  d <- reactive({
    validate(need(is.null(load_error()), paste("Data load failed:", load_error())))
    req(data_rv())
  })

  output$body <- renderUI({
    if (!isTRUE(authed())) return(mod_auth_ui("auth"))
    tagList(
      div(class = "ld-topbar",
          div(h1(class = "ld-title", "Program Leadership Dashboard"),
              uiOutput("as_of", inline = TRUE)),
          actionButton("refresh", "Refresh data", icon = icon("rotate"), class = "btn-sm btn-outline-secondary")),
      uiOutput("load_error_ui"),
      navset_tab(
        id = "main_tab",
        nav_panel("Program summary", value = "summary", mod_summary_ui("summary")),
        nav_panel("Resident", value = "resident", mod_resident_ui("resident"))
      )
    )
  })

  output$as_of <- renderUI({
    x <- data_rv()
    req(x)
    div(class = "ld-muted",
        sprintf("Data pulled %s · \"last week\" = %s – %s · academic year %d–%d",
                gsub(" 0", " ", format(x$fetched_at, "%b %d, %I:%M %p")),
                fmt_md(x$week$start), fmt_md(x$week$end),
                x$ay, x$ay + 1L))
  })

  output$load_error_ui <- renderUI({
    req(load_error())
    roundsui::roundsui_inline_error(paste("Data load failed:", load_error()))
  })

  goto <- mod_summary_server("summary", d)
  mod_resident_server("resident", d, goto)

  observeEvent(goto(), {
    nav_select("main_tab", "resident")
  })
}

shinyApp(ui, server)
