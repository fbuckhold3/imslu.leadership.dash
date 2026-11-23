# mod_auth.R — access-code gate (ecosystem pattern; same idea as the CCC
# dashboard's CCC_ACCESS_CODE). One shared code for chiefs + PD for now.

mod_auth_ui <- function(id) {
  ns <- NS(id)
  div(
    class = "ld-auth",
    roundsui::roundsui_card(
      title = "Program Leadership Dashboard",
      p(class = "ld-muted", "Enter the leadership access code to continue."),
      passwordInput(ns("code"), NULL, placeholder = "Access code", width = "100%"),
      actionButton(ns("submit"), "Enter", class = "btn-primary w-100"),
      uiOutput(ns("error"))
    )
  )
}

# Returns a reactive TRUE once the right code is entered.
mod_auth_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    authed <- reactiveVal(FALSE)
    err <- reactiveVal(NULL)

    observeEvent(input$submit, {
      expected <- Sys.getenv(ACCESS_CODE_ENV)
      if (!nzchar(expected)) {
        err(paste0("Access code not configured. Set ", ACCESS_CODE_ENV, "."))
        return()
      }
      if (isTRUE(nzchar(input$code)) && identical(trimws(input$code), expected)) {
        err(NULL)
        authed(TRUE)
      } else {
        err("That code didn't match.")
      }
    })

    output$error <- renderUI({
      req(err())
      roundsui::roundsui_inline_error(err())
    })

    authed
  })
}
