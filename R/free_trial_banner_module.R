
#' free trial banner
#'
#' A banner to show to users that are in their free trial to let them know they
#' will need to enable billing to continue using the app after their free trial
#' has expired.
#'
#' @param id the Shiny module id
#'
#' @importFrom shiny NS actionButton
#' @importFrom shinyjs hidden
#' @importFrom shinyFeedback loadingButton
#' @importFrom htmltools tagList tags
#'
#' @export
#'
free_trial_banner_module_ui <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    shinyjs::useShinyjs(),
    tags$style(paste0("
      .", ns(''),  "color_white {
        color: #FFF
      }
    ")),
    shinyjs::hidden(
      tags$div(
        id = ns("banner"),
        style = "width: 100%; z-index: 99999999; background-color: #000; position: fixed; bottom: 0; left: 0;",
        tags$div(
          shiny::actionButton(
            ns("close_banner"),
            NULL,
            icon = icon("times", class = paste("fa-2x", ns("color_white"))),
            class = "pull-right",
            style = "background: rgba(0, 0, 0, 1.0); border: none; display: inline;"
          )
        ),
        tags$div(
          style = "text-align: center; padding-bottom: 8px;",
          tags$h2(
            style = "color: white;",
            "You have ",
            tags$b(
              shiny::textOutput(ns("trial_days_remaining_out"), inline = TRUE)
            ),
            " days remaining in your free trial!"
          ),
          shinyFeedback::loadingButton(
            ns("enable_billing"),
            label = "Enable Billing",
            style = "color: #FFFFFF; font-size: 20px; display: inline; margin-top: -8px;",
            loadingLabel = "Redirecting...",
            loadingStyle = "color: #FFFFFF; background-color: grey; border-color: grey; font-size: 20px; display: inline; margin-top: -8px;"
          ),
          tags$h2(
            style = "color: white; display: inline; margin-left: 5px;",
            " to continue usage after your free trial ends."
          )
        )
      )
    )
  )

}

#' Free trial banner module server logic
#'
#' @param input the Shiny input
#' @param output the Shiny output
#' @param session the Shiny session
#'
#' @importFrom shiny observeEvent reactive renderText outputOptions
#' @importFrom shinyjs hideElement
#'
#' @export
#'
free_trial_banner_module <- function(input, output, session) {

  shiny::observeEvent(session$userData$subscription(), {
    hold_sub <- session$userData$subscription()
    hold_user <- session$userData$user()


    if (
      isFALSE(hold_sub$free_user) &&
      # show the billing banner if the user is in their free trial and they have not enebaled billing
      (isFALSE(hold_sub$is_billing_enabled) && hold_sub$trial_days_remaining > 0)) {

      shinyjs::delay(3000, shinyjs::showElement("banner", anim = TRUE))
    }
  })



  trial_days_remaining <- shiny::reactive({
    hold_user <- session$userData$user()
    hold_sub <- session$userData$subscription()
    req(isFALSE(hold_sub$free_user))


    ceiling(hold_sub$trial_days_remaining)
  })

  output$trial_days_remaining_out <- shiny::renderText({
    trial_days_remaining()
  })

  shiny::outputOptions(output, "trial_days_remaining_out", suspendWhenHidden = FALSE, priority = 100)

  shiny::observeEvent(input$enable_billing, {

    shiny::updateQueryString(
      queryString = "?page=account",
      session = session,
      mode = "replace"
    )
    session$reload()
  })

  # Close banner
  shiny::observeEvent(input$close_banner, {
   shinyjs::hideElement("banner", anim = TRUE)
  })
}
