
#' The payments Shiny app
#'
#' The Shiny module UI for the Stripe payments Shiny app.  This app can be easily added
#' to your Shiny apps that use polished.
#'
#' @param app_name the app name to display to users.
#'
#' @export
#'
#' @importFrom shiny actionLink icon
#' @importFrom shinydashboard dashboardSidebar dashboardBody sidebarMenu menuItem tabItems dashboardHeader dashboardPage
#' @importFrom htmltools HTML tags tagList
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyFeedback useShinyFeedback
#' @importFrom polished profile_module_ui
#' @importFrom waiter use_waiter waiter_show_on_load spin_fading_circles
#'
#' @return the UI for the "Admin Panel"
#'
payments_app_ui <- function(
  app_name = getOption("polished")$app_name
) {

  stripe_key_public <- getOption("pp")$keys$public

  head <- shinydashboard::dashboardHeader(
    title = "Payments",
    left_menu = tags$li(
      class = "dropdown",
      shiny::actionLink(
        "go_to_shiny_app",
        paste0("Go to ", app_name),
        style = "margin-left: -15px; color: #1a8dc7; font-size: 18px;"
      )
    ),
    polished::profile_module_ui("profile")
  )


  sidebar <- shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      id = "sidebar_menu",
      shinydashboard::menuItem(
        "Billing",
        tabName = "billing",
        icon = icon("credit-card")
      )
    )
  )


  body <- shinydashboard::dashboardBody(
    tags$head(
      tags$link(rel = "shortcut icon", href = "polishedpayments/images/polished_logo_transparent.png"),
      tags$script(src = "https://js.stripe.com/v3"),
      tags$script(paste0("var stripe = Stripe('", stripe_key_public, "');")),
      tags$script(src = "polishedpayments/js/polishedpayments.js?version=3"),
      shinyjs::useShinyjs(),
      shinyFeedback::useShinyFeedback()
    ),
    waiter::use_waiter(),
    waiter::waiter_show_on_load(html = waiter::spin_fading_circles()),

    shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "billing",
        billing_module_ui("billing")
      )
    )
  )

  shinydashboard::dashboardPage(
    head,
    sidebar,
    body,
    skin = "black"
  )
}


#' The server logic for the "Admin Panel" 'shinydashboard'
#'
#' The 'shiny' module server logic for the Admin Panel.
#'
#' @param input the Shiny server input
#' @param output the Shiny server output
#' @param session the Shiny server session
#'
#' @export
#'
#' @importFrom httr GET POST status_code content
#' @importFrom shiny callModule observeEvent reactiveVal reactive req moduleServer
#' @importFrom polished profile_module remove_query_string
#' @importFrom shinyFeedback showToast
#' @importFrom shinyWidgets sendSweetAlert
#' @importFrom tibble as_tibble
#'
#'
payments_app_server <- function(input, output, session) {

  shiny::callModule(
    polished::profile_module,
    "profile"
  )

  shiny::observeEvent(input$go_to_shiny_app, {

    hold_stripe <- session$userData$stripe()
    hold_user <- session$userData$user()

    if (length(intersect(hold_user$roles, getOption("pp")$free_roles)) > 0 || is.null(getOption("pp")$prices)) {
      # User has a free role (or `subscription_prices = NULL`), so go to the Shiny app
      polished::remove_query_string()
      session$reload()

    } else if (is.na(hold_stripe$subscription[1])) {

      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Update Subscription",
        text = "Please select a subscription to access the Shiny app.",
        type = "error"
      )

    } else if (hold_stripe$trial_days_remaining > 0 || !is.na(hold_stripe$default_payment_method)) {
      # to to the Shiny app
      polished::remove_query_string()
      session$reload()

    } else {
      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Update Payment Method",
        text = 'Go to the "Billing" page and enter your credit card information to access the app.',
        type = "error"
      )
    }

  }, ignoreInit = TRUE)


  shiny::callModule(
    billing_module,
    "billing"
  )
}
