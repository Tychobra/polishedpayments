
#' The payments Shiny app
#'
#' The Shiny module UI for the Stripe payments Shiny app.  This app can be easily added
#' to your Shiny apps that use polished.
#'
#' @param id the Shiny module id
#' @param custom_ui Either \code{NULL}, the default, or a list of 2 elements containing custom
#' UI to add additional 'shinydashboard' tabs to the 'polished' "Admin Panel".
#' @param app_name the app name to display to users.
#'
#' @export
#'
#' @importFrom shiny NS icon
#' @importFrom shinydashboard dashboardSidebar dashboardBody sidebarMenu menuItem tabItems dashboardHeader dashboardPage
#' @importFrom htmltools HTML tags tagList
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyFeedback useShinyFeedback
#' @importFrom polished profile_module_ui
#'
#' @return the UI for the "Admin Panel"
#'
app_module_ui <- function(
  id,
  custom_ui = NULL,
  app_name = getOption("polished")$app_name_display
) {
  ns <- shiny::NS(id)

  stopifnot(is.null(custom_ui) || names(custom_ui) == c("menu_items", "tab_items"))

  stripe_key_public <- getOption("pp")$keys$public

  head <- shinydashboard::dashboardHeader(
    title = "Payments",
    left_menu = tags$li(
      class = "dropdown",
      shiny::actionLink(
        ns("go_to_shiny_app"),
        paste0("Go to ", app_name),
        style = "margin-left: -15px; color: #1a8dc7; font-size: 18px;"
      )
    ),
    polished::profile_module_ui(ns("profile"))
  )

  if (is.null(custom_ui$menu_items)) {
    sidebar <- shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        id = ns("sidebar_menu"),
        shinydashboard::menuItem("Billing", tabName = ns("billing"), icon = icon("credit-card"))
      )
    )
  } else {
    sidebar <- shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        id = ns("sidebar_menu"),
        shinydashboard::menuItem(
          text = "User Access",
          tabName = "user_access",
          icon = shiny::icon("users")
        ),

        custom_ui$menu_items
      )
    )
  }


  if (is.null(custom_ui$tab_items)) {
    tab_items <- shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = ns("billing"),
        billing_module_ui(ns("billing"))
      )
    )
  } else {
    tab_items <- shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = ns("billing"),
        billing_module_ui(ns("billing"))
      ),
      custom_ui$tab_items
    )
  }




  body <- shinydashboard::dashboardBody(
    tags$head(
      tags$link(rel = "shortcut icon", href = "polishedpayments/images/polished_logo_transparent.png"),
      tags$script(src = "https://js.stripe.com/v3"),
      tags$script(paste0("var stripe = Stripe('", stripe_key_public, "');")),
      shinyjs::useShinyjs(),
      shinyFeedback::useShinyFeedback()
    ),
    #waiter::use_waiter(),
    #waiter::waiter_show_on_load(html = waiter::spin_fading_circles()),

    tab_items
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
#' @importFrom tibble as_tibble
#'
#'
app_module <- function(input, output, session) {
  ns <- session$ns

  shiny::callModule(
    polished::profile_module,
    "profile"
  )

  shiny::observeEvent(input$go_to_shiny_app, {

    hold_sub <- session$userData$stripe()$subscription
    hold_user <- session$userData$user()

    if (length(intersect(hold_user$roles, getOption("pp")$free_roles)) > 0 || is.null(getOption("pp")$prices)) {
      # User has a free role (or `subscription_prices = NULL`), so go to the Shiny app
      polished::remove_query_string()
      session$reload()

    } else if (is.na(hold_sub)) {

      shinyWidgets::sendSweetAlert(
        session = session,
        title = "Update Subscription",
        text = "Please select a subscription to access the Shiny app.",
        type = "error"
      )

    } else if (hold_sub$trial_days_remaining > 0 || !is.na(hold_sub$default_payment_method)) {
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
