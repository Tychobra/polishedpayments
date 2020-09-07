
#' The payments Shiny app
#'
#' The Shiny module UI for the Stripe payments Shiny app.  This app can be easily added
#' to your Shiny apps that use polished.
#'
#' @param id the Shiny module id
#' @param custom_ui Either \code{NULL}, the default, or a list of 2 elements containing custom
#' UI to add additional 'shinydashboard' tabs to the 'polished' "Admin Panel".
#'
#' @export
#'
#' @importFrom shiny NS icon
#' @importFrom shinydashboard dashboardSidebar dashboardBody sidebarMenu menuItem tabItems
#' @importFrom htmltools HTML tags tagList
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyFeedback useShinyFeedback
#' @importFrom shinydashboardPlus dashboardHeaderPlus dashboardPagePlus
#' @importFrom polished profile_module_ui
#'
#' @return the UI for the "Admin Panel"
#'
#' @noRd
app_module_ui <- function(
  id,
  custom_ui = NULL
) {
  ns <- shiny::NS(id)

  stopifnot(is.null(custom_ui) || names(custom_ui) == c("menu_items", "tab_items"))

  stripe_key_publishable <- "pk_test_P6RyZfb9X5UDWkaa88og5FeP00RMX1hfRr"#app_config$stripe$keys$publishable

  browser()
  head <- shinydashboardPlus::dashboardHeaderPlus(
    title = "Payments",
    polished::profile_module_ui(ns("profile"))
  )

  if (is.null(custom_ui$menu_items)) {
    sidebar <- shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        id = ns("sidebar_menu"),
        shinydashboard::menuItem("Pricing", tabName = ns("pricing"), icon = icon("usd")),
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
        tabName = ns("pricing"),
        pricing_module_ui(ns("pricing"))
      ),
      shinydashboard::tabItem(
        tabName = ns("billing"),
        billing_module_ui(ns("billing"))
      )
    )
  } else {
    tab_items <- shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = ns("pricing"),
        pricing_module_ui(ns("pricing"))
      ),
      shinydashboard::tabItem(
        tabName = ns("billing"),
        billing_module_ui(ns("billing"))
      ),
      custom_ui$tab_items
    )
  }


  shiny_app_button <- tags$div(
    style = "position: fixed; bottom: 15px; right: 15px; z-index: 1000;",
    shiny::actionButton(
      ns("go_to_shiny_app"),
      "Shiny App",
      icon = shiny::icon("rocket"),
      class = "btn-primary btn-lg",
      style = "color: #FFFFFF;"
    )
  )

  body <- shinydashboard::dashboardBody(
    tags$head(
      tags$link(rel = "shortcut icon", href = "images/alpha-arch-logo-square.png"),
      tags$script(src="https://js.stripe.com/v3"),
      tags$script(paste0("var stripe = Stripe('", stripe_key_publishable, "');")),
      shinyjs::useShinyjs(),
      shinyFeedback::useShinyFeedback()
    ),

    shiny_app_button,

    tab_items
  )

  shinydashboardPlus::dashboardPagePlus(
    head,
    sidebar,
    body,
    title = "Polished",
    skin = "black-light"
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
#' @importFrom shiny callModule observeEvent
#' @importFrom polished remove_query_string
#'
#' @noRd
#'
app_module <- function(input, output, session) {
  ns <- session$ns

  shiny::callModule(
    profile_module,
    "profile"
  )

  shiny::observeEvent(input$go_to_shiny_app, {

    # to to the Shiny app
    polished::remove_query_string()

    session$reload()

  }, ignoreInit = TRUE)

  shiny::callModule(pricing_module, "pricing")
  shiny::callModule(billing_module, "billing")
}

