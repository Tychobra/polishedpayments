
#' The payments Shiny app
#'
#' The Shiny module UI for the Stripe payments Shiny app.  This app can be easily added
#' to your Shiny apps that use polished.
#'
#' @param id the Shiny module id
#' @param custom_ui Either \code{NULL}, the default, or a list of 2 elements containing custom
#' UI to add additional 'shinydashboard' tabs to the 'polished' "Admin Panel".
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

  stopifnot(is.null(custom_admin_ui) || names(custom_admin_ui) == c("menu_items", "tab_items"))

  head <- shinydashboardPlus::dashboardHeaderPlus(
    title = options$title,
    polished::profile_module_ui(ns("profile"))
  )

  if (is.null(custom_admin_ui$menu_items)) {
    sidebar <- shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        id = ns("sidebar_menu"),
        shinydashboard::menuItem("Pricing", tabName = ns("pricing"), icon = icon("usd")),
        shinydashboard::menuItem("Billing", tabName = ns("billing"), icon = icon("credit-card")),


        options$sidebar_branding
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

        custom_admin_ui$menu_items,

        options$sidebar_branding
      )
    )
  }


  if (is.null(custom_admin_ui$tab_items)) {
    tab_items <- shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = ns("pricing"),
        pricing_module_ui("pricing")
      ),
      shinydashboard::tabItem(
        tabName = ns("billing"),
        billing_module_ui("billing")
      )
    )
  } else {
    tab_items <- shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = ns("pricing"),
        pricing_module_ui("pricing")
      ),
      shinydashboard::tabItem(
        tabName = ns("billing"),
        billing_module_ui("billing")
      ),
      custom_admin_ui$tab_items
    )
  }


  shiny_app_button <- tag$div(
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
      tags$script(paste0("var stripe = Stripe('", app_config$stripe$keys$publishable, "');")),
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
#' @importFrom shiny callModule observeEvent
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
    remove_query_string()

    session$reload()

  }, ignoreInit = TRUE)

  shiny::callModule(user_access_module, "user_access")
}

