
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

  stripe_key_publishable <- app_config$stripe$keys$pub#app_config$stripe$keys$publishable

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
#' @importFrom shinyFeedback showToast
#' @importFrom dplyr %>% tbl filter collect
#' @importFrom dbplyr in_schema
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

  # get the user'd billing information from the "billing" table
  session$userData$billing <- reactiveVal(NULL)
  session$userData$billing_trigger <- reactiveVal(0)

  # this observe will trigger as soon as the session starts (every time a session starts).
  # It will only trigger once per session. If the user is a new user it will create the user (
  # create a Stripe customer for the user and add a row with the user's Stripe customer ID
  # to the "billing" table).  If the the user is a returning user, it doesn't do anything
  # besides trigger the next observe
  observeEvent(list(
    session$userData$user(),
    session$userData$billing_trigger()
  ), {
    hold_user_uid <- session$userData$user()$user_uid
    hold_user_email <- session$userData$user()$email

    out <- NULL
    tryCatch({

      # TODO: API query to get user's billing information from the "subscriptions" table
      # res <- httr::GET()
      out <- conn %>%
        tbl(in_schema("polished", "subscriptions")) %>%
        filter(user_uid == hold_user_uid) %>%
        collect()

    }, error = function(err) {
      print(err)
      shinyFeedback::showToast('error', 'Error getting subscription info')
    })



    if (nrow(out) == 0) {
      # user does not yet have an entry in the billings table, so this is the
      # first sign in, we will add the user to the users table, create a Stripe
      # account for the user, and add an entry for the user to the billing table
      tryCatch({


        # create Stripe user
        res <- httr::POST(
          "https://api.stripe.com/v1/customers",
          body = list(
            "email" = hold_user_email,
            "metadata[polished_uid]" = hold_user_uid
          ),
          encode = "form",
          httr::authenticate(
            user = app_config$stripe$keys$secret,
            password = ""
          )
        )

        httr::stop_for_status(res)

        res_data <- jsonlite::fromJSON(
          httr::content(res, "text", encoding = "UTF-8")
        )

        customer_id <- res_data$id
        if (is.null(customer_id)) {
          stop("no customer id received from Stripe")
        }

        # if the trial period days is set in config.yml, then go ahead and set up the
        # subscription without a credit card
        stripe_subscription_id <- create_subscription(
          customer_id,
          plan_to_enable = app_config$stripe$prices[[1]],
          days_remaining = app_config$stripe$trial_period_days
        )



        billing_uid <- uuid::UUIDgenerate()
        # add the newly created Stripe customer to the "billing" table

        out <- list(
          uid = billing_uid,
          # just using my development account UID for initial development
          account_uid = "1d03c693-20e4-434d-b2b8-c1876dc2014b",
          user_uid = hold_user_uid,
          stripe_customer_id = customer_id,
          stripe_subscription_id = stripe_subscription_id
        )

        # TODO: update this to be an API request and determine the account uid based on the
        # API key sent with the request.
        DBI::dbExecute(
          conn,
          "INSERT INTO polished.subscriptions ( uid, account_uid, user_uid, stripe_customer_id, stripe_subscription_id )
          VALUES ( $1, $2, $3, $4, $5 )",
          params = unname(out)
        )

        out$created_at <- Sys.time()
        out$free_trial_days_remaining_at_cancel <- NA

      }, error = function(err) {

        print(err)
        shinyFeedback::showToast("error", "Error Setting up your account")
      })

    } else {

      # if user does not have a subscription, and they have not canceled their subscription,
      # go ahead and create a subscription
      if (is.na(out$stripe_subscription_id) && is.na(out$free_trial_days_remaining_at_cancel)) {

        trial_period_days <- app_config$stripe$trial_period_days

        tryCatch({

          stripe_subscription_id <- create_subscription(
            out$stripe_customer_id,
            plan_to_enable = app_config$stripe$prices[[1]],
            days_remaining = trial_period_days
          )

          dbExecute(
            conn,
            "UPDATE polished.subscriptions SET stripe_subscription_id=$1 WHERE uid=$2",
            params = list(
              stripe_subscription_id,
              out$uid
            )
          )

          session$userData$billing_trigger(session$userData$billing_trigger() + 1)
        }, error = function(err) {
          print(err)
          shinyFeedback::showToast("error", "Error creating subscription")
        })
      }

      # check that Stripe customer does not have any issues and log any potential
      # issues
      tryCatch({
        stripe_customer <- get_stripe_customer(out$stripe_customer_id)

        if (isTRUE(stripe_customer$deleted)) {
          stop("[Stripe Error] Stripe customer deleted")
        }
      }, error = function(err) {


        print("[Stripe Error] getting the Stripe customer")
        print(err)

      })

    }


    session$userData$billing(as.list(out))
  }, priority = 10)


  session$userData$sub_info_trigger <- reactiveVal(0)
  ### GET USER'S SUBSCRIPTION ###
  sub_info <- reactive({
    req(session$userData$billing())
    session$userData$sub_info_trigger()

    out <- NULL

    billing <- session$userData$billing()
    if (!is.na(billing$stripe_subscription_id)) {

      tryCatch({

        out <- get_stripe_subscription(
          conn,
          billing$stripe_subscription_id,
          api_key = app_config$stripe$keys$secret
        )

      }, error = function(err) {
        print(err)

        shinyFeedback::showToast("error", "subscription not found")
      })
    }

    out
  })

  shiny::callModule(
    pricing_module,
    "pricing",
    sub_info = sub_info
  )
  shiny::callModule(
    billing_module,
    "billing",
    sub_info = sub_info
  )
}

