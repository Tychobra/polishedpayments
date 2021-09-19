
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

    } else if (is.null(hold_sub)) {

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

  # get the user's billing information from the "billing" table
  session$userData$stripe <- reactiveVal(NULL)
  session$userData$stripe_trigger <- reactiveVal(0)

  # this observe will trigger as soon as the session starts (every time a session starts).
  # It will only trigger once per session. If the user is a new user it will create the user (
  # create a Stripe customer for the user and add a row with the user's Stripe customer ID
  # to the "subscriptions" table).  If the the user is a returning user, it doesn't do anything
  # besides trigger the next observe
  observeEvent(list(
    session$userData$user(),
    session$userData$stripe_trigger()
  ), {
    hold_user_uid <- session$userData$user()$user_uid
    hold_user_email <- session$userData$user()$email

    sub_db <- list()
    err_out <- NULL
    tryCatch({
      # API query to get user's billing information from the "subscriptions" table
      res <- httr::GET(
        url = paste0(getOption("polished")$api_url, "/subscriptions"),
        query = list(
          app_uid = getOption("polished")$app_uid,
          user_uid = hold_user_uid,
          is_live = getOption("pp")$is_live
        ),
        httr::authenticate(
          user = getOption("polished")$api_key,
          password = ""
        )
      )

      sub_db <- jsonlite::fromJSON(
        httr::content(res, "text", encoding = "UTF-8")
      )

      if (!identical(httr::status_code(res), 200L)) {
        print(sub_db)
        stop("error getting subscription", call. = FALSE)
      }

      sub_db <- tibble::as_tibble(sub_db)

    }, error = function(err) {
      msg <- 'Error getting subscription info'
      print(msg)
      print(err)
      err_out <<- err$message
      shinyFeedback::showToast('error', msg)
      invisible()
    })

    if (!is.null(err_out)) {
      return()
    }

    if (identical(nrow(sub_db), 0L)) {
      # user does not yet have an entry in the "subscriptions" table, so this is the
      # first sign in, we will add the user to the "subscriptions" table, create a Stripe
      # account for the user, and add an entry for the user to the "subscriptions" table
      tryCatch({

        # create Stripe customer
        customer_id <- create_stripe_customer(
          email = hold_user_email,
          user_uid = hold_user_uid
        )

        if (getOption("pp")$trial_period_days > 0 && !is.null(getOption("pp")$prices)) {
          # add the subscription to polished
          stripe_subscription_id <- create_stripe_subscription(
            customer_id,
            plan_to_enable = getOption("pp")$prices[[1]],
            days_remaining = getOption("pp")$trial_period_days
          )
        } else {
          # there is no trial period, and the user has not enabled a payment method,
          # so we cannot go ahead and set up a subscription.
          stripe_subscription_id <- NULL
        }

        # add the newly created Stripe customer to the "subscriptions" table
        res <- httr::POST(
          paste0(getOption("polished")$api_url, "/subscriptions"),
          body = list(
            "app_uid" = getOption("polished")$app_uid,
            "user_uid" = hold_user_uid,
            "stripe_customer_id" = customer_id,
            "stripe_subscription_id" = stripe_subscription_id,
            "is_live" = getOption("pp")$is_live
          ),
          encode = "json",
          httr::authenticate(
            user = getOption("polished")$api_key,
            password = ""
          )
        )

        res_content <- jsonlite::fromJSON(
          httr::content(res, "text", encoding = "UTF-8")
        )

        if (!identical(httr::status_code(res), 200L)) {
          print(res_content)
          stop("Error saving subsciption to db", call. = FALSE)
        }

        sub_db <- list(
          uid = res_content$uid,
          user_uid = hold_user_uid,
          stripe_customer_id = customer_id,
          stripe_subscription_id = stripe_subscription_id
        )

        if (is.null(getOption("pp")$prices)) {
          sub_db$stripe_subscription_id <- NA
        }

        sub_db$created_at <- Sys.time()
        sub_db$trial_days_remaining <- NA

        sub_db <- tibble::as_tibble(sub_db)

      }, error = function(err) {

        msg <- "Error Setting up your account"
        print(msg)
        print(err)
        shinyFeedback::showToast("error", msg)

        invisible()
      })

    } else {

      # if user does not have a subscription, and they have not canceled their subscription, and `subscription_prices != NULL`
      # go ahead and create a subscription
      if (!is.null(getOption("pp")$prices) && is.na(sub_db$stripe_subscription_id) && is.na(sub_db$free_trial_days_remaining_at_cancel)) {

        trial_period_days <- getOption("pp")$trial_period_days
        query_list <- shiny::getQueryString()

        if (trial_period_days <= 0) {

          if (!identical(query_list$page, "account")) {
            # there is no free trial, so redirect to the account page for them
            # to choose a subscription and enable billing.
            shiny::updateQueryString(
              queryString = "?page=account",
              session = session,
              mode = "replace"
            )
            session$reload()
          }


        } else {

          tryCatch({

            stripe_subscription_id <- create_stripe_subscription(
              sub_db$stripe_customer_id,
              plan_to_enable = getOption("pp")$prices[[1]],
              days_remaining = trial_period_days
            )

            # add newly created subscription to polished db via polished API
            res <- httr::PUT(
              url = paste0(getOption("polished")$api_url, "/subscriptions"),
              encode = "json",
              body = list(
                stripe_subscription_id = stripe_subscription_id,
                subscription_uid = sub_db$uid
              ),
              httr::authenticate(
                user = getOption("polished")$api_key,
                password = ""
              )
            )

            if (!identical(httr::status_code(res), 200L)) {

              res_content <- jsonlite::fromJSON(
                httr::content(res, "text", encoding = "UTF-8")
              )

              stop(res_content, call. = FALSE)
            }


            session$userData$stripe_trigger(session$userData$stripe_trigger() + 1)
          }, error = function(err) {
            print(err)
            shinyFeedback::showToast("error", "Error creating subscription")
          })
        }

      }

    }


    if (!identical(nrow(sub_db), 0L)) {

      out <- list(
        stripe_customer_id = sub_db$stripe_customer_id,
        free_user = FALSE,
        default_payment_method = NA,
        subscription = NA
      )

      if (!is.na(sub_db$stripe_subscription_id) && !is.null(getOption("pp")$prices)) {
        hold_sub <- get_stripe_subscription(sub_db$stripe_subscription_id)

        out$default_payment_method <- hold_sub$default_payment_method

        out$subscription <- list(
          uid = sub_db$uid,
          stripe_subscription_id = sub_db$stripe_subscription_id,
          item_id = hold_sub$item_id,
          plan_id = hold_sub$plan_id,
          nickname = hold_sub$nickname,
          amount = hold_sub$amount,
          interval = hold_sub$interval,
          trial_end = hold_sub$trial_end,
          is_billing_enabled = if (is.na(hold_sub$default_payment_method)) FALSE else TRUE,
          trial_days_remaining = hold_sub$trial_days_remaining,
          created_at = sub_db$created_at
        )
      }

      session$userData$stripe(out)
    }
  }, priority = 10)

  shiny::callModule(
    billing_module,
    "billing"
  )
}
