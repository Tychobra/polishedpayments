
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
#' @importFrom shinydashboard dashboardSidebar dashboardBody sidebarMenu menuItem tabItems
#' @importFrom htmltools HTML tags tagList
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyFeedback useShinyFeedback
#' @importFrom shinydashboardPlus dashboardHeaderPlus dashboardPagePlus
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

  head <- shinydashboardPlus::dashboardHeaderPlus(
    title = "Payments",
    left_menu = tagList(
      shiny::actionLink(
        ns("go_to_shiny_app"),
        paste0("Go to ", app_name),
        style = "margin-left: -15px; margin-top: -7.5px; color: #1a8dc7; font-size: 18px;"
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
    waiter::use_waiter(),
    waiter::waiter_show_on_load(html = waiter::spin_fading_circles()),

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
#' @param id the module id
#'
#' @export
#'
#' @importFrom httr GET POST status_code content
#' @importFrom shiny callModule observeEvent reactiveVal reactive req moduleServer
#' @importFrom polished profile_module remove_query_string
#' @importFrom shinyFeedback showToast
#'
#'
app_module <- function(id) {


  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns

      shiny::callModule(
        polished::profile_module,
        "profile"
      )

      shiny::observeEvent(input$go_to_shiny_app, {
        hold_sub <- sub_info()
        hold_user <- session$userData$user()


        if (length(intersect(hold_user$roles, getOption("pp")$free_roles)) > 0) {
          # User has a free role, so go to the Shiny app
          polished::remove_query_string()

          session$reload()
        } else if (is.null(hold_sub)) {

          shinyWidgets::show_alert(
            title = "Update Subscription",
            text = "Please select a subscription to access the Shiny app.",
            type = "error"
          )

        } else if (hold_sub$trial_days_remaining > 0 || !is.na(hold_sub$default_payment_method)) {
          # to to the Shiny app
          polished::remove_query_string()

          session$reload()
        } else {
          shinyWidgets::show_alert(
            title = "Update Payment Method",
            text = 'Go to the "Billing" page and enter your credit card information to access the app.',
            type = "error"
          )
        }

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

        out <- list()
        tryCatch({

          # API query to get user's billing information from the "subscriptions" table
          # update to use subscriptions endpoints of polishedapi
          res <- httr::GET(
            url = paste0(getOption("polished")$api_url, "/subscriptions"),
            query = list(
              app_uid = getOption("polished")$app_uid,
              user_uid = hold_user_uid
            ),
            httr::authenticate(
              user = getOption("polished")$api_key,
              password = ""
            )
          )

          out <- jsonlite::fromJSON(
            httr::content(res, "text", encoding = "UTF-8")
          )

          if (!identical(httr::status_code(res), 200L)) {
            print(out)
            stop("error getting subscription", call. = FALSE)
          }

          # correct possible dropped subscription columns if the subscription exists.
          if (length(out) > 0) {
            if (is.null(out$stripe_subscription_id)) out$stripe_subscription_id <- NA
            if (is.null(out$free_trial_days_remaining_at_cancel)) out$free_trial_days_remaining_at_cancel <- NA
          }


        }, error = function(err) {
          print(err)
          shinyFeedback::showToast('error', 'Error getting subscription info')
        })



        if (length(out) == 0) {
          # user does not yet have an entry in the billings table, so this is the
          # first sign in, we will add the user to the users table, create a Stripe
          # account for the user, and add an entry for the user to the billing table
          tryCatch({


            # create Stripe customer
            customer_id <- create_stripe_customer(
              email = hold_user_email,
              user_uid = hold_user_uid
            )

            # add the subscription to polished
            stripe_subscription_id <- create_stripe_subscription(
              customer_id,
              plan_to_enable = getOption("pp")$prices[[1]],
              days_remaining = getOption("pp")$trial_period_days
            )

            # add the newly created Stripe customer to the "subscriptions" table
            # send API request and determine the account uid based on the
            # API key sent with the request.
            res <- httr::POST(
              paste0(getOption("polished")$api_url, "/subscriptions"),
              body = list(
                "app_uid" = getOption("polished")$app_uid,
                "user_uid" = hold_user_uid,
                "stripe_customer_id" = customer_id,
                "stripe_subscription_id" = stripe_subscription_id
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

            out <- list(
              uid = res_content$uid,
              user_uid = hold_user_uid,
              stripe_customer_id = customer_id,
              stripe_subscription_id = stripe_subscription_id
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

            trial_period_days <- getOption("pp")$trial_period_days

            tryCatch({

              stripe_subscription_id <- create_stripe_subscription(
                out$stripe_customer_id,
                plan_to_enable = getOption("pp")$stripe$prices[[1]],
                days_remaining = trial_period_days
              )

              # add newly created subscription to polished db via polished API
              res <- httr::PUT(
                url = paste0(getOption("polished")$api_url, "/subscriptions"),
                encode = "json",
                body = list(
                  stripe_subscription_id = stripe_subscription_id,
                  subscription_uid = out$uid
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


              session$userData$billing_trigger(session$userData$billing_trigger() + 1)
            }, error = function(err) {
              print(err)
              shinyFeedback::showToast("error", "Error creating subscription")
            })
          }

          # check that Stripe customer does not have any issues and log any potential
          # issues
          #tryCatch({
          #  stripe_customer <- get_stripe_customer(out$stripe_customer_id)
          #
          #  if (isTRUE(stripe_customer$deleted)) {
          #    stop("[Stripe Error] Stripe customer deleted")
          #  }
          #}, error = function(err) {
          #
          #
          #  print("[Stripe Error] getting the Stripe customer")
          #  print(err)
          #
          #})

        }


        session$userData$billing(as.list(out))
      }, priority = 10)


      session$userData$sub_info_trigger <- shiny::reactiveVal(0)
      ### GET USER'S SUBSCRIPTION ###
      sub_info <- shiny::reactive({
        shiny::req(session$userData$billing())
        session$userData$sub_info_trigger()

        out <- NULL

        billing <- session$userData$billing()
        if (!is.na(billing$stripe_subscription_id)) {

          tryCatch({

            out <- get_stripe_subscription(
              stripe_subscription_id = billing$stripe_subscription_id
            )

          }, error = function(err) {
            print(err)
            # set the subscription to polished db via polished API
            res <- httr::PUT(
              url = paste0(getOption("polished")$api_url, "/subscriptions"),
              encode = "json",
              body = list(
                subscription_uid = billing$uid,
                stripe_subscription_id = NA
              ),
              httr::authenticate(
                user = getOption("polished")$api_key,
                password = ""
              )
            )
            # hard fail if above query fails so that app does not somehow go into look
            if (!identical(httr::status_code(res), 200L)) {
              res_content <- jsonlite::fromJSON(
                httr::content(res, "text", encoding = "UTF-8")
              )
              print(res_content)
              stop("failure to sync Stripe subscription with DB", call. = FALSE)
            }

            session$userData$billing_trigger(session$userData$billing_trigger() + 1)
            shinyFeedback::showToast("error", "subscription not found")

          })
        }

        out
      })

      shiny::callModule(
        billing_module,
        "billing",
        sub_info = sub_info
      )
    }
  )

}



