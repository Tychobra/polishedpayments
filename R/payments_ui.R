#' adds polished payments to your Shiny UI
#'
#' @param ui the Shiny app UI
#' @param app_name the app name to display to the user in the return to app link.
#'
#' @importFrom htmltools tagList tags
#' @importFrom httr status_code
#' @importFrom shiny parseQueryString
#'
#' @export
payments_ui <- function(
  ui,
  app_name = getOption("polished")$app_name_display
) {

  function(request) {

    if (is.function(ui)) {
      ui <- ui(request)
    }
    ui <- force(ui)

    user <- request$polished_user

    if (is.null(user)) {
      return()
    }

    query <- shiny::parseQueryString(request$QUERY_STRING)
    payments_query <- query$payments


    err_out <- NULL
    tryCatch({
      # get existing subscriptions from Polished API
      customer_res <- get_customers(
        app_uid = getOption("polished")$app_uid,
        user_uid = user$user_uid
      )

      if (!identical(httr::status_code(customer_res$response), 200L)) {
        stop(customer_res$content, call. = FALSE)
      }

      customer <- customer_res$content

      if (identical(nrow(customer), 0L)) {

        # This is the first time the user is accessing the app using polishedpayments, so
        # set the user up with the default subscription

        # Step 1: create Stripe customer
        stripe_customer_id <- create_stripe_customer(
          email = user$email,
          user_uid = user$user_uid
        )


        if (!is.null(getOption("pp")$prices) && getOption("pp")$trial_period_days > 0) {
          # Step 2: If the app is using polishedpayments subscriptions, create the Stripe subscription on Stripe.
          # If the subscription does not have a trial period, then we can't create the subscription until a payment
          # method is enabled, so also do not create the subscription if no trial period.
          stripe_subscription_id <- create_stripe_subscription(
            stripe_customer_id,
            plan_to_enable = getOption("pp")$prices[1],
            days_remaining = getOption("pp")$trial_period_days
          )
        } else {
          stripe_subscription_id <- NULL
        }

        # Step 3: add the newly created Stripe customer + possible subscription to the "customers" table
        add_customer_res <- add_customer(
          app_uid = getOption("polished")$app_uid,
          user_uid = user$user_uid,
          stripe_customer_id = stripe_customer_id,
          stripe_subscription_id = stripe_subscription_id
        )

        if (!identical(httr::status_code(add_customer_res$response), 200L)) {
          print(add_customer_res$content)
          stop("Polished API error creating customer", call. = FALSE)
        }

        customer_res <- get_customers(
          app_uid = getOption("polished")$app_uid,
          user_uid = user$user_uid
        )

        customer <- customer_res$content

      }

      if (!is.na(customer$stripe_subscription_id)) {
        stripe_sub <- get_stripe_subscription(customer$stripe_subscription_id)
      }

    }, error = function(err) {

      if (identical(err$message, "subscription canceled")) {
        update_customer(
          customer_uid = customer$uid,
          cancel_subscription = TRUE,
          free_trial_days_remaining_at_cancel = 0
        )
        customer$stripe_subscription_id <- NA
      }

      msg <- "unable to load UI"
      print(msg)
      print(err)
      err_out <<- err$message

      invisible(NULL)
    })

    if (!is.null(err_out)) {
      return(tags$h1("Network connection error.  Please try again."))
    }


    if (identical(payments_query, "TRUE")) {

      out <- payments_app_ui(
        app_name = app_name
      )

    } else {

      if (is.null(getOption("pp")$prices) || length(intersect(user$roles, getOption("pp")$free_roles)) > 0) {
        # No subscription required, so Go to app
        out <- htmltools::tagList(
          htmltools::tags$head(
            tags$script(src = "https://js.stripe.com/v3"),
            tags$script(paste0("var stripe = Stripe('", getOption("pp")$keys$public, "');")),
            tags$script(src = "polishedpayments/js/polishedpayments.js?version=3")
          ),
          ui
        )
      }  else {


        if (
          !is.na(customer$stripe_subscription_id) &&
          (stripe_sub$trial_days_remaining > 0 || !is.na(customer$default_payment_method))
        ) {
          # Go to payments page
          out <- htmltools::tagList(
            htmltools::tags$head(
              tags$script(src = "https://js.stripe.com/v3"),
              tags$script(paste0("var stripe = Stripe('", getOption("pp")$keys$public, "');")),
              tags$script(src = "polishedpayments/js/polishedpayments.js?version=3")
            ),
            ui
          )

        } else {
          # Not Authorized - No subscription found - Redirecting to Payments
          out <- NULL
        }

      }

    }


    out
  }
}
