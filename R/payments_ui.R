#' adds polished payments to your Shiny UI
#'
#' @param ui the Shiny app UI
#' @param app_name the app name to display to the user in the return to app link.
#'
#' @importFrom htmltools tagList tags
#' @importFrom httr status_code
#' @importFrom polished normalize_ui .polished
#' @importFrom shiny parseQueryString
#'
#' @export
payments_ui <- function(
  ui,
  app_name = polished::.polished$app_name
) {

  function(request) {

    query <- shiny::parseQueryString(request$QUERY_STRING)
    page_query <- query$page

    user <- request$polished_user
    stripe_user <- NULL



    if (!is.null(user)) {
      err_out <- NULL
      tryCatch({
        # get existing subscriptions from Polished API

        customer_res <- get_customers(
          app_uid = polished::.polished$app_uid,
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




          if (!is.null(.pp$prices) && .pp$trial_period_days > 0) {
            # Step 2: If the app is using polishedpayments subscriptions, create the Stripe subscription on Stripe.
            # If the subscription does not have a trial period, then we can't create the subscription until a payment
            # method is enabled, so also do not create the subscription if no trial period.
            stripe_subscription_id <- create_stripe_subscription(
              stripe_customer_id,
              plan_to_enable = .pp$prices[1],
              days_remaining = .pp$trial_period_days
            )
          } else {
            stripe_subscription_id <- NULL
          }

          # Step 3: add the newly created Stripe customer + possible subscription to the "customers" table
          add_customer_res <- add_customer(
            app_uid = polished::.polished$app_uid,
            user_uid = user$user_uid,
            stripe_customer_id = stripe_customer_id,
            stripe_subscription_id = stripe_subscription_id
          )

          if (!identical(httr::status_code(add_customer_res$response), 200L)) {
            print(add_customer_res$content)
            stop("Polished API error creating customer", call. = FALSE)
          }

          customer_res <- get_customers(
            app_uid = polished::.polished$app_uid,
            user_uid = user$user_uid
          )

          customer <- customer_res$content

        }


        stripe_user <- get_stripe(
          user_uid = user$user_uid
        )

      }, error = function(err) {

        # if (identical(err$message, "subscription canceled")) {
        #   update_customer(
        #     customer_uid = customer$uid,
        #     cancel_subscription = TRUE,
        #     free_trial_days_remaining_at_cancel = 0
        #   )
        #   customer$stripe_subscription_id <- NA
        # }

        msg <- "unable to load UI"
        print(msg)
        print(err)
        err_out <<- err$message

        invisible(NULL)
      })

      if (!is.null(err_out)) {
        return(tags$h1("Network connection error.  Please try again."))
      }
    }


    request$stripe <- stripe_user

    if (identical(page_query, "payments")) {

      if (is.null(user)) {
        # TODO: redirect to registration page
        out <- tags$h1("Redirecting to registration page")
      } else {
        # continue to payments page
        out <- payments_app_ui(
          request = request,
          app_name = app_name
        )
      }

    } else {

      if (isTRUE(.pp$is_subscription_required)) {

        if (is.null(user)) {
          # redirect to registration page
          out <- tags$h1("Redirecting to registration page")
        } else if (is_subscription_valid(stripe_user)) {
            # Go to custom Shiny app using polishedpayments
            out <- htmltools::tagList(
              htmltools::tags$head(
                tags$script(src = "https://js.stripe.com/v3"),
                tags$script(paste0("var stripe = Stripe('", .pp$keys$public, "');")),
                tags$script(src = "polishedpayments/js/polishedpayments.js?version=3")
              ),
              force(polished::normalize_ui(ui, request))
            )
        } else {
          # Not Authorized - No subscription found - Redirecting to Payments
          out <- tags$h1("Redirecting to payments page")
        }

      }  else {

        # No subscription required, so Go to app
        out <- htmltools::tagList(
          htmltools::tags$head(
            tags$script(src = "https://js.stripe.com/v3"),
            tags$script(paste0("var stripe = Stripe('", .pp$keys$public, "');")),
            tags$script(src = "polishedpayments/js/polishedpayments.js?version=3")
          ),
          force(polished::normalize_ui(ui, request))
        )

      }

    }


    out
  }
}
