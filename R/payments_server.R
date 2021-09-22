


#' adds polished payments to your Shiny server
#'
#'
#' Wrap your Shiny server in this function to add polished payments to your Shiny
#' server before your custom Shiny app's server logic runs.
#'
#' @param server the Shiny server function
#'
#' @importFrom shiny getDefaultReactiveDomain observeEvent
#' @importFrom httr status_code
#'
#' @export
#'
payments_server <- function(
  server
) {

  function(input, output, session) {

    session$userData$stripe <- reactiveVal(NULL)
    session$userData$stripe_trigger <- reactiveVal(0)

    shiny::observeEvent(session$userData$user(), {

      query_list <- shiny::getQueryString()
      payments_query <- query_list$payments

      hold_user <- session$userData$user()

      tryCatch({
        # get any existing subscriptions from Polished API
        customer_res <- get_customers(
          app_uid = getOption("polished")$app_uid,
          user_uid = session$userData$user()$user_uid
        )

        if (!identical(httr::status_code(customer_res$response), 200L)) {
          print(customer_res$content)
          stop("error getting subscription from Polished API", call. = FALSE)
        }

        customer <- as.list(customer_res$content)



        if (identical(nrow(customer), 0L)) {
          stop("no customer", call. = FALSE)

        } else if (is.null(getOption("pp")$prices)) {
          # app is using single payments only (i.e. no subscriptions)

          session$userData$stripe(list(
            polished_customer_uid = customer$uid,
            stripe_customer_id = customer$stripe_customer_id,
            free_user = FALSE,
            default_payment_method = customer$default_payment_method,
            trial_days_remaining = customer$free_trial_days_remaining_at_cancel,
            subscription = NA
          ))
          return()

        } else if (length(intersect(hold_user$roles, getOption("pp")$free_roles)) > 0) {
          # app is using subscriptions, but user is allowed to access the app without a subscription
          # because they are a "free_user"
          session$userData$stripe(list(
            polished_customer_uid = customer$uid,
            stripe_customer_id = customer$stripe_customer_id,
            free_user = TRUE,
            default_payment_method = customer$default_payment_method,
            trial_days_remaining = customer$free_trial_days_remaining_at_cancel,
            subscription = NA
          ))
          return()

        } else {

          stripe_out <- list(
            polished_customer_uid = customer$uid,
            stripe_customer_id = customer$stripe_customer_id,
            free_user = FALSE,
            default_payment_method = customer$default_payment_method,
            trial_days_remaining = customer$free_trial_days_remaining_at_cancel
          )

          if (is.na(customer$stripe_subscription_id)) {
            # customer does not have a subscription or trial and one is required.  Since user is on app, redirect to payments.
            if (!identical(payments_query, "TRUE")) {
              shiny::updateQueryString(
                queryString = "?payments=TRUE",
                session = session,
                mode = "replace"
              )
              session$reload()
              return()
            } else {
              stripe_out$subscription <- NA
            }


          } else {
            stripe_sub <- get_stripe_subscription(customer$stripe_subscription_id)

            # if subscription is NA, that means the user has canceled their subscription, so redirect them to the
            # account page for them to restart their subscription
            # user is either in trial period or they already have a subscription with
            # billing enabled, so let them access the app.
            if ((stripe_sub$trial_days_remaining <= 0 && is.na(customer$default_payment_method)) && !identical(payments_query, "TRUE")) {
              shiny::updateQueryString(
                queryString = "?payments=TRUE",
                session = session,
                mode = "replace"
              )
              session$reload()
            } else {

              stripe_out$trial_days_remaining <- stripe_sub$trial_days_remaining

              stripe_out$subscription = list(
                uid = customer$uid,
                stripe_subscription_id = customer$stripe_subscription_id,
                item_id = stripe_sub$item_id,
                plan_id = stripe_sub$plan_id,
                nickname = stripe_sub$nickname,
                amount = stripe_sub$amount,
                interval = stripe_sub$interval,
                trial_end = stripe_sub$trial_end,
                created_at = customer$created_at
              )
            }

          }


          session$userData$stripe(stripe_out)
        }

      }, error = function(err) {

        print(err)

        if (identical(err$message, "unable to create subscription") || identical(err$message, "subscription canceled")) {
          # you can't create a Stripe subscription until after the Stripe user
          # has entered their payment info.  If the stripe user needs to create
          # a subscription (i.e. )
          shiny::updateQueryString(
            queryString = "?payments=TRUE",
            session = session,
            mode = "replace"
          )
          session$reload()
        } else {

          showToast("error", err$message)
        }

        invisible(NULL)
      })

    })




    observeEvent(session$userData$stripe(), {
      query_list <- shiny::getQueryString()
      payments_query <- query_list$payments

      if (identical(payments_query, "TRUE")) {

        payments_app_server(input, output, session)

      } else {

        server(input, output, session)
      }

    }, once = TRUE)

  }
}
