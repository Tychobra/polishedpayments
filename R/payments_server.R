


#' adds polished payments to your Shiny server
#'
#'
#' Wrap your Shiny server in this function to add polished payments to your Shiny
#' server before your custom Shiny app's server logic runs.
#'
#' @param server the Shiny server function
#'
#' @importFrom shiny getDefaultReactiveDomain observeEvent
#' @importFrom httr GET authenticate content status_code
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
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

      hold_user <- session$userData$user()

      tryCatch({
        # get any existing subscriptions from Polished API
        res <- httr::GET(
          paste0(getOption("polished")$api_url, "/subscriptions"),
          httr::authenticate(
            user = getOption("polished")$api_key,
            password = ""
          ),
          query = list(
            app_uid = getOption("polished")$app_uid,
            user_uid = session$userData$user()$user_uid,
            is_live = getOption("pp")$is_live
          )
        )

        sub_db <- jsonlite::fromJSON(
          httr::content(res, "text", encoding = "UTF-8")
        )

        if (!identical(httr::status_code(res), 200L)) {
          print(sub_db)
          stop("error getting subscription from Polished API", call. = FALSE)
        }

        sub_db <- as.list(sub_db)

        if (is.null(getOption("pp")$prices)) {
          session$userData$stripe(sub_db)
          return()
        }
        # user does not have a subscription, so set the user up with the
        # default subscription.


        if (identical(nrow(sub_db), 0L)) {
          stop("no subscription", call. = FALSE)

        } else if (is.null(getOption("pp")$prices)) {
          session$userData$stripe(list(
            stripe_customer_id = sub_db$stripe_customer_id,
            free_user = FALSE,
            default_payment_method = sub_db$defualt_payment_method,
            subscription = NA
          ))

          return()

        } else if (length(intersect(hold_user$roles, getOption("pp")$free_roles)) > 0) {

          session$userData$stripe(list(
            stripe_customer_id = sub_db$stripe_customer_id,
            free_user = TRUE,
            default_payment_method = sub_db$defualt_payment_method,
            subscription = NA
          ))

          return()

        } else {

          # if subscription is NA, that means the user has canceled their subscription, so redirect them to the
          # account page for them to restart their subscription
          if (is.na(sub_db$stripe_subscription_id)) {
            shiny::updateQueryString(
              queryString = "?page=account",
              session = session,
              mode = "replace"
            )
            session$reload()

          } else {
            stripe_sub <- get_stripe_subscription(sub_db$stripe_subscription_id)

            if (stripe_sub$trial_days_remaining > 0 || !is.na(stripe_sub$default_payment_method)) {

              # user is either in trial period or they already have a subscription with
              # billing enabled, so let them access the app.
              session$userData$stripe(list(
                stripe_customer_id = sub_db$stripe_customer_id,
                free_user = FALSE,
                default_payment_method = stripe_sub$default_payment_method,
                subscription = list(
                  uid = sub_db$uid,
                  stripe_subscription_id = sub_db$stripe_subscription_id,
                  item_id = stripe_sub$item_id,
                  plan_id = stripe_sub$plan_id,
                  nickname = stripe_sub$nickname,
                  amount = stripe_sub$amount,
                  interval = stripe_sub$interval,
                  trial_end = stripe_sub$trial_end,
                  is_billing_enabled = if (is.na(stripe_sub$default_payment_method)) FALSE else TRUE,
                  trial_days_remaining = stripe_sub$trial_days_remaining,
                  created_at = sub_db$created_at
                )
              ))

              return()
            } else {

              # user's free trial is over and they have not enabled billing, so redirect
              # to the account page for them to choose a subscription and enable billing.
              shiny::updateQueryString(
                queryString = "?page=account",
                session = session,
                mode = "replace"
              )
              session$reload()

            }
          }
        }

      }, error = function(err) {

        print(err)

        if (identical(err$message, "unable to create subscription") || identical(err$message, "subscription canceled")) {
          # you can't create a Stripe subscription until after the Stripe user
          # has entered their payment info.  If the stripe user needs to create
          # a subscription (i.e. )
          shiny::updateQueryString(
            queryString = "?page=account",
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

    callModule(
      app_module,
      "payments"
    )


    observeEvent(session$userData$stripe(), {

      server(input, output, session)

    }, once = TRUE)

  }
}
