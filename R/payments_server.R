


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

        sub_db <- tibble::as_tibble(sub_db)
        # user does not have a subscription, so set the user up with the
        # default subscription.
        if (identical(nrow(sub_db), 0L)) {

          # if the user has a role that allows them free access to the Shiny app or prices == NULL, then
          # let them access the app.
          if (length(intersect(hold_user$roles, getOption("pp")$free_roles)) > 0 || is.null(getOption("pp")$prices)) {
            session$userData$stripe(list(
              stripe_customer_id = NA,
              free_user = TRUE,
              default_payment_method = NA,
              subscription = NA
            ))

            return()
          }

          # set the user up with the default subscription
          # Step 1: create Stripe customer
          customer_id <- create_stripe_customer(
            email = hold_user$email,
            user_uid = hold_user$user_uid
          )

          if (getOption("pp")$trial_period_days > 0 && !is.null(getOption("pp")$prices)) {
            # Step 2: Create the Stripe subscription on Stripe
            stripe_subscription_id <- create_stripe_subscription(
              customer_id,
              plan_to_enable = getOption("pp")$prices[1],
              days_remaining = getOption("pp")$trial_period_days
            )
          } else {
            stripe_subscription_id <- NULL
          }

          # Step 3: add the newly created Stripe customer + subscription to the "subscriptions" table
          post_sub_res <- httr::POST(
            paste0(getOption("polished")$api_url, "/subscriptions"),
            body = list(
              "app_uid" = getOption("polished")$app_uid,
              "user_uid" = hold_user$user_uid,
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

          post_sub_res_content <- jsonlite::fromJSON(
            httr::content(post_sub_res, "text", encoding = "UTF-8")
          )

          if (!identical(httr::status_code(post_sub_res), 200L)) {
            print(post_sub_res_content)
            stop("Error saving subsciption to db", call. = FALSE)
          }

          # new subscription created, so reload the session to check the newly created subscription
          session$reload()
        } else {

          # if the user has a role that allows them free access to the Shiny app or prices == NULL, then
          # let them access the app.
          if (length(intersect(hold_user$roles, getOption("pp")$free_roles)) > 0 || is.null(getOption("pp")$prices)) {
            session$userData$stripe(list(
              stripe_customer_id = sub_db$stripe_customer_id,
              free_user = TRUE,
              default_payment_method = NA,
              subscription = NA
            ))

            return()
          }

          # if subscription is NA, that means the user has canceled their subscription, so redirect them to the
          # account page for them to restart their subscription
          if (is.na(sub_db$stripe_subscription_id) && !is.null(getOption("pp")$prices)) {
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


    observeEvent(session$userData$stripe(), {

      server(input, output, session)

    }, once = TRUE)

  }
}
