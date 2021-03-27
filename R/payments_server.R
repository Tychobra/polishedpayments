


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
#'
#' @export
#'
payments_server <- function(
  server
) {

  function(input, output, session) {


    session$userData$subscription <- reactiveVal(NULL)

    shiny::observeEvent(session$userData$user(), {

      hold_user <- session$userData$user()

      # if the user has a role that allows them free access to the Shiny app, then
      # let them access the app.
      if (length(intersect(hold_user$roles, getOption("pp")$free_roles)) > 0) {
        session$userData$subscription(list(
          free_user = TRUE
        ))

        return()
      }

      # otherwise start checking their subscription
      res <- httr::GET(
        paste0(getOption("polished")$api_url, "/subscriptions"),
        httr::authenticate(
          user = getOption("polished")$api_key,
          password = ""
        ),
        query = list(
          app_uid = getOption("polished")$app_uid,
          user_uid = session$userData$user()$user_uid
        )
      )

      res_content <- jsonlite::fromJSON(
        httr::content(res, "text", encoding = "UTF-8")
      )

      if (!identical(httr::status_code(res), 200L)) {
        print(res_content)
        stop(res_content, call. = FALSE)
      }

      # user does not have a subscription, so set up the user up with the
      # default subscription.
      if (length(res_content) == 0) {

        # set the user up with the default subscription
        # Step 1: create Stripe customer
        customer_id <- create_stripe_customer(
          email = hold_user$email,
          user_uid = hold_user$user_uid
        )

        # Step 2: Create the Stripe subscription on Stripe
        stripe_subscription_id <- create_stripe_subscription(
          customer_id,
          plan_to_enable = getOption("pp")$prices[[1]],
          days_remaining = getOption("pp")$trial_period_days
        )

        # Step 3: add the newly created Stripe customer + subscription to the "subscriptions" table
        res2 <- httr::POST(
          paste0(getOption("polished")$api_url, "/subscriptions"),
          body = list(
            "app_uid" = getOption("polished")$app_uid,
            "user_uid" = hold_user$user_uid,
            "stripe_customer_id" = customer_id,
            "stripe_subscription_id" = stripe_subscription_id
          ),
          encode = "json",
          httr::authenticate(
            user = getOption("polished")$api_key,
            password = ""
          )
        )

        res2_content <- jsonlite::fromJSON(
          httr::content(res, "text", encoding = "UTF-8")
        )

        if (!identical(httr::status_code(res2), 200L)) {
          print(res_content)
          stop("Error saving subsciption to db", call. = FALSE)
        }

        # new subscription created, so reload the session to check the newly created subscription
        session$reload()

      } else {

        # correct possible dropped subscription columns if the subscription exists.
        if (is.null(res_content$stripe_subscription_id)) res_content$stripe_subscription_id <- NA
        if (is.null(res_content$free_trial_days_remaining_at_cancel)) res_content$free_trial_days_remaining_at_cancel <- NA

        # if subscription in NA, that means the user has canceled their subscription, so redirect them to the
        # account page for them to restart their subscription
        if (is.na(res_content$stripe_subscription_id)) {
          shiny::updateQueryString(
            queryString = "?page=account",
            session = session,
            mode = "replace"
          )
          session$reload()
        } else {
          stripe_sub <- get_stripe_subscription(res_content$stripe_subscription_id)

          if (stripe_sub$trial_days_remaining > 0 || !is.na(stripe_sub$default_payment_method)) {
            # user is either in trial period or they already have a subscription with
            # billing enabled, so let them access the app.


            session$userData$subscription(list(
              free_user = FALSE,
              price_id = stripe_sub$plan_id,
              nickname = stripe_sub$nickname,
              is_billing_enabled = if (is.na(stripe_sub$default_payment_method)) FALSE else TRUE,
              trial_days_remaining = stripe_sub$trial_days_remaining
            ))


            return()
          } else {

            # user's free trial is over and they have not enabled billing, so redirect
            # to the account page for them to choose a subacription and enable billing.
            shiny::updateQueryString(
              queryString = "?page=account",
              session = session,
              mode = "replace"
            )
            session$reload()

          }
        }
      }

    })


    observeEvent(session$userData$subscription, {

      server(input, output, session)

    }, once = TRUE)


  }

}
