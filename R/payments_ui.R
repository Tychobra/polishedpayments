#' adds polished payments to your Shiny UI
#'
#'
#' @export
payments_ui <- function(
  ui
) {

  function(request) {

    if (is.function(ui)) {
      ui <- ui(request)
    }
    ui <- force(ui)


    print(list(
      ui_user = request$user
    ))
    # TODO: look up the user's subscription here. movae as much logic from payments_server to
    # here as possible.
    user <- request$polished_user

    # get any existing subscriptions from Polished API
    res <- httr::GET(
      paste0(getOption("polished")$api_url, "/subscriptions"),
      httr::authenticate(
        user = getOption("polished")$api_key,
        password = ""
      ),
      query = list(
        app_uid = getOption("polished")$app_uid,
        user_uid = user$user_uid,
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

    if (identical(nrow(sub_db), 0L)) {

      # set the user up with the default subscription
      # Step 1: create Stripe customer
      customer_id <- create_stripe_customer(
        email = user$email,
        user_uid = user$user_uid
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
          "user_uid" = user$user_uid,
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

    }

    # if (is.na(sub_db$stripe_subscription_id) && !is.null(getOption("pp")$prices)) {
    #   shiny::updateQueryString(
    #     queryString = "?page=account",
    #     session = session,
    #     mode = "replace"
    #   )
    #   session$reload()
    # }


    if (TRUE) {
      out <- app_module_ui("payments")
    } else {
      out <- shiny::tagList(
        shiny::tags$head(
          tags$script(src = "https://js.stripe.com/v3"),
          tags$script(paste0("var stripe = Stripe('", getOption("pp")$keys$public, "');"))
        ),
        ui
      )
    }

    out
  }
}
