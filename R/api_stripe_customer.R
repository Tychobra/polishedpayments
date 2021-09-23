

get_stripe <- function(
  user_uid,
  user_roles,
  is_on_payments = FALSE,
  app_uid = getOption("polished")$app_uid,
  stripe_prices = getOption("pp")$prices,
  free_roles = getOption("pp")$free_roles
) {
  customer_res <- get_customers(
    app_uid = app_uid,
    user_uid = user_uid
  )

  if (!identical(httr::status_code(customer_res$response), 200L)) {
    print(customer_res$content)
    stop("error getting subscription from Polished API", call. = FALSE)
  }

  customer <- as.list(customer_res$content)



  if (identical(nrow(customer), 0L)) {
    stop("no customer", call. = FALSE)

  } else if (is.null(stripe_prices)) {
    # app is using single payments only (i.e. no subscriptions)

    out <- list(
      polished_customer_uid = customer$uid,
      stripe_customer_id = customer$stripe_customer_id,
      free_user = FALSE,
      default_payment_method = customer$default_payment_method,
      trial_days_remaining = customer$free_trial_days_remaining_at_cancel,
      subscription = NA
    )

  } else if (length(intersect(user_roles, free_roles)) > 0) {
    # app is using subscriptions, but user is allowed to access the app without a subscription
    # because they are a "free_user"
    out <- list(
      polished_customer_uid = customer$uid,
      stripe_customer_id = customer$stripe_customer_id,
      free_user = TRUE,
      default_payment_method = customer$default_payment_method,
      trial_days_remaining = customer$free_trial_days_remaining_at_cancel,
      subscription = NA
    )

  } else {

    out <- list(
      polished_customer_uid = customer$uid,
      stripe_customer_id = customer$stripe_customer_id,
      free_user = FALSE,
      default_payment_method = customer$default_payment_method,
      trial_days_remaining = customer$free_trial_days_remaining_at_cancel
    )

    if (is.na(customer$stripe_subscription_id)) {
      # customer does not have a subscription or trial and one is required.  Since user is on app, redirect to payments.
      if (isFALSE(is_on_payments)) {

        out <- NULL

      } else {
        stripe_out$subscription <- NA
      }


    } else {
      stripe_sub <- get_stripe_subscription(customer$stripe_subscription_id)

      # if subscription is NA, that means the user has canceled their subscription, so redirect them to the
      # account page for them to restart their subscription

      if ((stripe_sub$trial_days_remaining <= 0 && is.na(customer$default_payment_method)) && isFALSE(is_on_payments)) {

        print("subscription has been cancelled")
        out <- NULL
      } else {

        out$trial_days_remaining <- stripe_sub$trial_days_remaining

        out$subscription = list(
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
  }

  return(out)
}


#' Create a new Stripe customer
#'
#' @param email The new Stripe customer's email address.
#' @param user_uid the new Stripe customer's polished user uid.
#'
#' @export
#'
#'
add_stripe_customer <- function(email, user_uid) {

  res <- httr::POST(
    "https://api.stripe.com/v1/customers",
    body = list(
      "email" = email,
      "metadata[polished_uid]" = user_uid
    ),
    encode = "form",
    httr::authenticate(
      user = getOption("pp")$keys$secret,
      password = ""
    )
  )

  res_data <- jsonlite::fromJSON(
    httr::content(res, "text", encoding = "UTF-8")
  )

  if (!identical(httr::status_code(res), 200L)) {
    print(res_data)
    stop("error creating Stripe user", call. = FALSE)
  }


  customer_id <- res_data$id
  if (is.null(customer_id)) {
    stop("no customer id received from Stripe")
  }

  customer_id
}



#' Collect information for a Stripe customer
#'
#' @param stripe_customer_id The Stripe customer's ID.
#'
#' @export
#'
#'
get_stripe_customer <- function(stripe_customer_id) {

  res <- httr::GET(
    "https://api.stripe.com/v1/customers",
    query = list(
      "customer_id" = customer_id
    ),
    httr::authenticate(
      user = getOption("pp")$keys$secret,
      password = ""
    )
  )

  res_data <- jsonlite::fromJSON(
    httr::content(res, "text", encoding = "UTF-8")
  )

  if (!identical(httr::status_code(res), 200L)) {
    print(res_data)
    stop("error creating Stripe user", call. = FALSE)
  }


  customer_id <- res_data$id
  if (is.null(customer_id)) {
    stop("no customer id received from Stripe")
  }

  res_data
}


