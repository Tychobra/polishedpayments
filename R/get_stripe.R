#' Get relevant information about a Stripe customer from Stripe and Polished
#'
#' This is a helper API wrapper for internal use in Polished Payments Shiny apps.
#' It is not intended to be used outside of Shiny.
#'
#' @param user_uid the customer's `polished` user uid
#' @param app_uid the `polished` app uid; defaults to the current app
#' (e.g. \code{polished::.polished$app_uid})
#'
#' @return a list with the following elements:
#' - polished_customer_uid
#' - stripe_customer_id
#' - default_payment_method
#' - trial_days_remaining
#' - subscription: either NA if no subscription in Stripe, or a list of with the following
#' elements retrieved from the Stripe API:
#'   - stripe_subscription_id
#'   - item_id
#'   - plan_id
#'   - nickname
#'   - amount
#'   - interval
#'   - trial_end
#'   - created_at
#'
#' @importFrom httr status_code
#' @importFrom polished .polished
#'
get_stripe <- function(
  user_uid,
  app_uid = polished::.polished$app_uid
) {

  if (!is.character(app_uid) && length(app_uid) != 1L) {
    stop("`app_uid` must be a length 1 character vector", call. = FALSE)
  }

  customer_res <- get_customers(
    app_uid = app_uid,
    user_uid = user_uid
  )

  if (!identical(httr::status_code(customer_res$response), 200L)) {
    print(customer_res$content)
    stop("error getting subscription from Polished API", call. = FALSE)
  }


  if (identical(nrow(customer_res$content), 0L)) {
    return(NULL)
  }

  customer <- as.list(customer_res$content)


  out <- list(
    polished_customer_uid = customer$uid,
    stripe_customer_id = customer$stripe_customer_id,
    default_payment_method = customer$default_payment_method,
    trial_days_remaining = customer$free_trial_days_remaining_at_cancel
  )

  if (is.na(customer$stripe_subscription_id[1])) {
    out$subscription <- NA
  } else {

    stripe_sub <- get_stripe_subscription(customer$stripe_subscription_id)

    # if subscription is NA, that means the user has canceled their subscription, so redirect them to the
    # account page for them to restart their subscription
    if (is.na(stripe_sub[1])) {
      out$subscription <- NA
    } else {

      out$trial_days_remaining <- stripe_sub$trial_days_remaining

      out$subscription = list(
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

  return(out)
}